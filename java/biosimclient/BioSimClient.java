/*
 * This file is part of the biosimclient library
 *
 * Author Mathieu Fortin - Canadian Forest Service
 * Copyright (C) 2020 Her Majesty the Queen in right of Canada
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package biosimclient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import biosimclient.BioSimEnums.ClimateModel;
import biosimclient.BioSimEnums.Month;
import biosimclient.BioSimEnums.Period;
import biosimclient.BioSimEnums.RCP;
import biosimclient.BioSimEnums.Variable;

/**
 * This class enables a client for the Biosim server at repicea.dyndns.org.
 * 
 * @author Mathieu Fortin - October 2019
 */
public final class BioSimClient {

	private static int MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION = -1; // not set yet
	private static int MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS = -1; // not set yet
	private static final int MAXIMUM_NB_LOCATIONS_PER_BATCH_REMOVALS = 200;
	private static int MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST = -1; // not set yet
	
	static final String FieldSeparator = ",";
	
	private static final InetSocketAddress REpiceaAddress = new InetSocketAddress("repicea.dynu.net", 80);
	private static final InetSocketAddress LocalAddress = new InetSocketAddress("192.168.0.194", 5000);
	
	private static final String SPACE_IN_REQUEST = "%20";

	static final List<Month> AllMonths = Arrays.asList(Month.values());

	private static final String NORMAL_API = "BioSimNormals";
	private static final String GENERATOR_API = "BioSimWG";
	private static final String EPHEMERAL_API = "BioSimWGEphemeralMode";
	private static final String MODEL_API = "BioSimModel";
	private static final String MODEL_LIST_API = "BioSimModelList";
	private static final String BIOSIMCLEANUP_API = "BioSimMemoryCleanUp";
	private static final String BIOSIMMEMORYLOAD_API = "BioSimMemoryLoad";
	private static final String BIOSIMMAXMEMORY_API = "BioSimMaxMemory";
	private static final String BIOSIMMAXCOORDINATES = "BioSimMaxCoordinatesPerRequest";
	private static final String BIOSIMMODELHELP = "BioSimModelHelp";
	private static final String BIOSIMMODELDEFAULTPARAMETERS = "BioSimModelDefaultParameters";

	protected static final BioSimGeneratedClimateMap GeneratedClimateMap = new BioSimGeneratedClimateMap();

//	private static Integer BioSimMaxMemory;
	
	private static List<String> ReferenceModelList;


	static class InternalShutDownHook extends Thread {
		@Override
		public void run() {
			try {
				if (!GeneratedClimateMap.isEmpty()) {
					System.out.println("Shutdown hook from BioSimClient called!");
					BioSimClient.clearCache();
				}
			} catch (BioSimClientException e) {
				e.printStackTrace();
			} catch (BioSimServerException e2) {
				e2.printStackTrace();
			}
		}
	}
	
	static {
		Runtime.getRuntime().addShutdownHook(new InternalShutDownHook());
	}

	static boolean isLocal = false;

	static boolean ForceClimateGenerationEnabled = false;  // default value
	
	static Integer NbNearestNeighbours = null;
	
	private static String addQueryIfAny(String urlString, String query) {
		if (query != null && !query.isEmpty()) {
			return urlString.trim() + "?" + query;
		} else {
			return urlString;
		}
	}

	private static synchronized String getStringFromConnection(String api, String query) throws BioSimClientException, BioSimServerException {
		InetSocketAddress address;
		if (isLocal) {
			address = BioSimClient.LocalAddress;
		} else {
			address = BioSimClient.REpiceaAddress;
		}
		String urlString = "http://" + address.getHostName() + ":" + address.getPort() + "/" + api;
		urlString = addQueryIfAny(urlString, query);
		try {
			URL bioSimURL = new URL(urlString);
			HttpURLConnection connection = (HttpURLConnection) bioSimURL.openConnection();
			int code = connection.getResponseCode();
			if (code < 200 || code > 202) { // if true that means it is not connected
				throw new BioSimClientException("Unable to connect to BioSIM server! Please check your connection or contact your network administrator.");
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
			String completeString = "";
			String lineStr;
			int line = 0;
			while ((lineStr = br.readLine()) != null) {
				if (line == 0) {
					completeString += lineStr;

				} else {
					completeString += "\n" + lineStr;
				}
				line++;
			}
			br.close();
			if (completeString.startsWith("Exception")) {
				throw new BioSimServerException(completeString);
			}
			return completeString;
		} catch (MalformedURLException e) {
			throw new BioSimClientException(e.getMessage());
		} catch (IOException e) {
			throw new BioSimClientException(e.getMessage());
		}
	}



	
	private static LinkedHashMap<BioSimPlot, BioSimDataSet> internalCalculationForNormals(Period period,
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel,
			List<Month> averageOverTheseMonths) throws BioSimClientException, BioSimServerException {
		LinkedHashMap<BioSimPlot, BioSimDataSet> outputMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();

		String query = constructCoordinatesQuery(locations);
		query += "&" + period.parsedQuery;

		if (rcp != null) {
			query += "&rcp=" + rcp.getURLString();
		}
		
		if(climModel != null) {
			query += "&climMod=" + climModel.name();
		}
		
		String serverReply = BioSimClient.getStringFromConnection(NORMAL_API, query);
		
		readLines(serverReply, "month", locations, outputMap);

		if (averageOverTheseMonths == null || averageOverTheseMonths.isEmpty()) {
			List<Integer> fieldsToBeRemoved = null;
			for (BioSimDataSet bioSimDataSet : outputMap.values()) {
				if (fieldsToBeRemoved == null) {
					fieldsToBeRemoved = new ArrayList<Integer>();
					for (int i = bioSimDataSet.getFieldNames().size() - 1; i > 0; i--) {	// reverse order
						if (!Variable.getFieldNamesForNormals().contains(bioSimDataSet.getFieldNames().get(i))) {
							fieldsToBeRemoved.add(i);
						}
					}
				}
				for (Integer fieldId : fieldsToBeRemoved) {
					bioSimDataSet.removeField(fieldId);
				}
			}
			return outputMap;
		} else {
			LinkedHashMap<BioSimPlot, BioSimDataSet> formattedOutputMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
			for (BioSimPlot location : outputMap.keySet()) {
				BioSimDataSet ds = outputMap.get(location);
				BioSimMonthMap bsmm = new BioSimMonthMap(ds);
				formattedOutputMap.put(location, bsmm.getMeanForTheseMonths(averageOverTheseMonths));
			}
			return formattedOutputMap;
		}
	}

	private static int getMaxNumberLocationsInSingleRequest() {
		if (MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST == -1) { // true when called for the first time
			try {
				MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST = (int) (BioSimClient.getMaxNbWgoutObjectsOnServer() * .05);
			} catch (Exception e) {
				MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST = 1000;
			}
		}
		return MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST;
	}

	/**
	 * This method clears the reference to the teleIO objects that are stored in the internal map.
	 * @throws BioSimClientException
	 * @throws BioSimServerException
	 */
	public static void clearCache() throws BioSimClientException, BioSimServerException {
		if (!GeneratedClimateMap.isEmpty()) {
			BioSimClient.removeWgoutObjectsFromServer(GeneratedClimateMap.values());
		}
	}
	
	/**
	 * Retrieves the normals and compiles the mean or sum over some months.
	 * @param period a Period enum variable
	 * @param locations a List of BioSimPlot instances
	 * @param rcp an RCP enum variable (if null the server takes the RCP 4.5 by default 
	 * @param climModel a ClimateModel enum variable (if null the server takes the RCM4 climate model
	 * @param averageOverTheseMonths the months over which the mean or sum is to be
	 *                               calculated. If empty or null the method returns
	 *                               the monthly averages.
	 * @return a Map with the BioSimPlot instances as keys and BioSimDataSet instances as values.
	 * @throws BioSimClientException if the client fails or a BioSimServerException if the server fails 
	 */
	public static LinkedHashMap<BioSimPlot, BioSimDataSet> getNormals(
			Period period,
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel,
			List<Month> averageOverTheseMonths) throws BioSimClientException, BioSimServerException {
		if (locations.size() > BioSimClient.getMaxNumberLocationsInSingleRequest()) {
			throw new BioSimClientException("The maximum number of locations for a single request is " + MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST);
		}
		if (locations.size() > BioSimClient.getMaximumNbLocationsPerBatchNormals()) {
			LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
			List<BioSimPlot> copyList = new ArrayList<BioSimPlot>();
			copyList.addAll(locations);
			List<BioSimPlot> subList = new ArrayList<BioSimPlot>();
			while (!copyList.isEmpty()) {
				while (!copyList.isEmpty() && subList.size() < BioSimClient.getMaximumNbLocationsPerBatchNormals()) {
					subList.add(copyList.remove(0));
				}
				resultingMap.putAll(internalCalculationForNormals(period, subList, rcp, climModel, averageOverTheseMonths));
				subList.clear();
			}
			return resultingMap;
		} else {
			return internalCalculationForNormals(period, locations, rcp, climModel, averageOverTheseMonths);
		}
	}

	static void removeWgoutObjectsFromServer(Collection<String> references) 
			throws BioSimClientException, BioSimServerException {
		if (references.size() > MAXIMUM_NB_LOCATIONS_PER_BATCH_REMOVALS) {
			List<String> referenceList = new ArrayList<String>();
			referenceList.addAll(references);
			List<String> subList = new ArrayList<String>();
			while (!referenceList.isEmpty()) {
				while (!referenceList.isEmpty() && subList.size() < MAXIMUM_NB_LOCATIONS_PER_BATCH_REMOVALS) {
					subList.add(referenceList.remove(0));
				}
				internalRemovalOfWgoutObjectsFromServer(subList);
				subList.clear();
			}
		} else {
			internalRemovalOfWgoutObjectsFromServer(references);
		}
	}

	private static void internalRemovalOfWgoutObjectsFromServer(Collection<String> references) 
			throws BioSimClientException, BioSimServerException {
		if (references != null && !references.isEmpty()) {
			String query = "";
			for (String reference : references) {
				if (query.isEmpty()) {
					query += reference;
				} else {
					query += SPACE_IN_REQUEST + reference;
				}
			}
			getStringFromConnection(BIOSIMCLEANUP_API, "ref=" + query);
			for (String reference : references) {
				GeneratedClimateMap.removeValue(reference);
			}
		}
	}

	static int getNbWgoutObjectsOnServer() throws Exception {
		String serverReply = getStringFromConnection(BIOSIMMEMORYLOAD_API, null);
		try {
			return Integer.parseInt(serverReply);
		} catch (NumberFormatException e) {
			throw new BioSimClientException("The server reply could not be parsed: " + e.getMessage());
		}
	}

	/**
	 * The maximum number of wgouts instances that can be stored in the internal map of the server.
	 * @return
	 */
	private static int getMaxNbWgoutObjectsOnServer() throws Exception {
		String serverReply = getStringFromConnection(BIOSIMMAXMEMORY_API, null);
		try {
			return Integer.parseInt(serverReply);
		} catch (NumberFormatException e) {
			throw new BioSimClientException("The server reply could not be parsed: " + e.getMessage());
		}
	}

	/**
	 * Retrieves the monthly normals.
	 * @param period a Period enum variable
	 * @param locations a List of BioSimPlot instances
	 * @param rcp an RCP enum variable (if null the server takes the RCP 4.5 by default 
	 * @param climModel a ClimateModel enum variable (if null the server takes the RCM4 climate model
	 * @return a Map with the BioSimPlot instances as keys and BioSimDataSet instances as values.
	 * @throws BioSimClientException if the client fails or a BioSimServerException if the server fails 
	 */
	public static Map<BioSimPlot, BioSimDataSet> getMonthlyNormals(
			Period period, 
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel) throws BioSimClientException, BioSimServerException {
		return getNormals(period, locations, rcp, climModel, null);
	}

	/**
	 * Retrieves the yearly normals.
	 * @param period a Period enum variable
	 * @param locations a List of BioSimPlot instances
	 * @param rcp an RCP enum variable (if null the server takes the RCP 4.5 by default 
	 * @param climModel a ClimateModel enum variable (if null the server takes the RCM4 climate model
	 * @return a Map with the BioSimPlot instances as keys and BioSimDataSet instances as values.
	 * @throws BioSimClientException if the client fails or a BioSimServerException if the server fails 
	 */
	public static Map<BioSimPlot, BioSimDataSet> getAnnualNormals(
			Period period, 
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel) throws BioSimClientException, BioSimServerException {
		return getNormals(period, locations, rcp, climModel, AllMonths);
	}

	private static String constructCoordinatesQuery(List<BioSimPlot> locations) {
		String latStr = "";
		String longStr = "";
		String elevStr = "";
		for (BioSimPlot location : locations) {
			if (latStr.isEmpty()) {
				latStr += location.getLatitudeDeg();
			} else {
				latStr += SPACE_IN_REQUEST + location.getLatitudeDeg();
			}
			if (longStr.isEmpty()) {
				longStr += location.getLongitudeDeg();
			} else {
				longStr += SPACE_IN_REQUEST + location.getLongitudeDeg();
			}
			if (elevStr.isEmpty()) {
				elevStr += processElevationM(location);
			} else {
				elevStr += SPACE_IN_REQUEST + processElevationM(location);
			}
		}

		String query = "";
		query += "lat=" + latStr;
		query += "&long=" + longStr;
		if (!elevStr.isEmpty()) {
			query += "&elev=" + elevStr;
		}
		return query;
	}

	private static String processElevationM(BioSimPlot location) {
		if (Double.isNaN(location.getElevationM())) {
			return "NaN";
		} else {
			return "" + location.getElevationM();
		}
	}
	
//	private static String getVariablesQuery(List<Variable> variables) {		
//		String variablesQuery = "";
////		List<Variable> variables = Arrays.asList(Variable.values());
//		for (Variable v : variables) {
//			variablesQuery += v.name();
//			if (variables.indexOf(v) < variables.size() - 1) {
//				variablesQuery += SPACE_IN_REQUEST;
//			}
//		}
//		return variablesQuery;
//	}

	/**
	 * Generate climate variable and apply a particular model on these generated variables.
	 * 
	 * This method is based on the ephemeral mode. The generated climate variables are not 
	 * cached.
	 * 
	 * @param modelName  the name of the model
	 * @param teleIORefs a LinkedHashMap with the references to the TeleIO objects on the server
	 * @param additionalParms a BioSimParameterMap instance that contains the eventual additional parameters for the model
	 * @return a LinkedHashMap with BioSimPlot instances as keys and a Map with years and climate variables values as values.
	 * @throws BioSimClientException
	 */
	static LinkedHashMap<BioSimPlot, BioSimDataSet> applyModel(int fromYr, 
			int toYr,
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel,
			int rep,
			String modelName,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
//		if (!getReferenceModelList().contains(modelName)) {
//			throw new InvalidParameterException("The model " + modelName
//					+ " is not a valid model. Please consult the list of models through the function getModelList()");
//		}

//		LinkedHashMap<BioSimPlot, String> outputMap = new LinkedHashMap<BioSimPlot, String>();
		
		String query = constructCoordinatesQuery(locations);
		query += "&from=" + fromYr;
		query += "&to=" + toYr;
		if (rcp != null) {
			query += "&rcp=" + rcp.getURLString();
		}
		
		if(climModel != null) {
			query += "&climMod=" + climModel.name();
		}
		
		if (ForceClimateGenerationEnabled) {
			System.out.println("Warning: past climate is generated instead of being compiled from observations!");
			query += "&source=FromNormals";
		}
		
		if (NbNearestNeighbours != null) {
			query += "&nb_nearest_neighbor=" + NbNearestNeighbours.toString();
		}
		
		if (rep > 1) {
			query += "&rep=" + rep;
		}
		
		query += "&model=" + modelName;
		if (additionalParms != null) {
			query += "&" + additionalParms.parse();
		}
		
		String serverReply = getStringFromConnection(EPHEMERAL_API, query);
		LinkedHashMap<BioSimPlot, BioSimDataSet> outputMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
		readLines(serverReply, "rep", locations, outputMap);
		return outputMap;
	}


	
	/**
	 * Generates climate for some locations over a particular time interval.
	 * 
	 * @param fromYr    beginning of the interval (inclusive)
	 * @param toYr      end of the interval (inclusive)
	 * @param locations a List of BioSimPlot instances
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climModel a ClimateModel enum variable (by default RCM 4)
	 * @return a LinkedHashMap with BioSimPlot instances as key
	 *         and String instances as values. Those strings are actually the code
	 *         for the TeleIO instance on the server.
	 * @throws BioSimClientException
	 */
	static LinkedHashMap<BioSimPlot, String> getGeneratedClimate(
			int fromYr, 
			int toYr,
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel,
			int rep) throws BioSimClientException, BioSimServerException {
//		boolean compress = false; // disabling compression by default
		LinkedHashMap<BioSimPlot, String> outputMap = new LinkedHashMap<BioSimPlot, String>();

//		String variablesQuery = getVariablesQuery(Arrays.asList(Variable.values()));
		
		String query = constructCoordinatesQuery(locations);
//		query += "&var=" + variablesQuery;
//		if (compress) {
//			query += "&compress=1";
//		} else {
//			query += "&compress=0";
//		}
		query += "&from=" + fromYr;
		query += "&to=" + toYr;
		if (rcp != null) {
			query += "&rcp=" + rcp.getURLString();
		}
		
		if(climModel != null) {
			query += "&climMod=" + climModel.name();
		}
		
		if (ForceClimateGenerationEnabled) {
			System.out.println("Warning: past climate is going to be generated instead of being compiled from observations!");
			query += "&source=FromNormals";
		}
		
		if (NbNearestNeighbours != null) {
			query += "&nb_nearest_neighbor=" + NbNearestNeighbours.toString();
		}
		
		if (rep > 1) {
			query += "&rep=" + rep;
		}
		
//		System.out.println("Sending request!");
		String serverReply = getStringFromConnection(GENERATOR_API, query);

		String[] ids = serverReply.split(" ");
		if (ids.length != locations.size()) {
			throw new BioSimClientException("The number of wgout ids is different from the number of locations!");
		}
		for (int i = 0; i < locations.size(); i++) {
			String id = ids[i];
			BioSimPlot location = locations.get(i);
			if (id.toLowerCase().startsWith("error")) {
				throw new BioSimClientException("The server was unable to generate the climate for this location: "
						+ location.toString() + ": " + id);
			}
			outputMap.put(location, id);
		}
		return outputMap;
	}

	/**
	 * Returns the names of the available models. This is a clone of the
	 * true list to avoid any intended changes in the model list.
	 * 
	 * @return a List of String instances
	 */
	public static List<String> getModelList() throws BioSimClientException, BioSimServerException {
		List<String> copy = new ArrayList<String>();
		copy.addAll(getReferenceModelList());
		return copy;
	}
	
	public static String getModelHelp(String modelName) throws BioSimClientException, BioSimServerException {
		if (modelName == null) {
			throw new InvalidParameterException("THe modelName parameter cannot be set to null!");
		}
		String serverReply = getStringFromConnection(BIOSIMMODELHELP, "model=" + modelName);
		return serverReply;
	}

	
	public static BioSimParameterMap getModelDefaultParameters(String modelName) throws BioSimClientException, BioSimServerException {
		if (modelName == null) {
			throw new InvalidParameterException("THe modelName parameter cannot be set to null!");
		}
		String serverReply = getStringFromConnection(BIOSIMMODELDEFAULTPARAMETERS, "model=" + modelName);
		String[] parms = serverReply.split(FieldSeparator);
		BioSimParameterMap parmMap = new BioSimParameterMap();
		for (String parm : parms) {
			String[] keyValue = parm.split(":");
			if (keyValue.length > 1) {
				parmMap.put(keyValue[0], keyValue[1]);
			} else {
				parmMap.put(keyValue[0], null);
			}
		}
		return parmMap;
	}

	private static List<String> getReferenceModelList() throws BioSimClientException, BioSimServerException {
		if (ReferenceModelList == null) {
			List<String> myList = new ArrayList<String>();
			String modelList = BioSimClient.getStringFromConnection(BioSimClient.MODEL_LIST_API, null);
			String[] models = modelList.split("\n");
			for (String model : models) {
				myList.add(model);
			}
			ReferenceModelList = new ArrayList<String>();
			ReferenceModelList.addAll(myList);
		}
		return ReferenceModelList;
	}
 	
	
	/**
	 * Applies a particular model on some generated climate variables.
	 * 
	 * @param modelName  the name of the model
	 * @param teleIORefs a LinkedHashMap with the references to the TeleIO objects on the server
	 * @param additionalParms a BioSimParameterMap instance that contains the eventual additional parameters for the model
	 * @return a LinkedHashMap with BioSimPlot instances as keys and a Map with years and climate variables values as values.
	 * @throws BioSimClientException
	 */
	static LinkedHashMap<BioSimPlot, BioSimDataSet> applyModel(
			String modelName,
			LinkedHashMap<BioSimPlot, String> teleIORefs,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
		if (!getReferenceModelList().contains(modelName)) {
			throw new InvalidParameterException("The model " + modelName
					+ " is not a valid model. Please consult the list of models through the function getModelList()");
		}
//		boolean compress = false; // disabling compression

		String wgoutQuery = "";
		List<BioSimPlot> refListForLocations = new ArrayList<BioSimPlot>();
		for (BioSimPlot location : teleIORefs.keySet()) {
			refListForLocations.add(location);
			if (wgoutQuery.isEmpty()) {
				wgoutQuery += teleIORefs.get(location);
			} else {
				wgoutQuery += SPACE_IN_REQUEST + teleIORefs.get(location);
			}
		}

		LinkedHashMap<BioSimPlot, BioSimDataSet> outputMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
		String query = "";
		query += "model=" + modelName;
//		if (compress) {
//			query += "&compress=1";
//		} else {
//			query += "&compress=0";
//		}
		query += "&wgout=" + wgoutQuery;
		if (additionalParms != null) {
			query += "&" + additionalParms.parse();
		}

		String serverReply = getStringFromConnection(MODEL_API, query);
		
		readLines(serverReply, "rep", refListForLocations, outputMap);
		
		return outputMap;
	}

	
	private static void readLines(String serverReply,
			String fieldLineStarter,
			List<BioSimPlot> refListForLocations,
			LinkedHashMap<BioSimPlot, BioSimDataSet> outputMap) throws BioSimClientException, BioSimServerException {
		String[] lines = serverReply.split("\n");
		BioSimDataSet dataSet = null;
		int locationId = 0;
		BioSimPlot location = null;
		boolean properlyInitialized = false;
		for (String line : lines) {
			if (line.toLowerCase().startsWith("error")) {
				throw new BioSimServerException(line);
			} else if (line.toLowerCase().startsWith(fieldLineStarter)) { // means it is a new location
				if (dataSet != null) {	// must be indexed before instantiating a new DataSet
					dataSet.indexFieldType();
				}
				location = refListForLocations.get(locationId);
				String[] fields = line.split(FieldSeparator);
				List<String> fieldNames = Arrays.asList(fields);
				dataSet = new BioSimDataSet(fieldNames);
				outputMap.put(location, dataSet);
				locationId++;
				properlyInitialized = true;
			} else {
				if (!properlyInitialized) {
					throw new BioSimClientException(serverReply);
				} else {
					Object[] fields = Arrays.asList(line.split(FieldSeparator)).toArray(new Object[]{});
					dataSet.addObservation(fields);
				}
			}
		}
		if (dataSet != null) {
			dataSet.indexFieldType();	// last DataSet has not been instantiated so it needs to be here.
		}
	}
	
	private static LinkedHashMap<BioSimPlot, BioSimDataSet> internalCalculationForClimateVariables(
			int fromYr, 
			int toYr, 
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climMod,
			String modelName, 
			int rep,
			boolean isEphemeral,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
		Map<BioSimPlot, String> alreadyGeneratedClimate = new HashMap<BioSimPlot, String>();
		List<BioSimPlot> locationsToGenerate = new ArrayList<BioSimPlot>();
		
		if (isEphemeral) {
			locationsToGenerate.addAll(locations);
			return BioSimClient.applyModel(fromYr, toYr, locationsToGenerate, rcp, climMod, rep, modelName, additionalParms);
		} else { // here we retrieve what is already available
			for (BioSimPlot location : locations) {
				BioSimQuerySignature querySignature = new BioSimQuerySignature(fromYr, toYr, location, rcp, climMod, rep, ForceClimateGenerationEnabled);
				if (GeneratedClimateMap.containsKey(querySignature)) {
					alreadyGeneratedClimate.put(location, GeneratedClimateMap.get(querySignature));
				} else {
					locationsToGenerate.add(location);
				}
			}
			
			Map<BioSimPlot, String> generatedClimate = new HashMap<BioSimPlot, String>();
			if (!locationsToGenerate.isEmpty()) { // here we generate the climate if needed
				generatedClimate.putAll(BioSimClient.getGeneratedClimate(fromYr, toYr, locationsToGenerate, rcp, climMod, rep));
				
				for (BioSimPlot location : generatedClimate.keySet()) {
					// TODO FP we could avoid creating again a signature here by storing the signature in a map
					GeneratedClimateMap.put(new BioSimQuerySignature(fromYr, toYr, location, rcp, climMod, rep, ForceClimateGenerationEnabled),		
							generatedClimate.get(location));
				}
			}

			generatedClimate.putAll(alreadyGeneratedClimate);

			LinkedHashMap<BioSimPlot, String> mapForModels = new LinkedHashMap<BioSimPlot, String>();
			for (BioSimPlot location : locations) {
				mapForModels.put(location, generatedClimate.get(location));
			}
			LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = BioSimClient.applyModel(modelName, mapForModels, additionalParms);
			return resultingMap;
		}

	}

	/**
	 * Returns a model output for a particular time interval. The method is based on 
	 * the ephemeral mode and consequently, the generated climate variables are discarded
	 * after this method has been run. 
	 * 
	 * The "modelname" argument sets the model to be applied on
	 * the generated climate variables. It should be one of the strings returned by the 
	 * getModelList static method. Generating the climate is time consuming. The 
	 * generated climate is stored on the server and it can be re used with some 
	 * other models. 
	 * 
	 * @param fromYr starting date (yr) of the period (inclusive)
	 * @param toYr ending date (yr) of the period (inclusive)
	 * @param locations the locations of the plots (BioSimPlot instances)
	 * @param modelName a string representing the model name
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climMod a ClimateModel enum variable (by default RCM 4)
	 * @param rep the number of replicates if needed. Should be equal to or greater than 1. 
	 * @param additionalParms a BioSimParameterMap instance that contains the eventual additional parameters for the model
	 * @return a LinkedHashMap of BioSimPlot instances (keys) and climate variables (values)
	 * @throws BioSimClientException if the client fails or BioSimServerException if the server fails
	 */
	public static LinkedHashMap<BioSimPlot, BioSimDataSet> getModelOutput(int fromYr, 
			int toYr,
			List<BioSimPlot> locations, 
			RCP rcp,
			ClimateModel climMod,
			String modelName,
			int rep,
			BioSimParameterMap additionalParms)	throws BioSimClientException, BioSimServerException {
		return BioSimClient.getModelOutput(fromYr, toYr, locations, rcp, climMod, modelName, rep, true, additionalParms); // ephemeral mode enabled
	}

	
	/**
	 * Returns a model output for a particular time interval. The method is based on 
	 * the ephemeral mode and consequently, the generated climate variables are discarded
	 * after this method has been run. 
	 * 
	 * The "modelname" parameter sets the model to be applied on
	 * the generated climate. It should be one of the strings returned by the 
	 * getModelList static method. Generating the climate is time consuming. The 
	 * generated climate is stored on the server and it can be re used with some 
	 * other models. The number of replicate is set to 1.
	 * 
	 * @param fromYr starting date (yr) of the period (inclusive)
	 * @param toYr ending date (yr) of the period (inclusive)
	 * @param locations the locations of the plots (BioSimPlot instances)
	 * @param modelName a string representing the model name
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climMod a ClimateModel enum variable (by default RCM 4)
	 * @param additionalParms a BioSimParameterMap instance that contains the eventual additional parameters for the model
	 * @return a LinkedHashMap of BioSimPlot instances (keys) and climate variables (values)
	 * @throws BioSimClientException if the client fails or BioSimServerException if the server fails
	 */
	public static LinkedHashMap<BioSimPlot, BioSimDataSet> getModelOutput(int fromYr, 
			int toYr,
			List<BioSimPlot> locations, 
			RCP rcp,
			ClimateModel climMod,
			String modelName,
			BioSimParameterMap additionalParms)
			throws BioSimClientException, BioSimServerException {
		return BioSimClient.getModelOutput(fromYr, toYr, locations, rcp, climMod, modelName, 1, true, additionalParms);
	}

	
	

	/**
	 * Returns a model output for a particular period. In this method, the ephemeral 
	 * mode can be disabled by setting the argument isEphemeral to false. In such 
	 * a case, the generated climate is cached in memory. This involves
	 * two request to the server. The first aims at generating the climate, whereas
	 * the second applies a model on the generated climate in order to obtain the 
	 * desired variables. The ephemeral model should be preferred when a single model
	 * is to be applied to some locations. If several models are to be applied to the
	 * some locations, then the ephemeral mode should be disabled. The climate is then
	 * generated only once for all the models. This implies several calls to this method
	 * with exactly the same signature except for the argument "modelName". This "modelName"
	 * argument sets the model to be applied on the generated climate. It should be one 
	 * of the strings returned by the getModelList static method. 
	 * 
	 * @param fromYr starting date (yr) of the period (inclusive)
	 * @param toYr ending date (yr) of the period (inclusive)
	 * @param locations the locations of the plots (BioSimPlot instances)
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climMod a ClimateModel enum variable (by default RCM 4)
	 * @param modelName a string representing the model name
	 * @param rep the number of replicates if needed. Should be equal to or greater than 1. 
	 * @param isEphemeral a boolean that overrides the storage procedure on the server
	 * @param additionalParms a BioSimParameterMap instance that contains the eventual additional parameters for the model
	 * @return a LinkedHashMap of BioSimPlot instances (keys) and climate variables (values)
	 * @throws BioSimClientException if the client fails or BioSimServerException if the server fails
	 */
	public static LinkedHashMap<BioSimPlot, BioSimDataSet> getModelOutput(int fromYr, 
			int toYr,
			List<BioSimPlot> locations, 
			RCP rcp,
			ClimateModel climMod,
			String modelName,
			int rep,
			boolean isEphemeral,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
		if (rep < 1) {
			throw new InvalidParameterException("The rep parameter should be equal to or greater than 1!");
		} 
		if (locations.size() > BioSimClient.getMaxNumberLocationsInSingleRequest()) {
			throw new BioSimClientException("The maximum number of locations for a single request is " + MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST);
		}
		if (locations.size() > BioSimClient.getMaximumNbLocationsPerBatchWeatherGeneration()) {
			LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
			List<BioSimPlot> copyList = new ArrayList<BioSimPlot>();
			copyList.addAll(locations);
			List<BioSimPlot> subList = new ArrayList<BioSimPlot>();
			while (!copyList.isEmpty()) {
				while (!copyList.isEmpty() && subList.size() < BioSimClient.getMaximumNbLocationsPerBatchWeatherGeneration()) {
					subList.add(copyList.remove(0));
				}
				resultingMap.putAll(internalCalculationForClimateVariables(fromYr, toYr, subList, rcp, climMod, modelName, rep, isEphemeral, additionalParms));
				subList.clear();
			}
			return resultingMap;
		} else {
			return internalCalculationForClimateVariables(fromYr, toYr, locations, rcp, climMod, modelName, rep, isEphemeral, additionalParms);
		}
	}
	
	
	private static int getMaximumNbLocationsPerBatchWeatherGeneration() throws BioSimClientException, BioSimServerException {
		setMaxCapacities();
		return MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION;
	}

	private static void setMaxCapacities() throws BioSimClientException, BioSimServerException {
		if (MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION == -1 || MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS == -1) {
			String serverReply = getStringFromConnection(BIOSIMMAXCOORDINATES, null);
			try {
				String[] maxCapacities = serverReply.split(FieldSeparator);
				MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS = Integer.parseInt(maxCapacities[0]);
				MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION = Integer.parseInt(maxCapacities[1]);
			} catch (NumberFormatException e) {
				throw new BioSimClientException("The server reply could not be parsed: " + e.getMessage());
			}
		}
	}
	
	private static int getMaximumNbLocationsPerBatchNormals() throws BioSimClientException, BioSimServerException {
		setMaxCapacities();
		return MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS;
	}

	/**
	 * Reset the configuration to its initial values.
	 */
	public static void resetClientConfiguration() {
		NbNearestNeighbours = null;
		ForceClimateGenerationEnabled = false;
	}
	
	
	/**
	 * By default the climate generation retrieves the observations for the
	 * dates prior to the current date. If this option is set to true, then 
	 * the climate is generated from the normals even for dates prior to
	 * the current date.
	 * 
	 * @param bool a boolean
	 */
	public static void setForceClimateGenerationEnabled(boolean bool) {
		ForceClimateGenerationEnabled = bool;
	}

	/**
	 * This option forces the client to generate weather for past dates instead
	 * of using the observations. By default, it is disabled
	 * @return a boolean
	 */
	public static boolean isForceClimateGenerationEnabled() {
		return BioSimClient.ForceClimateGenerationEnabled;
	}

	/**
	 * This option set the number of stations in the imputation of the climate variables
	 * @param nbNearestNeighbours an integer between 1 and 35. The default is 4 stations.
	 */
	public static void setNbNearestNeighbours(int nbNearestNeighbours) {
		if (nbNearestNeighbours < 1 || nbNearestNeighbours > 35) {
			throw new InvalidParameterException("The number of nearest neighbours must be an integer between 1 and 35!");
		}
		NbNearestNeighbours = nbNearestNeighbours;
	}

	/**
	 * Returns the number of climate station used in the imputation of the climate variables.
	 * @return an integer
	 */
	public static int getNbNearestNeighbours() {
		if (NbNearestNeighbours == null) {
			return 4; // default value
		} else {
			return NbNearestNeighbours;
		}
	}
	
	public static void main(String[] args) throws BioSimClientException, BioSimServerException {
//		List<String> modelList = BioSimClient.getModelList();
//		System.out.print(BioSimClient.getModelDefaultParameters(modelList.get(0)));
		System.out.print(BioSimClient.getModelDefaultParameters("Spruce_Budworm_Biology"));
	}
	
}
