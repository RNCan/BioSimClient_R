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

	private static final int MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION = 10;
	private static final int MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS = 50;
	private static final int MAXIMUM_NB_LOCATIONS_PER_BATCH_REMOVALS = 200;
	private static int MAXIMUM_NB_LOCATIONS_IN_A_SINGLE_REQUEST = -1; // not set yet
	
	private static final String FieldSeparator = ",";
	
	private static final InetSocketAddress REpiceaAddress = new InetSocketAddress("repicea.dynu.net", 80);
	private static final InetSocketAddress LocalAddress = new InetSocketAddress("192.168.0.194", 5000);
	
	private static final String SPACE_IN_REQUEST = "%20";

	static final List<Month> AllMonths = Arrays.asList(Month.values());

	private static final String NORMAL_API = "BioSimNormals";
	private static final String GENERATOR_API = "BioSimWG";
	private static final String MODEL_API = "BioSimModel";
	private static final String MODEL_LIST_API = "BioSimModelList";
	private static final String BIOSIMCLEANUP_API = "BioSimMemoryCleanUp";
	private static final String BIOSIMMEMORYLOAD_API = "BioSimMemoryLoad";
	private static final String BIOSIMMAXMEMORY_API = "BioSimMaxMemory";

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

//	private static boolean MultithreadingEnabled = true; // Default value
	
	private final static String addQueryIfAny(String urlString, String query) {
		if (query != null && !query.isEmpty()) {
			return urlString.trim() + "?" + query;
		} else {
			return urlString;
		}
	}

	private final static String getStringFromConnection(String api, String query) throws BioSimClientException, BioSimServerException {
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

		String variablesQuery = getVariablesQuery();

		String query = constructCoordinatesQuery(locations);

		query += "&var=" + variablesQuery;
		query += "&compress=0"; // compression is disabled by default
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
						if (!Variable.getFieldNames().contains(bioSimDataSet.getFieldNames().get(i))) {
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

	private static synchronized int getMaxNumberLocationsInSingleRequest() {
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
		if (locations.size() > MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS) {
			LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
			List<BioSimPlot> copyList = new ArrayList<BioSimPlot>();
			copyList.addAll(locations);
			List<BioSimPlot> subList = new ArrayList<BioSimPlot>();
			while (!copyList.isEmpty()) {
				while (!copyList.isEmpty() && subList.size() < MAXIMUM_NB_LOCATIONS_PER_BATCH_NORMALS) {
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

	protected static void removeWgoutObjectsFromServer(Collection<String> references) 
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

	protected static int getNbWgoutObjectsOnServer() throws Exception {
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
	
	@Deprecated
	private static String getVariablesQuery() {		// TODO FP remove this when the server side is updated
		String variablesQuery = "";
		List<Variable> variables = Arrays.asList(Variable.values());
		for (Variable v : variables) {
			variablesQuery += v.name();
			if (variables.indexOf(v) < variables.size() - 1) {
				variablesQuery += SPACE_IN_REQUEST;
			}
		}
		return variablesQuery;
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
	protected static LinkedHashMap<BioSimPlot, String> getGeneratedClimate(
			int fromYr, 
			int toYr,
			List<BioSimPlot> locations,
			RCP rcp,
			ClimateModel climModel,
			int rep) throws BioSimClientException, BioSimServerException {
		boolean compress = false; // disabling compression by default
		LinkedHashMap<BioSimPlot, String> outputMap = new LinkedHashMap<BioSimPlot, String>();

		String variablesQuery = getVariablesQuery();
		
		String query = constructCoordinatesQuery(locations);
		query += "&var=" + variablesQuery;
		if (compress) {
			query += "&compress=1";
		} else {
			query += "&compress=0";
		}
		query += "&from=" + fromYr;
		query += "&to=" + toYr;
		if (rcp != null) {
			query += "&rcp=" + rcp.getURLString();
		}
		
		if(climModel != null) {
			query += "&climMod=" + climModel.name();
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
	protected static LinkedHashMap<BioSimPlot, BioSimDataSet> applyModel(
			String modelName,
			LinkedHashMap<BioSimPlot, String> teleIORefs,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
		if (!getReferenceModelList().contains(modelName)) {
			throw new InvalidParameterException("The model " + modelName
					+ " is not a valid model. Please consult the list of models through the function getModelList()");
		}
		boolean compress = false; // disabling compression

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
		if (compress) {
			query += "&compress=1";
		} else {
			query += "&compress=0";
		}
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
	
	
	/**
	 * Returns a model output for a particular period. The method involves
	 * two request to the server. The first aims at generating the climate, whereas
	 * the second applies a model on the generated climate in order to obtain the 
	 * desired variables. The "modelname" parameter sets the model to be applied on
	 * the generated climate. It should be one of the strings returned by the 
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
		return BioSimClient.getModelOutput(fromYr, toYr, locations, rcp, climMod, modelName, rep, false, additionalParms);
	}

	
	/**
	 * Returns a model output for a particular period. The method involves
	 * two request to the server. The first aims at generating the climate, whereas
	 * the second applies a model on the generated climate in order to obtain the 
	 * desired variables. The "modelname" parameter sets the model to be applied on
	 * the generated climate. It should be one of the strings returned by the 
	 * getModelList static method. Generating the climate is time consuming. The 
	 * generated climate is stored on the server and it can be re used with some 
	 * other models. The number of replicates is set to 1 here.
	 * 
	 * @param fromYr starting date (yr) of the period (inclusive)
	 * @param toYr ending date (yr) of the period (inclusive)
	 * @param locations the locations of the plots (BioSimPlot instances)
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climMod a ClimateModel enum variable (by default RCM 4)
	 * @param modelName a string representing the model name
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
		return BioSimClient.getModelOutput(fromYr, toYr, locations, rcp, climMod, modelName, 1, false, additionalParms);
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
		} else { // here we retrieve what is already available
			for (BioSimPlot location : locations) {
				BioSimQuerySignature querySignature = new BioSimQuerySignature(fromYr, toYr, location, rcp, climMod, rep);
				if (GeneratedClimateMap.containsKey(querySignature)) {
					alreadyGeneratedClimate.put(location, GeneratedClimateMap.get(querySignature));
				} else {
					locationsToGenerate.add(location);
				}
			}
		}

		Map<BioSimPlot, String> generatedClimate = new HashMap<BioSimPlot, String>();
		if (!locationsToGenerate.isEmpty()) { // here we generate the climate if needed
			generatedClimate.putAll(BioSimClient.getGeneratedClimate(fromYr, toYr, locationsToGenerate, rcp, climMod, rep));
			
			if (!isEphemeral) { // then we stored the reference in the static map for future use
				for (BioSimPlot location : generatedClimate.keySet()) {
					GeneratedClimateMap.put(new BioSimQuerySignature(fromYr, toYr, location, rcp, climMod, rep),
							generatedClimate.get(location));
				}
			}
		}

		generatedClimate.putAll(alreadyGeneratedClimate);

		LinkedHashMap<BioSimPlot, String> mapForModels = new LinkedHashMap<BioSimPlot, String>();
		for (BioSimPlot location : locations) {
			mapForModels.put(location, generatedClimate.get(location));
		}
		LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = BioSimClient.applyModel(modelName, mapForModels, additionalParms);
		if (isEphemeral) { // then we remove the wgout instances stored on the server
			BioSimClient.removeWgoutObjectsFromServer(generatedClimate.values());
		}
		return resultingMap;
	}

	/**
	 * Returns a model output for a particular period. The method involves
	 * two request to the server. The first aims at generating the climate, whereas
	 * the second applies a model on the generated climate in order to obtain the 
	 * desired variables. The "modelname" parameter sets the model to be applied on
	 * the generated climate. It should be one of the strings returned by the 
	 * getModelList static method. Generating the climate is time consuming. So there
	 * is a procedure to avoid generating the climate twice for the same location. The
	 * "isEphemeral" parameter actually overrides this procedure if it is set to true.
	 * Otherwise, the generated climate is stored on the server and it can be re used 
	 * with some other models.
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
		if (locations.size() > MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION) {
			LinkedHashMap<BioSimPlot, BioSimDataSet> resultingMap = new LinkedHashMap<BioSimPlot, BioSimDataSet>();
			List<BioSimPlot> copyList = new ArrayList<BioSimPlot>();
			copyList.addAll(locations);
			List<BioSimPlot> subList = new ArrayList<BioSimPlot>();
			while (!copyList.isEmpty()) {
				while (!copyList.isEmpty() && subList.size() < MAXIMUM_NB_LOCATIONS_PER_BATCH_WEATHER_GENERATION) {
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

	/**
	 * Returns a model output for a particular period. The method involves
	 * two request to the server. The first aims at generating the climate, whereas
	 * the second applies a model on the generated climate in order to obtain the 
	 * desired variables. The "modelname" parameter sets the model to be applied on
	 * the generated climate. It should be one of the strings returned by the 
	 * getModelList static method. Generating the climate is time consuming. So there
	 * is a procedure to avoid generating the climate twice for the same location. The
	 * "isEphemeral" parameter actually overrides this procedure if it is set to true.
	 * Otherwise, the generated climate is stored on the server and it can be re used 
	 * with some other models. The number of replicates is set to 1 here.
	 * 
	 * @param fromYr starting date (yr) of the period (inclusive)
	 * @param toYr ending date (yr) of the period (inclusive)
	 * @param locations the locations of the plots (BioSimPlot instances)
	 * @param rcp an RCP enum variable (by default RCP 4.5)
	 * @param climMod a ClimateModel enum variable (by default RCM 4)
	 * @param modelName a string representing the model name
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
			boolean isEphemeral,
			BioSimParameterMap additionalParms) throws BioSimClientException, BioSimServerException {
		return getModelOutput(fromYr, toYr, locations, rcp, climMod, modelName, 1, isEphemeral, additionalParms);
	}

//	public static void main(String[] args) throws BioSimClientException {
////		List<String> references = new ArrayList<String>();
////		for (int i = 0; i < 402; i++) {
////			references.add("" + i);
////		}			
////		
////		BioSimClient.removeWgoutObjectsFromServer(references);
//		List<String> models = BioSimClient.getModelList();
//		for (String model : models)
//			System.out.println(model);
//	}
}
