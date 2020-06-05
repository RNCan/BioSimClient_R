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

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import biosimclient.BioSimEnums.Month;
import biosimclient.BioSimEnums.Variable;

/**
 * Handles the server replies. This class derived from DataSet includes
 * a function that makes it possible to convert the DataSet instance into
 * a Map provided that there is no repeated entry. Should it be the case,
 * tjhe getMap method would throw an Exception.
 *
 * @author Mathieu Fortin - March 2020
 *
 */
public class BioSimDataSet {

	protected List<String> fieldNames;
	protected List<Class<?>> fieldTypes;
	protected List<Observation> observations;

	/**
	 * Only constructor with the field names.
	 * @param fieldNames a List of String instances
	 */
	public BioSimDataSet(List<String> fieldNames) {
		this.fieldNames = new ArrayList<String>();
		fieldTypes = new ArrayList<Class<?>>();
		observations = new ArrayList<Observation>();
		for (String fieldName : fieldNames) {
			addFieldName(fieldName);
		}
	}

	private void addFieldName(String name) {
		int index = 0;
		while (fieldNames.contains(name)) {
			name = name.concat(((Integer) index).toString());
		}
		fieldNames.add(name);
	}

	/**
	 * This method returns the number of observations in the dataset.
	 * @return an integer
	 */
	public int getNumberOfObservations() {
		return observations.size();
	}

	/**
	 * Returns the field names in a list. The list is a new list so that changes will
	 * not affect the fieldNames member.
	 * @return a List instance
	 */
	public List<String> getFieldNames() {
		List<String> fieldNames = new ArrayList<String>();
		fieldNames.addAll(this.fieldNames);
		return fieldNames;
	}

	
	/**
	 * Indexes the different field types. More specifically, it goes 
	 * through the columns and find the appropriate class for a particular
	 * field. This method should be called after adding all the observations.
	 */
	public void indexFieldType() {
		fieldTypes.clear();
		for (int j = 0; j < fieldNames.size(); j++) {
			setClassOfThisField(j);
		}
	}

	/**
	 * Returns the observations of the data set.
	 * @return a List of Observation instances
	 */
	public List<Observation> getObservations() {
		return observations;
	}
	
	public void addObservation(Object[] observationFrame) {
		parseDifferentFields(observationFrame);
		observations.add(new Observation(observationFrame));
	}

	private void parseDifferentFields(Object[] lineRead) {
		for (int i = 0; i < fieldNames.size(); i++) {
			try {
				lineRead[i] = (Integer) Integer.parseInt(lineRead[i].toString());
			} catch (NumberFormatException e1) {
				try {
					lineRead[i] = (Double) Double.parseDouble(lineRead[i].toString());
				} catch (NumberFormatException e2) {
					lineRead[i] = lineRead[i].toString();
				}
			}
		}
	}
	
	private void setClassOfThisField(int fieldIndex) {
		if (isInteger(fieldIndex)) {
			setFieldType(fieldIndex, Integer.class);
		} else if (isDouble(fieldIndex)) {
			setFieldType(fieldIndex, Double.class);
			reconvertToDoubleIfNeedsBe(fieldIndex);
		} else {
			setFieldType(fieldIndex, String.class);
			reconvertToStringIfNeedsBe(fieldIndex);
		}
	}
	
	private boolean isInteger(int j) {
		boolean isInteger = true;
		for (int i = 0; i < getNumberOfObservations(); i++) {
			if (!(getValueAt(i,j) instanceof Integer)) {
					isInteger = false;
					break;
			} 
		}
		return isInteger;
	}


	private void reconvertToStringIfNeedsBe(int j) {
		for (int i = 0; i < getNumberOfObservations(); i++) {
			Object value = getValueAt(i,j);
			if ((value instanceof Number)) {
				setValueAt(i,j, value.toString());
			}
		} 
	}

	private void reconvertToDoubleIfNeedsBe(int j) {
		for (int i = 0; i < getNumberOfObservations(); i++) {
			Object value = getValueAt(i,j);
			if ((value instanceof Integer)) {
				setValueAt(i,j, ((Integer) value).doubleValue()); // MF2020-04-30 Bug corrected here it was previously changed for a String
			}
		} 
	}

	/**
	 * This method returns any object in the dataset at row i and column j.
	 * @param i the index of the row
	 * @param j the index of the column
	 * @return an Object instance
	 */
	protected Object getValueAt(int i, int j) {
		return observations.get(i).values.get(j);
	}


	private void setValueAt(int i, int j, Object value) {
		if (value.getClass().equals(fieldTypes.get(j))) {
			observations.get(i).values.remove(j);
			observations.get(i).values.add(j, value);
		} 
	}

	private boolean isDouble(int indexJ) {
		boolean isDouble = true;
		for (int i = 0; i < getNumberOfObservations(); i++) {
			if (!(getValueAt(i,indexJ) instanceof Number)) {
					isDouble = false;
					break;
			} 
		}
		return isDouble;
	}


	private void setFieldType(int fieldIndex, Class clazz) {
		if (fieldIndex < fieldTypes.size()) {
			fieldTypes.set(fieldIndex, clazz);	
		} else if (fieldIndex == fieldTypes.size()) {
			fieldTypes.add(clazz);	
		} else {
			throw new InvalidParameterException("The field type cannot be set!");
		}
	}

	
	/**
	 * Converts the DataSet instance into a Map. There should not be any deplicate entry. 
	 * Otherwise the method returns an Exception.
	 * @return a LinkedHashMap with embedded LinkedHashMap if there are more than two fields.
	 */
	public LinkedHashMap getMap() {
		LinkedHashMap outputMap = new LinkedHashMap();
		Object[] rec;
		LinkedHashMap currentMap;
		for (Observation obs : getObservations()) {
			rec = obs.toArray();
			currentMap = outputMap;
			for (int i = 0; i < rec.length - 1; i++) {
				if (i == rec.length - 2) {
					currentMap.put(rec[i], rec[i+1]);
				} else if (!currentMap.containsKey(rec[i])) {
					currentMap.put(rec[i], new LinkedHashMap());
					currentMap = (LinkedHashMap) currentMap.get(rec[i]);
				} else {
					throw new InvalidParameterException();
				}
			}
		}
		return outputMap;
	}
	
	final BioSimDataSet getMonthDataSet(List<Month> months, List<Variable> variables) throws BioSimClientException {
		BioSimMonthMap monthMap = new BioSimMonthMap(this);
		return monthMap.getMeanForTheseMonths(months, variables);
	}

}
