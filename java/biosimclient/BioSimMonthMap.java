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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import biosimclient.BioSimEnums.Month;
import biosimclient.BioSimEnums.Variable;


/**
 * An inner class that handles the mean and sum of the different variables for normals.
 * @author Mathieu Fortin - October 2019
 */
@SuppressWarnings("serial")
class BioSimMonthMap extends LinkedHashMap<Month, Map<Variable, Double>> {

	
	BioSimMonthMap(BioSimDataSet dataSet) {
		Map<Variable, Integer> fieldIndices = new HashMap<Variable, Integer>();
		for (Variable v : Variable.values()) {
			fieldIndices.put(v, dataSet.getFieldNames().indexOf(v.fieldName));
		}
		int monthIndexInDataset = dataSet.getFieldNames().indexOf("Month");
		for (Observation obs : dataSet.getObservations()) {
			Object[] record = obs.toArray();
			int monthValue = (int) record[monthIndexInDataset];
			Month m = Month.values()[monthValue - 1];
			put(m, new HashMap<Variable, Double>());
			for (Variable v : Variable.values()) {
				if (fieldIndices.get(v) != -1) {
					double value = (double) record[fieldIndices.get(v)];
					get(m).put(v, value);
				}
			}
		}
	}
	
	
	final BioSimDataSet getMeanForTheseMonths(List<Month> months) throws BioSimClientException {
		Map<Variable, Double> outputMap = new LinkedHashMap<Variable, Double>();
		int nbDays = 0;
		for (Month month : months) {
			if (containsKey(month)) {
				for (Variable var : Variable.getVariablesForNormals()) {
					if (get(month).containsKey(var)) {
						double value = get(month).get(var);
						if (!var.isAdditive()) {
							value *= month.nbDays;
						} 
						if (!outputMap.containsKey(var)) {
							outputMap.put(var, 0d);
						}
						outputMap.put(var, outputMap.get(var) + value);
					} else {
						throw new BioSimClientException("The variable " + var.name() + " is not in the MonthMap instance!");
					}
				}
			} else {
				throw new BioSimClientException("The )month " + month.name() + " is not in the MonthMap instance!");
			}
			nbDays += month.nbDays;
		}
		for (Variable var : Variable.getVariablesForNormals()) {
			if (!var.additive) {
				outputMap.put(var, outputMap.get(var) / nbDays);
			}
		}
		
		List<String> fieldNames = new ArrayList<String>();
		for (Variable v : outputMap.keySet()) {
			fieldNames.add(v.toString());
		}
		BioSimDataSet ds = new BioSimDataSet(fieldNames);
		Object[] rec = new Object[outputMap.size()];
		int i = 0;
		for (Variable v : outputMap.keySet()) {
			rec[i++] = outputMap.get(v);
		}
		ds.addObservation(rec);
		ds.indexFieldType();
		return ds;
	}
	
}

