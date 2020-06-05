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
import java.util.Arrays;
import java.util.List;

@SuppressWarnings("rawtypes")
public class Observation implements Comparable {

	static List<Integer> comparableFields = new ArrayList<Integer>();
	
	List<Object> values;
	
	protected Observation(Object[] obj) {
		values = new ArrayList<Object>();
		values.addAll(Arrays.asList(obj));
	}
		
	@SuppressWarnings("unchecked")
	@Override
	public int compareTo(Object o) {
		for (Integer index : comparableFields) {
			Comparable thisValue = (Comparable) values.get(index);
			Comparable thatValue = (Comparable) ((Observation) o).values.get(index);
			int comparisonResult = thisValue.compareTo(thatValue);
			if (comparisonResult < 0) {
				return -1;
			} else if (comparisonResult > 0) {
				return 1;
			}
		}
		return 0;
	}

	/**
	 * Converts this observation to an array of Object instances
	 * @return an Array of Object instances
	 */
	public Object[] toArray() {return values.toArray();}


	/**
	 * Checks if two observations have the same values.
	 * @param obs an Observation instance
	 * @return a boolean
	 */
	public boolean isEqualToThisObservation(Observation obs) {
		if (obs == null) {
			return false;
		} else {
			if (values.size() != obs.values.size()) {
				return false;
			} 
			for (int i = 0; i < values.size(); i++) {
				Object thisValue = values.get(i);
				Object thatValue = obs.values.get(i);
				Class thisClass = thisValue.getClass();
				if (!thisClass.equals(thatValue.getClass())) {
					return false;
 				} else {
 					if (thisClass.equals(Double.class)) {
 						if (Math.abs((Double) thisValue - (Double) thatValue) > 1E-8) {
 							return false;
 						}
 					} else if (!thisValue.equals(thatValue)) {
 						return false;
 					}
 				}
			}
			return true;
		}
	}
}
