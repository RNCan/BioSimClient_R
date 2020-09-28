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
import java.util.HashMap;

public class BioSimParameterMap extends HashMap<String,Object> {

	public void addParameter(String parameterName, Object value) {
		if (value instanceof Number || value instanceof String) {
			put(parameterName, value);
		} else {
			throw new InvalidParameterException("The value must be a String or a Number instance!");
		}
	}

	protected String parse() {
		if (isEmpty()) {
			return null;
		} else {
			String str = "Parameters=";
			String sep = "*"; 
			for (String key : keySet()) {
				Object value = get(key);
				String valueString;
				if (value == null) {
					valueString = "";
				} else {
					valueString = value.toString().trim();
				}
				str += sep + key.trim() + ":" + valueString;
			}
			return str;
		}
	}

	@Override
	public String toString() {
		String str = "";
		for (String key : keySet()) {
			Object value = get(key);
			String valueString;
			if (value == null) {
				valueString = "";
			} else {
				valueString = value.toString().trim();
			}
			str += key.trim() + ":" + valueString + BioSimClient.FieldSeparator;
		}
		str = str.substring(0, str.length() - 1);
		return str;
	}
}
