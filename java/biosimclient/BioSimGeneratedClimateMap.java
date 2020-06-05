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

import java.util.concurrent.ConcurrentHashMap;

/**
 * A one-to-one map to ensure that there is only one query signature for 
 * one reference.
 * @author Mathieu Fortin - December 2019
 */
class BioSimGeneratedClimateMap extends ConcurrentHashMap<BioSimQuerySignature, String> {

	final ConcurrentHashMap<String, BioSimQuerySignature> sisterMap; 
	
	BioSimGeneratedClimateMap() {
		sisterMap = new ConcurrentHashMap<String, BioSimQuerySignature>();
	}

	@Override
	public String put(BioSimQuerySignature signature, String reference) {
		if (contains(signature)) {
			sisterMap.remove(get(signature));	// we first remove the entry in the sister map if it exists
		}
		super.put(signature, reference);
		sisterMap.put(reference, signature);
		return reference;
	}
	
	void removeValue(String reference) {
		if (sisterMap.contains(reference)) {
			BioSimQuerySignature signature = sisterMap.get(reference);
			remove(signature);
			sisterMap.remove(reference);
		}
	}
	
}
