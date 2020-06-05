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

import biosimclient.BioSimEnums.ClimateModel;
import biosimclient.BioSimEnums.RCP;

/**
 * Internal class for storing generated climate. Two queries are assumed to be the
 * same when all their internal fields are the same. The latitude, longitude and 
 * elevation are assumed to be within a one-meter difference.  
 * @author Mathieu Fortin - December 2019
 */
class BioSimQuerySignature {
	
	final int initialYear;
	final int finalYear;
	final RCP rcp;
	final ClimateModel climMod;
	final double latitudeDeg;
	final double longitudeDeg;
	final double elevationM;
	final int rep;
	
	BioSimQuerySignature(int initialYear, int finalYear, BioSimPlot location, RCP rcp, ClimateModel climMod, int rep) {
		this.initialYear = initialYear;
		this.finalYear = finalYear;
		if (rcp == null) {
			this.rcp = RCP.RCP45; // default value
 		} else {
 			this.rcp = rcp;
 		}
		if (climMod == null) {
			this.climMod = ClimateModel.RCM4; // default value
		} else {
			this.climMod = climMod;
		}
		this.latitudeDeg = location.getLatitudeDeg();
		this.longitudeDeg = location.getLongitudeDeg();
		this.elevationM = location.getElevationM();
		this.rep = rep;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BioSimQuerySignature) {
			BioSimQuerySignature thatQuery = (BioSimQuerySignature) obj;
			if (thatQuery.initialYear == initialYear) {
				if (thatQuery.finalYear == finalYear) {
					if (thatQuery.rep == rep) {
						if (thatQuery.rcp == rcp) {
							if (thatQuery.climMod == climMod) {
								if (Math.abs(thatQuery.latitudeDeg - latitudeDeg) < 1E-5) {		// about 1 m at equator
									if (Math.abs(thatQuery.longitudeDeg - longitudeDeg) < 1E-5) { // about 1 m at equator
										if (Math.abs(thatQuery.elevationM - elevationM) < 1d) { // one meter 
											return true;
										}
									}
								}
							}
						}
					}
				}
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return initialYear * 10000000 + finalYear;
	}

}
