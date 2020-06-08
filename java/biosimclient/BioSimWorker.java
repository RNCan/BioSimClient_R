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
import java.util.LinkedHashMap;
import java.util.List;

import biosimclient.BioSimEnums.ClimateModel;
import biosimclient.BioSimEnums.RCP;

public class BioSimWorker extends Thread {

	LinkedHashMap<BioSimPlot, String> output;
	Exception e;
	final int fromYr; 
	final int toYr; 
	final List<BioSimPlot> locationsForThisThread; 
	final RCP rcp; 
	final ClimateModel climMod; 
	final int rep;
	final int from;
	final int to;
	
	BioSimWorker(int fromYr, int toYr, List<BioSimPlot> locationsToGenerate, RCP rcp, ClimateModel climMod, int rep, int from, int to) {
		super();
		this.from = from;
		this.to = to;
		this.fromYr = fromYr;
		this.toYr = toYr;
		this.locationsForThisThread = new ArrayList<BioSimPlot>();
		for (int i = from; i <= to; i++) {
			locationsForThisThread.add(locationsToGenerate.get(i));
		}
		this.rcp = rcp;
		this.climMod = climMod;
		this.rep = rep;
		start();
	}
	
	@Override
	public void run() {
		try {
			LinkedHashMap<BioSimPlot,String> out = BioSimClient.getGeneratedClimate(fromYr, toYr, locationsForThisThread, rcp, climMod, rep);
			output = new LinkedHashMap<BioSimPlot, String>();
			output.putAll(out);
		} catch (Exception e) {
			this.e = e;
		}
	}
	
}
