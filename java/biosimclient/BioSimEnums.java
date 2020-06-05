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

public class BioSimEnums {
	
	public static enum RCP {
		RCP45("4_5"),
		RCP85("8_5");
		
		private final String urlString;
		RCP(String urlString) {
			this.urlString = urlString;
		}
		
		String getURLString() {
			return urlString;
		}
	}
	
	public static enum ClimateModel {
		Hadley,
		RCM4,
		GCM4;
	}

	public static enum Period {
		FromNormals1951_1980("period=1951_1980"),
		FromNormals1961_1990("period=1961_1990"),
		FromNormals1971_2000("period=1971_2000"),
		FromNormals1981_2010("period=1981_2010"),
		FromNormals1991_2020("period=1991_2020"),
		FromNormals2001_2030("period=2001_2030"),
		FromNormals2011_2040("period=2011_2040"),
		FromNormals2021_2050("period=2021_2050"),
		FromNormals2031_2060("period=2031_2060"),
		FromNormals2041_2070("period=2041_2070"),
		FromNormals2051_2080("period=2051_2080"),
		FromNormals2061_2090("period=2061_2090"),
		FromNormals2071_2100("period=2071_2100");
		
		String parsedQuery;
		
		Period(String parsedRequest) {
			this.parsedQuery = parsedRequest;
		}
	}
		
	public static enum Variable {	// TODO complete this
		TN("TMIN_MN", false, "min air temperature"),
		T("", false, "air temperature"),
		TX("TMAX_MN", false, "max air temperature"),
		P("PRCP_TT", true, "precipitation"),
		TD("TDEX_MN", false, "temperature dew point"),
		H("", false, "humidity"),
		WS("", false, "wind speed"),
		WD("", false, "wind direction"),
		R("", true, "solar radiation"),
		Z("", false, "atmospheric pressure"),
		S("", true, "snow precipitation"),
		SD("", false, "snow depth accumulation"),
		SWE("", true, "snow water equivalent"),
		WS2("", false, "wind speed at 2 m");
		
		String description;
		String fieldName;
		boolean additive;
		
		Variable(String fieldName, boolean additive, String description) {
			this.fieldName = fieldName;
			this.additive = additive;
			this.description = description;
		}
		
		public boolean isAdditive() {return additive;};
		public String getDescription() {return description;}
	}
	
	public static enum Month {
		January(31),
		February(28),
		March(31),
		April(30),
		May(31),
		June(30),
		July(31),
		August(31),
		September(30),
		October(31),
		November(30),
		December(31);
		
		int nbDays;
		
		Month(int nbDays) {
			this.nbDays = nbDays;
		}
	}

}
