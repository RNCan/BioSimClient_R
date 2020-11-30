###################################
# Tests for the BioSim Client
# Mathieu Fortin - Nov 2019
###################################


library(BioSIM)

locations <- BioSIM::twoLocationsInSouthernQuebec

annualMean <- getAnnualNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)

biosimclient.config(forceClimateGenerationEnabled = T)

ClimaticQc_Annual <- getModelOutput(1981, 2010,
                                    locations$Name,
                                    locations$Latitude,
                                    locations$Longitude,
                                    locations$Elevation,
                                    "ClimaticQc_Annual",
                                    rep=10)

ClimaticQc_Annual_moy <- aggregate(ClimaticQc_Annual[, c(8,11,13)], list(ClimaticQc_Annual$KeyID), mean)

annualMean

ClimaticQc_Annual_moy

shutdownJava()


