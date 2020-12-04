###################################
# Tests for the BioSim Client
# Mathieu Fortin - Nov 2019
###################################


# id <- c("Quebec", "Sorel")
# latDeg <- c(46.87, 46.03)
# longDeg <- c(-71.25, -73.12)
# elevM <- c(114, 15)
# twoLocationsInSouthernQuebec <- data.frame(id, latDeg, longDeg, elevM)
# save(file = "./data/twoLocationsInSouthernQuebec.RData", twoLocationsInSouthernQuebec)

library(BioSIM)

locations <- BioSIM::twoLocationsInSouthernQuebec
print(locations)

summerMean <- getNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation, c("June", "July", "August"))
summerMean


monthlyMeans <- getMonthlyNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)
monthlyMeans

annualNormals <- getAnnualNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)
annualNormals

getModelList()

addParms <- c("LowerThreshold"=5)
degreeDays <- getModelOutput(2017, 2021, locations$Name, locations$Latitude, locations$Longitude, locations$Elevation, "DegreeDay_Annual", F, rcp = "RCP85", climModel = "GCM4", additionalParms = addParms)
degreeDays

MPB_SLR <- getModelOutput(2017, 2021, locations$Name, locations$Latitude, locations$Longitude, locations$Elevation, "MPB_SLR", F, rcp = "RCP85", climModel = "GCM4")
MPB_SLR


shutdownJava()


