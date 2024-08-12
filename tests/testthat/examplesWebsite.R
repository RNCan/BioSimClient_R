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
J4R::shutdownClient()


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


library(BioSIM)
l <- structure(list(
  Name = c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6"),
  X = c(-116.623960, -116.459132, -116.294005,
        -116.128578, -115.962854, -115.796835),
  Y = c(58.2332800,
        58.2638898, 58.2942755, 58.3244365, 58.3543720,
        58.3840815),
  elev = c(326, 315, 301, 287, 275, 267)), row.names = c(NA, 6L), class = "data.frame")



a<- generateWeather(modelNames = c("Gypsy_Moth_Seasonality", "MPB_SLR"),
                fromYr = 2015,
                toYr = 2021,
                id=l$Name,
                latDeg = l$Y,
                longDeg = l$X,
                elevM = l$elev,
                rcp = "RCP85",
                climModel = "GCM4")
# getModelHelp("MPB_SLR")
b<-a$Gypsy_Moth_Seasonality
a$MPB_SLR

#Starting local Java server...
#If you are using BioSIM Web API for a scientific publication, please cite it. Type citation("BioSIM") in #your R console to obtain the full citation.
#Error in .checkForExceptionInCallback(callback) :
#  java.lang.NoSuchFieldException: size
#java.lang.Class.getField(Unknown Source)
#j4r.lang.codetranslator.REnvironment.processField(REnvironment.java:384)
#j4r.lang.codetranslator.REnvironment.processCode(REnvironment.java:234)
#j4r.net.server.JavaGatewayServer$JavaGatewayClientThread.processRequest(JavaGatewayServer.java:177)
#j4r.net.server.JavaGatewayServer$JavaGatewayClientThread.run(JavaGatewayServer.java:116)
#java.lang.Thread.run(Unknown Source)


