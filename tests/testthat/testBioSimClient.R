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

biosimLocal <- F


biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)

latDeg <- runif(n = 3, min=48, max=51)
longDeg <- runif(n = 3, min=-76, max=-66)
ids <- c("plot1", "plot1", "plot2")

testWithSameIds <- generateWeather("DegreeDay_Annual", 1999, 2000, ids, latDeg, longDeg)
test_that("Testing that replicated ids do not interfere", {
   expect_equal(nrow(testWithSameIds$DegreeDay_Annual), 6)
})

output <- generateWeather("DegreeDay_Annual", 1990, 1990, "Reservoir Gouin", 48.5, -74.5, NA)[["DegreeDay_Annual"]]

test_that("Testing that one elevation with NA are properly processed", {
  expect_equal(output$DD, 2195.55, tolerance = 1E-8)
})

output <- generateWeather(modelName = "DegreeDay_Annual", fromYr = 1990, toYr = 1990, id=c("Reservoir Gouin", "fake1", "fake2"), latDeg = c(48.5,49,50), longDeg = c(-74.5,-73,-72), elevM = c(300,NA,200))[["DegreeDay_Annual"]]

test_that("Testing that one elevation among others with NA are properly processed", {
  expect_equal(output[1,"DD"], 2290.10, tolerance = 1E-8)
  expect_equal(output[2,"DD"], 2275.30, tolerance = 1E-8)
  expect_equal(output[3,"DD"], 2218.95, tolerance = 1E-8)
})

output <- generateWeather(modelName = "DegreeDay_Annual", fromYr = 1990, toYr = 1990, id=c("Reservoir Gouin", "fake1", "fake2"), latDeg = c(48.5,49,50), longDeg = c(-74.5,-73,-72), elevM = c(NA,NA,NA))[["DegreeDay_Annual"]]

test_that("Testing that all the elevations with NA are properly processed", {
  expect_equal(output[1,"DD"], 2195.55, tolerance = 1E-8)
  expect_equal(output[2,"DD"], 2275.30, tolerance = 1E-8)
  expect_equal(output[3,"DD"], 1986.00, tolerance = 1E-8)
})

output <- generateWeather(modelName = "DegreeDay_Annual", fromYr = 1990, toYr = 1990, id=c("Reservoir Gouin", "fake1", "fake2"), latDeg = c(48.5,49,50), longDeg = c(-74.5,-73,-72))[["DegreeDay_Annual"]]

test_that("Testing that all the elevations with no elevation argument are properly processed", {
  expect_equal(output[1,"DD"], 2195.55, tolerance = 1E-8)
  expect_equal(output[2,"DD"], 2275.30, tolerance = 1E-8)
  expect_equal(output[3,"DD"], 1986.00, tolerance = 1E-8)
})

locations <- BioSIM::twoLocationsInSouthernQuebec
print(locations)

normals <- getAnnualNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)

test_that("Testing that 1981-2010 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TN"], -0.1383562, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TX"], 9.331781, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"P"], 1319, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TN"], 1.69589, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TX"], 11.37041, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"P"], 1033.4, tolerance = 1E-4)
})

normals <- getAnnualNormals("1971_2000", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)

test_that("Testing that 1971-2000 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TN"], -0.3986, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TX"], 9.1545, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"P"], 1290.8, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TN"], 1.0715, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TX"], 11.0635, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"P"], 978.7, tolerance = 1E-4)
})

normals <- getAnnualNormals("1961_1990", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)

test_that("Testing that 1961-1990 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TN"], -0.7035, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TX"], 9.1295, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"P"], 1237.2, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TN"], 0.6646, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TX"], 10.9737, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"P"], 947.7, tolerance = 1E-4)
})

normals <- getAnnualNormals("1951_1980", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)

test_that("Testing that 1951-1980 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TN"], -0.7252, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TX"], 9.0410, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"P"], 1198.2, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TN"], 0.7326, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TX"], 10.9416, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"P"], 949.9, tolerance = 1E-4)
})

summerMean <- getNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation, c("June", "July", "August"))

test_that("Testing that 1981-2010 summer normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(summerMean[which(summerMean$KeyID == "Quebec"),"TN"], 12.5206, tolerance = 1E-4)
  expect_equal(summerMean[which(summerMean$KeyID == "Quebec"),"TX"], 23.3858, tolerance = 1E-4)
  expect_equal(summerMean[which(summerMean$KeyID == "Quebec"),"P"], 363.7, tolerance = 1E-4)
  expect_equal(summerMean[which(summerMean$KeyID == "Sorel"),"TN"], 14.7489, tolerance = 1E-4)
  expect_equal(summerMean[which(summerMean$KeyID == "Sorel"),"TX"], 25.2467, tolerance = 1E-4)
  expect_equal(summerMean[which(summerMean$KeyID == "Sorel"),"P"], 302.9, tolerance = 1E-4)
})

normals <- getAnnualNormals("2061_2090", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation, rcp="RCP85", climModel="Hadley")

test_that("Testing that 2061-2090 annual normals under RCP 8.5 and climate model Hadley for Quebec and Sorel can be properly retrieved", {
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TN"], 7.6723, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"TX"], 16.3221, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Quebec"),"P"], 1378.0, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TN"], 8.4736, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"TX"], 17.8904, tolerance = 1E-4)
  expect_equal(normals[which(normals$KeyID == "Sorel"),"P"], 1159.2, tolerance = 1E-4)
})

#system.time({
degreeDays <- generateWeather("DegreeDay_Annual", 1994, 2002, locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)[["DegreeDay_Annual"]]
#})
test_that("Testing degree-days between 1994 and 2004 for Quebec and Sorel can be properly retrieved", {
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1994),"DD"], 2818.35, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1995),"DD"], 2836.45, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1996),"DD"], 2754.80, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1997),"DD"], 2548.75, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1998),"DD"], 2894.25, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 1999),"DD"], 2959.20, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2000),"DD"], 2686.75, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2001),"DD"], 2929.85, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2002),"DD"], 2711.30, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1994),"DD"], 3312.85, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1995),"DD"], 3382.15, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1996),"DD"], 3258.50, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1997),"DD"], 3061.85, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1998),"DD"], 3514.90, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 1999),"DD"], 3659.80, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2000),"DD"], 3197.30, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2001),"DD"], 3524.25, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2002),"DD"], 3232.50, tolerance = 1E-4)
})


growingSeason <- generateWeather("GrowingSeason", 1994, 2002, locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)[["GrowingSeason"]]

test_that("Testing growing season between 1994 and 2004 for Quebec and Sorel can be properly retrieved", {
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1994),"Length"], 174, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1995),"Length"], 187, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1996),"Length"], 171, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1997),"Length"], 172, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1998),"Length"], 179, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 1999),"Length"], 162, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 2000),"Length"], 171, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 2001),"Length"], 189, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Quebec" & growingSeason$Year == 2002),"Length"], 169, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1994),"Length"], 220, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1995),"Length"], 205, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1996),"Length"], 209, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1997),"Length"], 184, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1998),"Length"], 234, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 1999),"Length"], 218, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 2000),"Length"], 200, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 2001),"Length"], 216, tolerance = 1E-4)
  expect_equal(growingSeason[which(growingSeason$KeyID == "Sorel" & growingSeason$Year == 2002),"Length"], 173, tolerance = 1E-4)
})

degreeDays <- generateWeather("DegreeDay_Annual", 2017, 2021,
                              locations$Name, locations$Latitude, locations$Longitude,
                              locations$Elevation, rcp = "RCP85", climModel = "GCM4")[["DegreeDay_Annual"]]

test_that("Testing degree-days between 2017 and 2021 under RCP 8.5 and climate model GCM4 for Quebec and Sorel can be properly retrieved", {
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2017),"DD"], 2851.30, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2018),"DD"], 2741.35, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2019),"DD"], 2611.30, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2020),"DD"], 2783.15, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2021),"DD"], 3173.75, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2017),"DD"], 3552.60, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2018),"DD"], 3454.60, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2019),"DD"], 3266.15, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2020),"DD"], 3539.65, toletance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2021),"DD"], 3803.15, toletance = 1E-4)
})

degreeDays <- generateWeather("DegreeDay_Annual", 2017, 2018,
                              locations$Name, locations$Latitude, locations$Longitude,
                              locations$Elevation, additionalParms = list(c("LowerThreshold"=5)))[["DegreeDay_Annual"]]

test_that("Testing degree-days above 5C in 2017 and 2018 can be properly retrieved", {
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2017),"DD"], 1797.15, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Quebec" & degreeDays$Year == 2018),"DD"], 1781.00, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2017),"DD"], 2396.45, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "Sorel" & degreeDays$Year == 2018),"DD"], 2390.85, tolerance = 1E-4)
})


biosimclient.config(forceClimateGenerationEnabled = T)
degreeDays1 <- generateWeather("DegreeDay_Annual", 2017, 2018,
                                  locations$Name, locations$Latitude, locations$Longitude, locations$Elevation,
                                  additionalParms = list(c("LowerThreshold"=5)))[["DegreeDay_Annual"]]
degreeDays2 <- generateWeather("DegreeDay_Annual", 2017, 2018,
                               locations$Name, locations$Latitude, locations$Longitude, locations$Elevation,
                               additionalParms = list(c("LowerThreshold"=5)))[["DegreeDay_Annual"]]

test_that("Testing degree-days above 5C in 2017 and 2018 are generated and not compiled from observations", {
  expect_equal(all(degreeDays1$DD != 0), TRUE)
})
biosimclient.config()  ### reset the configuration
biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)



biosimclient.config(nbNearestNeighbours = 20)
degreeDays <- generateWeather("DegreeDay_Annual", 2017, 2018, "lostSomewhere", 50, -70, 300, additionalParms = list(c("LowerThreshold"=5)))[["DegreeDay_Annual"]]
test_that("Testing degree-days above 5C in 2017 and 2018 for 20 nearest neighbours", {
  expect_equal(degreeDays[which(degreeDays$KeyID == "lostSomewhere" & degreeDays$Year == 2017),"DD"], 1221.5, tolerance = 1E-4)
  expect_equal(degreeDays[which(degreeDays$KeyID == "lostSomewhere" & degreeDays$Year == 2018),"DD"], 1249.3, tolerance = 1E-4)
})
biosimclient.config()  ### reset the configuration
biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)



addParms <- getModelDefaultParameters("DegreeDay_Annual")
test_that("Testing default parameters are returned", {
  expect_equal(length(addParms), 8)
})

dd1 <- generateWeather("DegreeDay_Annual", 2017, 2018, "lostSomewhere", 50, -70, 300)[["DegreeDay_Annual"]]
dd2 <- generateWeather("DegreeDay_Annual", 2017, 2018, "lostSomewhere", 50, -70, 300, additionalParms = list(addParms))[["DegreeDay_Annual"]]
test_that("Testing degree-days above 5C in 2017 and 2018 are generated and not compiled from observations", {
  expect_equal(length(dd1[,1]), 2)
  expect_equal(dd1[which(dd1$KeyID == "lostSomewhere" & dd1$Year == 2017),"DD"], 2107.85, tolerance = 1E-4)
  expect_equal(dd1[which(dd1$KeyID == "lostSomewhere" & dd1$Year == 2018),"DD"], 2002.50, tolerance = 1E-4)
  expect_equal(dd1[which(dd1$KeyID == "lostSomewhere" & dd1$Year == 2017),"DD"], dd2[which(dd2$KeyID == "lostSomewhere" & dd2$Year == 2017),"DD"], tolerance = 1E-4)
  expect_equal(dd1[which(dd1$KeyID == "lostSomewhere" & dd1$Year == 2018),"DD"], dd2[which(dd2$KeyID == "lostSomewhere" & dd2$Year == 2018),"DD"], tolerance = 1E-4)
})


biosimclient.config(forceClimateGenerationEnabled = T, nbNearestNeighbours = 4)
ClimaticQc_Annual <- generateWeather("ClimaticQc_Annual", 1981, 2010, locations$Name, locations$Latitude, locations$Longitude,
                                    locations$Elevation, rep=2)[["ClimaticQc_Annual"]]
test_that("Testing that we get 120 observations", {
  expect_equal(length(ClimaticQc_Annual[,1]), 120)
})
biosimclient.config()
biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)



annualMean <- getAnnualNormals("1981_2010", locations$Name, locations$Latitude, locations$Longitude, locations$Elevation)
biosimclient.config(forceClimateGenerationEnabled = T)
ClimaticQc_Annual <- generateWeather("ClimaticQc_Annual",
                                    1981, 2010,
                                    locations$Name,
                                    locations$Latitude,
                                    locations$Longitude,
                                    locations$Elevation,
                                    rep=10)[["ClimaticQc_Annual"]]
biosimclient.config()
biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)


ClimaticQc_Annual_moy <- aggregate(ClimaticQc_Annual[, c(8,11,13)], list(ClimaticQc_Annual$KeyID), mean)
colnames(ClimaticQc_Annual_moy) <- c("KeyID", "P", "TN", "TX")

test_that("Testing that temperatures do not differ by more than 0.1 C", {
  expect_equal(any(abs(ClimaticQc_Annual_moy[,c("TN", "TX")] - annualMean[,c("TN", "TX")]) > 0.2), FALSE)
})

test_that("Testing that precipitation does not differ by more than 50mm", {
  expect_equal(any(abs(ClimaticQc_Annual_moy[,c("P")] - annualMean[,c("P")]) > 50), FALSE)
})


biosimclient.config(forceClimateGenerationEnabled = T)
ClimaticQc_Annual <- generateWeather("ClimaticQc_Annual", 1981, 2010, locations$Name, locations$Latitude, locations$Longitude,
                                    locations$Elevation, rep=2, repModel=2)[["ClimaticQc_Annual"]]
test_that("Testing that we get 240 observations when simulating 2 locations x 30 years x 2rep x 2repModel", {
  expect_equal(length(ClimaticQc_Annual[,1]), 240)
})
biosimclient.config()
biosimclient.config(isLocalConnectionEnabled = biosimLocal, isTestModeEnabled = T)


modelList <- getModelList()

for (model in modelList) {
  a <- getModelDefaultParameters(model)
  test_that(paste("Testing that default parameters can be retrieved for", model), {
    expect_true(!is.null(a))
  })
}




shutdownClient()
