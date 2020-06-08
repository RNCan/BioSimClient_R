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

multiThreadingEnabled <- isMultithreadingEnabled()

test_that("Testing that multithreading is enabled by default", {
  expect_equal(multiThreadingEnabled, TRUE)
})

setMultithreadingEnabled(FALSE)
multiThreadingEnabled <- isMultithreadingEnabled()

test_that("Testing that multithreading has been disabled for the tests", {
  expect_equal(multiThreadingEnabled, FALSE)
})


locations <- BioSIM::twoLocationsInSouthernQuebec
print(locations)

variables <- c("TN","TX","P")

normals <- getAnnualNormals("1981_2010", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM, memSize = 500)

test_that("Testing that 1981-2010 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.1383562) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.331781) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1319) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 1.69589) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 11.37041) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 1033.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1971_2000", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1971-2000 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.44) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.129589) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1284.8) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 1.071507) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 11.06356) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 978.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1961_1990", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1961-1990 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.7452055) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.13726) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1235.3) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 0.6646575) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 10.9737) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 948.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1951_1980", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1951-1980 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.7167123) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.057808) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1200.1) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 0.6835616) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 10.91753) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 952.2) < 1E-4, TRUE)
})

summerMean <- getNormals("1981_2010", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM, c("June", "July", "August"))

test_that("Testing that 1981-2010 summer normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(summerMean[which(summerMean$id == "Quebec"),"TN"] - 12.52065) < 1E-4, TRUE)
  expect_equal(abs(summerMean[which(summerMean$id == "Quebec"),"TX"] - 23.38587) < 1E-4, TRUE)
  expect_equal(abs(summerMean[which(summerMean$id == "Quebec"),"P"] - 363.7) < 1E-4, TRUE)
  expect_equal(abs(summerMean[which(summerMean$id == "Sorel"),"TN"] - 14.74891) < 1E-4, TRUE)
  expect_equal(abs(summerMean[which(summerMean$id == "Sorel"),"TX"] - 25.24674) < 1E-4, TRUE)
  expect_equal(abs(summerMean[which(summerMean$id == "Sorel"),"P"] - 302.9) < 1E-4, TRUE)
})

normals <- getAnnualNormals("2061_2090", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM, rcp="RCP85", climModel="Hadley")
test_that("Testing that 2061-2090 annual normals under RCP 8.5 and climate model Hadley for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - 7.672329) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 16.32219) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1378.0) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 8.473699) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 17.89041) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 1159.2) < 1E-4, TRUE)
})


degreeDays <- getModelOutput(1994, 2002, locations$id, locations$latDeg, locations$longDeg, locations$elevM, "DegreeDay_Annual", F)

test_that("Testing degree-days between 1994 and 2004 for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1994),"DD"] - 2802.15) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1995),"DD"] - 2825.00) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1996),"DD"] - 2742.65) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1997),"DD"] - 2538.80) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1998),"DD"] - 2881.85) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 1999),"DD"] - 2946.05) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2000),"DD"] - 2667.60) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2001),"DD"] - 2913.65) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2002),"DD"] - 2697.20) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1994),"DD"] - 3312.80) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1995),"DD"] - 3382.15) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1996),"DD"] - 3258.50) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1997),"DD"] - 3069.95) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1998),"DD"] - 3525.50) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 1999),"DD"] - 3659.65) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2000),"DD"] - 3197.30) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2001),"DD"] - 3524.25) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2002),"DD"] - 3232.50) < 1E-4, TRUE)
})

growingSeason <- getModelOutput(1994, 2002, locations$id, locations$latDeg, locations$longDeg, locations$elevM, "GrowingSeason", F)

test_that("Testing growing season between 1994 and 2004 for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1994),"Length"] - 172) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1995),"Length"] - 187) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1996),"Length"] - 171) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1997),"Length"] - 172) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1998),"Length"] - 179) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 1999),"Length"] - 162) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 2000),"Length"] - 171) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 2001),"Length"] - 189) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Quebec" & growingSeason$Year == 2002),"Length"] - 169) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1994),"Length"] - 220) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1995),"Length"] - 205) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1996),"Length"] - 209) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1997),"Length"] - 184) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1998),"Length"] - 234) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 1999),"Length"] - 218) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 2000),"Length"] - 200) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 2001),"Length"] - 216) < 1E-4, TRUE)
  expect_equal(abs(growingSeason[which(growingSeason$id == "Sorel" & growingSeason$Year == 2002),"Length"] - 173) < 1E-4, TRUE)
})

degreeDays <- getModelOutput(2017, 2021, locations$id, locations$latDeg, locations$longDeg, locations$elevM, "DegreeDay_Annual", T, rcp = "RCP85", climModel = "GCM4")

test_that("Testing degree-days between 2017 and 2021 under RCP 8.5 and climate model GCM4 for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2017),"DD"] - 2840.05) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2018),"DD"] - 2740.50) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2019),"DD"] - 2703.95) < 1E-4, FALSE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2020),"DD"] - 3064.90) < 1E-4, FALSE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2021),"DD"] - 3216.00) < 1E-4, FALSE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2017),"DD"] - 3552.60) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2018),"DD"] - 3454.60) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2019),"DD"] - 3233.45) < 1E-4, FALSE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2020),"DD"] - 3361.55) < 1E-4, FALSE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2021),"DD"] - 3612.30) < 1E-4, FALSE)
})

degreeDays <- getModelOutput(2017, 2018, locations$id, locations$latDeg, locations$longDeg, locations$elevM, "DegreeDay_Annual", T, additionalParms = c("LowerThreshold"=5))

test_that("Testing degree-days above 5C in 2017 and 2018 can be properly retrieved", {
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2017),"DD"] - 1789.10) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Quebec" & degreeDays$Year == 2018),"DD"] - 1781.80) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2017),"DD"] - 2396.45) < 1E-4, TRUE)
  expect_equal(abs(degreeDays[which(degreeDays$id == "Sorel" & degreeDays$Year == 2018),"DD"] - 2390.85) < 1E-4, TRUE)
})

J4R::shutdownJava()



