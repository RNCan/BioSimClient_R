###################################
# Tests for the BioSimWebAPI patches
# Mathieu Fortin - Nov 2019
###################################


# id <- c("Quebec", "Sorel")
# latDeg <- c(46.87, 46.03)
# longDeg <- c(-71.25, -73.12)
# elevM <- c(114, 15)
# twoLocationsInSouthernQuebec <- data.frame(id, latDeg, longDeg, elevM)
# save(file = "./data/twoLocationsInSouthernQuebec.RData", twoLocationsInSouthernQuebec)

library(BioSIM)

biosimclient.config(isTestModeEnabled = T)

# MODELS_WITH_MISSING_INITIAL_YEAR <- c("BudBurst", "Climate_Moisture_Index_Annual", "Gypsy_Moth_Seasonality", "HemlockWoollyAdelgid_Annual",
#   "MPB_Cold_Tolerance_Annual", "Spruce_Budworm_Biology_Annual", "SpruceBeetle")
MODELS_WITH_MISSING_INITIAL_YEAR <- c("BudBurst", "Climate_Mosture_Index_Annual", "Gypsy_Moth_Seasonality", "MPB_Cold_Tolerance_Annual", "Spruce_Budworm_Biology_Annual", "SpruceBeetle")

# MODELS_REQUIRING_MORE_THAN_ONE_YEAR <- c("EmeraldAshBorerColdHardiness_Annual", "HemlockWoollyAdelgid_Daily", "MPB_Cold_Tolerance_Daily",
#   "MPB_SLR", "Standardised_Precipitation_Evapotranspiration_Index")
MODELS_REQUIRING_MORE_THAN_ONE_YEAR <- c("EmeraldAshBorerColdHardiness_Annual", "MPB_Cold_Tolerance_Daily", "MPB_SLR", "Standardised_Precipitation_Evapotranspiration_Index")


output <- generateWeather(c("DegreeDay_Annual",MODELS_WITH_MISSING_INITIAL_YEAR),
                          2010,
                          2015,
                          "Reservoir Gouin",
                          48.5,
                          -74.5,
                          NA)
for (d in output) {
  test_that("Testing that first year is 2010", {
    expect_equal(d[1,"Year"], 2010)
  })
}

output <- generateWeather(c("DegreeDay_Annual", MODELS_REQUIRING_MORE_THAN_ONE_YEAR),
                          1990,
                          1990,
                          "Reservoir Gouin",
                          48.5,
                          -74.5,
                          NA)

for (d in output) {
  test_that("Testing that first year is 2010", {
    expect_equal(d[1,"Year"], 1990)
  })
}


shutdownClient()
