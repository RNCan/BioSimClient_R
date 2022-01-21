########################################################
# Client for BioSim
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: October 2019
########################################################


#'
#' A list of two plots located in southern Quebec
#'
#' @docType data
#'
#' @usage data(twoLocationsInSouthernQuebec)
#'
#' @keywords datasets
#'
#' @examples
#' data(twoLocationsInSouthernQuebec)
"twoLocationsInSouthernQuebec"

#'
#' The list of all months
#'
#' @export
allMonths <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

.createBioSimPlots <- function(latDeg, longDeg, elevM) {
#  if (length(latDeg) == length(longDeg)) {
#    if (length(latDeg) == length(elevM)) {
  jList <- J4R::createJavaObject("java.util.ArrayList")
  jPlots <- J4R::createJavaObject("biosimclient.BioSimPlotImpl", latDeg, longDeg, elevM)
  jList$add(jPlots)
  return(jList)
#    }
#  }
#  stop("createBioSimPlots: The arguments of the function are not of the same length!")
}


#'
#' Return the normals for a period
#'
#' If the argument averageOverTheseMonths is left NULL or empty, the monthly normals are provided. If
#' this argument is filled with some months, then the normal are aggregated over these months.
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#' @param averageOverTheseMonths a vector with some months if there is a need for aggregating the climate varibles
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' \dontrun{
#' summerMean <- getNormals("1981_2010", locations$Name, locations$Latitude,
#'                           locations$Longitude, locations$Elevation,
#'                           c("June", "July", "August"))}
#'
#' @export
getNormals <- function(period, id, latDeg, longDeg, elevM = rep(NA, length(longDeg)), averageOverTheseMonths, rcp="RCP45", climModel = "RCM4") {
  # For debugging
  # period <- "1981_2010"
  # id <- locations$id
  # latDeg <- locations$latDeg
  # longDeg <- locations$longDeg
  # elevM <- locations$elevM
  elevM <- .checkInputAndFormatIfNeeded(id, latDeg, longDeg, elevM)
  # if (length(id) != length(latDeg)) {
  #   stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  # }
  if (!is.null(averageOverTheseMonths) && length(averageOverTheseMonths) > 0) {
    for (month in averageOverTheseMonths) {
      if (!(month %in% allMonths)) {
        stop(paste("The month", month, "is not recognized!"))
      }
    }
  }

  .connectToBioSIMClient()
  jPlots <- .createBioSimPlots(latDeg, longDeg, elevM)

  jAverageOverTheseMonths <- J4R::createJavaObject("java.util.ArrayList")
  isSummarized <- F
  if (!is.null(averageOverTheseMonths) && length(averageOverTheseMonths) > 0) {
    jAverageOverTheseMonths$add(J4R::createJavaObject("biosimclient.BioSimEnums$Month", averageOverTheseMonths))
    isSummarized <- T
  }
  jPeriod <- J4R::createJavaObject("biosimclient.BioSimEnums$Period", paste("FromNormals", period, sep=""))
  jRCP <- J4R::createJavaObject("biosimclient.BioSimEnums$RCP", rcp)
  jClimModel <- J4R::createJavaObject("biosimclient.BioSimEnums$ClimateModel", climModel)

  map <- J4R::callJavaMethod("biosimclient.BioSimClient", "getNormals", jPeriod, jPlots, jRCP, jClimModel, jAverageOverTheseMonths)

  mapSize <- map$size()
  listSize <- jPlots$size()
  if (mapSize != listSize) {
    print(paste("The map has size =", mapSize, "while the list has size =", listSize))
  }

  outputBioSimDataSet <- J4R::callJavaMethod("biosimclient.BioSimDataSet", "convertLinkedHashMapToBioSimDataSet", map)
  outputDataFrame <- .convertJavaDataSetIntoDataFrame(outputBioSimDataSet)
  outputDataFrame <- .setKeyID(outputDataFrame, id)

  return(outputDataFrame)
}


#'
#' Return the annual normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' \dontrun{
#' annualNormals <- getAnnualNormals("1981_2010", locations$Name, locations$Latitude,
#'                                      locations$Longitude, locations$Elevation)}
#'
#' @export
getAnnualNormals <- function(period, id, latDeg, longDeg, elevM = rep(NA, length(longDeg)), rcp="RCP45", climModel = "RCM4") {
  return(getNormals(period, id, latDeg, longDeg, elevM, BioSIM::allMonths, rcp, climModel))
}


#'
#' Return the monthly normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' \dontrun{
#' monthlyMeans <- getMonthlyNormals("1981_2010", locations$Name, locations$Latitude,
#'                                    locations$Longitude, locations$Elevation)}
#'
#' @export
getMonthlyNormals <- function(period, id, latDeg, longDeg, elevM = rep(NA, length(longDeg)), rcp="RCP45", climModel = "RCM4") {
  return(getNormals(period, id, latDeg, longDeg, elevM, NULL, rcp, climModel))
}

#'
#' Return the list of models available in BioSim
#'
#' Provide the list of model that can be used in BioSIM after generating
#' the climate for some locations.
#'
#' @examples
#'
#' \dontrun{
#' getModelList()}
#'
#' @export
getModelList <- function() {
  .connectToBioSIMClient()
  jList <- J4R::callJavaMethod("biosimclient.BioSimClient", "getModelList")
  return(J4R::getAllValuesFromListObject(jList))
}

#'
#' Provide help for a particular model
#'
#' @param modelName should be one of the character string returned by the getModelList function
#'
#' @examples
#'
#' \dontrun{
#' getModelHelp("Spruce_Budworm_Biology")}
#'
#' @export
getModelHelp <- function(modelName) {
  .connectToBioSIMClient()
  return(cat(J4R::callJavaMethod("biosimclient.BioSimClient", "getModelHelp", modelName)))
}

#'
#' Provide help for a particular model
#'
#' @param modelName should be one of the character string returned by the getModelList function
#'
#' @examples
#'
#' \dontrun{
#' getModelHelp("Spruce_Budworm_Biology")}
#'
#' @export
getModelDefaultParameters <- function(modelName) {
  .connectToBioSIMClient()
  defParms <- J4R::callJavaMethod("biosimclient.BioSimClient", "getModelDefaultParameters", modelName)
  str <- defParms$toString()
  strSplit <- strsplit(str, "\\*")[[1]]
  keys <- c()
  values <- c()
  for (ch in strSplit) {
    strSubsplit <- strsplit(ch, ":")[[1]]
    keys <- c(keys, strSubsplit[1])
    if (length(strSubsplit) > 1) {
      value <- strSubsplit[2]
    } else {
      value <- ""
    }
    values <- c(values, value)
  }
  names(values) <- keys
  return(values)
}





.checkInputAndFormatIfNeeded <- function(id, latDeg, longDeg, elevM) {
  if (is.null(id) | is.null(latDeg) | is.null(longDeg) | is.null(elevM)) {
    stop("The arguments id, latDeg, longDeg and elevM must be non null!")
  }
  lengths <- c(length(id), length(latDeg), length(longDeg), length(elevM))
  if (any(lengths) == 0) {
    stop("The arguments id, latDeg, longDeg and elevM must have at least one element!")
  }
  if (any(lengths - mean(lengths) != 0)) {
    stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  }
  indexNa <- which(is.na(elevM))
  if (length(indexNa) > 0) {
    elevM[indexNa] <- NaN
  }
  classes <- c(class(latDeg), class(longDeg), class(elevM))
  if (any(classes != "numeric")) {
    stop("The arguments latDeg, longDeg and elevM must be numerics!")
  }
  return(elevM)
}

#'
#' Generate climate and apply a model (DEPRECATED).
#'
#' This function generated the basic climate variables for some locations
#' and applies a particular model on this generated climate.
#'
#' @param fromYr the starting date (yr) of the period (inclusive)
#' @param toYr the ending date (yr) of the period (inclusive)
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#' @param modelName a character. Should be one of the models listed in the available models (see the getModelList() method)
#' @param isEphemeral a logical. If set to true, the generated climate is not stored on the server, which implies a greater
#' computational burden and inconsistencies if different models are applied on the same locations. By default, it is set to
#' true.
#' @param rep number of replicates of generated climate (is set to 1 by default)
#' @param repModel number of replicates on the model end (is set to 1 by default)
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#' @param additionalParms a named vector with the additional parameters if needed
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' addParms <- c("LowerThreshold"=5)
#' \dontrun{
#' degreeDays <- getModelOutput(2017, 2021, locations$Name, locations$Latitude,
#'                              locations$Longitude, locations$Elevation, "DegreeDay_Annual",
#'                              rcp = "RCP85", climModel = "GCM4", additionalParms = addParms)}
#'
#' @export
getModelOutput <- function(fromYr, toYr, id, latDeg, longDeg, elevM = rep(NA, length(longDeg)), modelName, isEphemeral = T, rep = 1, repModel = 1, rcp = "RCP45", climModel = "RCM4", additionalParms = NULL) {
  .Deprecated(new = "generateModelOutput",
              msg = "Function getModelOutput has a signature that can cause trouble in some cases. For this reason, it is now deprecated. Please use the generateModelOutput function instead.")
  return(generateModelOutput(modelName, fromYr, toYr, id, latDeg, longDeg, elevM, isEphemeral, rep, repModel, rcp, climModel, additionalParms))
}


#'
#' Generate climate and apply a model (DEPRECATED).
#'
#' This function generated the basic climate variables for some locations
#' and applies a particular model on this generated climate.
#'
#' @param modelName a character. Should be one of the models listed in the available models (see the getModelList() method)
#' @param fromYr the starting date (yr) of the period (inclusive)
#' @param toYr the ending date (yr) of the period (inclusive)
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA or can be NULL, in which cases BioSim relies on a digital elevation model)
#' @param isEphemeral a logical. If set to true, the generated climate is not stored on the server, which implies a greater
#' computational burden and inconsistencies if different models are applied on the same locations. By default, it is set to
#' true.
#' @param rep number of replicates of generated climate (is set to 1 by default)
#' @param repModel number of replicates on the model end (is set to 1 by default)
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#' @param additionalParms a named vector with the additional parameters if needed
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' addParms <- c("LowerThreshold"=5)
#' \dontrun{
#' degreeDays <- generateModelOutput("DegreeDay_Annual", 2017, 2021, locations$Name, locations$Latitude,
#'                              locations$Longitude, locations$Elevation,
#'                              rcp = "RCP85", climModel = "GCM4", additionalParms = addParms)}
#'
#' @export
generateModelOutput <- function(modelName, fromYr, toYr, id, latDeg, longDeg,
                                elevM = rep(NA, length(longDeg)), isEphemeral = T, rep = 1,
                                repModel = 1, rcp = "RCP45", climModel = "RCM4", additionalParms = NULL) {
  .Deprecated(new = "generateModelOutput",
              msg = "Function generateModelOutput is deprecated. Please use the generateWeather function.")
}


.setKeyID <- function(outputDataFrame, id) {
  InnerKeyID <- 1:length(id)
  id.df <- data.frame(InnerKeyID, id)
  colnames(id.df)[2] <- "KeyID"
  outputDataFrameFinal <- merge(id.df, outputDataFrame, by ="InnerKeyID")
  if (nrow(outputDataFrameFinal) != nrow(outputDataFrame)) {
    stop("Some ids were apparently not merged into the outputDataFrame!")
  }
  return(outputDataFrameFinal[,-1])
}

#'
#' Clear the cache of the client (DEPRECATED).
#'
#' When using the weather generator, some objects are stored in memory on the server and
#' a reference is stored in the client, so that subsequent calls on models for the same
#' location and time interval does not have to generate the climate over and over again.
#' After a while it may happen that a large number of objects are kept in memory. This method
#' clears this cache on both the server and the client ends.
#'
#' @examples
#'
#' \dontrun{
#' clearCache()}
#'
#' @export
clearCache <- function() {
  .Deprecated(new = "clearCache",
              msg = "This function is now useless.")
  #  J4R::callJavaMethod("biosimclient.BioSimClient", "clearCache")
}



#'
#' Generate a meteorological time series and apply one or many models.
#'
#' This function generated a meteorological time series for some locations
#' and applies one or many models on this series.
#'
#' @param modelNames a character or a vector of character. Should be one or some models listed in the available models (see the getModelList() method)
#' @param fromYr the starting date (yr) of the period (inclusive)
#' @param toYr the ending date (yr) of the period (inclusive)
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA or can be NULL, in which cases BioSim relies on a digital elevation model)
#' @param rep number of replicates of generated climate (is set to 1 by default)
#' @param repModel number of replicates on the model end (is set to 1 by default)
#' @param rcp an representative concentration pathway (either "RCP45" or "RCP85")
#' @param climModel a climatic model (either "RCM4", "GCM4" or "Hadley")
#' @param additionalParms a list of named vectors with the additional parameters if needed
#'
#' @return a list of data.frame objects
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' addParms <- c("LowerThreshold"=5)
#' \dontrun{
#' degreeDays <- generateWeather("DegreeDay_Annual", 2017, 2021, locations$Name, locations$Latitude,
#'                    locations$Longitude, locations$Elevation,
#'                    rcp = "RCP85", climModel = "GCM4",
#'                    additionalParms = list(addParms))}
#'
#' @export
generateWeather <- function(modelNames, fromYr, toYr, id, latDeg, longDeg,
                            elevM = rep(NA, length(longDeg)), rep = 1,
                            repModel = 1, rcp = "RCP45", climModel = "RCM4",
                            additionalParms = NULL) {
#  For debugging
  # locations <- twoLocationsInSouthernQuebec
  # fromYr <- 1998
  # toYr <- 2006
  # id <- locations$id
  # latDeg <- locations$latDeg
  # longDeg <- locations$longDeg
  # elevM <- locations$elevM
  # modelNames <- "DegreeDay_Annual"
  # additionalParms <- NULL
  # rcp <- "RCP45"
  # climModel <- "RCM4"
  # rep <- 1
  # repModel <- 1
  # additionalParms <- NULL

  elevM <- .checkInputAndFormatIfNeeded(id, latDeg, longDeg, elevM)

  # if (length(id) != length(latDeg)) {
  #   stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  # }
  if (!is.null(additionalParms)) {
    if (is.list(additionalParms)) {
      for (item in additionalParms) {
        if (!is.null(item) && is.null(names(item))) {
          stop("The element of the additionalParms list instance must be named vectors!")
        }
      }
    } else {
      stop("The additionalParms argument must be a list of named vectors or NULL!")
    }
  }
  .connectToBioSIMClient()
  jPlots <- .createBioSimPlots(latDeg, longDeg, elevM)
  jRCP <- J4R::createJavaObject("biosimclient.BioSimEnums$RCP", rcp)
  jClimModel <- J4R::createJavaObject("biosimclient.BioSimEnums$ClimateModel", climModel)
  if (is.null(additionalParms)) {
    jAdditionalParmsList <- J4R::createJavaObject("java.util.ArrayList", isNullObject = T)
  } else {
    jAdditionalParmsList <- J4R::createJavaObject("java.util.ArrayList")
    for (item in additionalParms) {
      if (is.null(item)) {
        jAdditionalParmsList$add(J4R::createJavaObject("biosimclient.BioSimParameterMap", isNullObject = T))
      } else {
        jAdditionalParms <- J4R::createJavaObject("biosimclient.BioSimParameterMap")
        jAdditionalParmsList$add(jAdditionalParms)
        for (name in names(item)) {
          jAdditionalParms$addParameter(name, item[name])
        }
      }
    }
  }

  jModelList <- J4R::createJavaObject("java.util.ArrayList")
  jModelList$add(modelNames)

  map <- J4R::callJavaMethod("biosimclient.BioSimClient",
                             "generateWeather",
                             as.integer(fromYr),
                             as.integer(toYr),
                             jPlots,
                             jRCP,
                             jClimModel,
                             jModelList,
                             as.integer(rep),
                             as.integer(repModel),
                             jAdditionalParmsList)

  listSize <- jPlots$size()
  outputList <- list()
  for (i in 0:(jModelList$size() - 1)) {
    thisModelName <- jModelList$get(i)
    innerMap <- map$get(thisModelName)
    innerMapSize <- innerMap$size()
    if (innerMapSize != listSize) {
      stop(paste("The map has size =", mapSize, "while the list has size =", listSize))
    }
    outputBioSimDataSet <- J4R::callJavaMethod("biosimclient.BioSimDataSet", "convertLinkedHashMapToBioSimDataSet", innerMap)
    outputDataFrame <- .convertJavaDataSetIntoDataFrame(outputBioSimDataSet)
    outputDataFrame <- .setKeyID(outputDataFrame, id)
    outputList[[thisModelName]] <- outputDataFrame
  }
  return(outputList)
}


