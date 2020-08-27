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
  if (length(latDeg) == length(longDeg)) {
    if (length(latDeg) == length(elevM)) {
      jList <- J4R::createJavaObject("java.util.ArrayList")
      jPlots <- J4R::createJavaObject("biosimclient.BioSimPlotImpl", latDeg, longDeg, elevM)
      jList$add(jPlots)
      return(jList)
    }
  }
  stop("createBioSimPlots: The arguments of the function are not of the same length!")
}


.createVariableList <- function(variables) {
  myVariables <- J4R::createJavaObject("biosimclient.BioSimEnums$Variable", variables)
  jList <- J4R::createJavaObject("java.util.ArrayList")
  jList$add(myVariables)
  return(jList)
}


#'
#' Return the normals for a period
#'
#' If the argument averageOverTheseMonths is left NULL or empty, the monthly normals are provided. If
#' this argument is filled with some months, then the normal are aggregated over these months.
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
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
#' variables <- c("TN","TX","P")
#' \dontrun{
#' summerMean <- getNormals("1981_2010", variables, locations$Name, locations$Latitude,
#'                           locations$Longitude, locations$Elevation,
#'                           c("June", "July", "August"))}
#'
#' @export
getNormals <- function(period, variables, id, latDeg, longDeg, elevM, averageOverTheseMonths, rcp="RCP45", climModel = "RCM4") {
  # For debugging
  # period <- "1981_2010"
  # id <- locations$id
  # latDeg <- locations$latDeg
  # longDeg <- locations$longDeg
  # elevM <- locations$elevM
  if (length(id) != length(latDeg)) {
    stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  }
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
  jVariables <- .createVariableList(variables)
  jRCP <- J4R::createJavaObject("biosimclient.BioSimEnums$RCP", rcp)
  jClimModel <- J4R::createJavaObject("biosimclient.BioSimEnums$ClimateModel", climModel)

  maps <- J4R::callJavaMethod("biosimclient.BioSimClient", "getNormals", jPeriod, jVariables, jPlots, jRCP, jClimModel, jAverageOverTheseMonths)

  listOfPlots <- J4R::getAllValuesFromListObject(jPlots)

  outputDataFrame <- .formatDataFrame(listOfPlots, maps, id)

  return(outputDataFrame)
}


#'
#' Return the annual normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
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
#' variables <- c("TN","TX","P")
#' \dontrun{
#' annualNormals <- getAnnualNormals("1981_2010", variables, locations$Name, locations$Latitude,
#'                                      locations$Longitude, locations$Elevation)}
#'
#' @export
getAnnualNormals <- function(period, variables, id, latDeg, longDeg, elevM, rcp="RCP45", climModel = "RCM4") {
  return(getNormals(period, variables, id, latDeg, longDeg, elevM, BioSIM::allMonths, rcp, climModel))
}


#'
#' Return the monthly normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000", "1981_2010" up to "2071_2100")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
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
#' variables <- c("TN","TX","P")
#' \dontrun{
#' monthlyMeans <- getMonthlyNormals("1981_2010", variables, locations$Name, locations$Latitude,
#'                                    locations$Longitude, locations$Elevation)}
#'
#' @export
getMonthlyNormals <- function(period, variables, id, latDeg, longDeg, elevM, rcp="RCP45", climModel = "RCM4") {
  return(getNormals(period, variables, id, latDeg, longDeg, elevM, NULL, rcp, climModel))
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
  return(J4R::getAllValuesFromListObject(J4R::callJavaMethod("biosimclient.BioSimClient", "getModelList")))
}


.formatDataFrame <- function(listOfPlots, maps, id) {
  latDeg <- listOfPlots$getLatitudeDeg()
  longDeg <- listOfPlots$getLongitudeDeg()
  elevM <- listOfPlots$getElevationM()

  outputDataFrame <- NULL

  for (i in 1:length(listOfPlots)) {
    plot <- listOfPlots[[i]]
    if (maps$containsKey(plot) == F) {
      stop(paste("Plot", i, "is not in the map"))
    }
    data.i <- maps$get(plot)
    data.i <- .convertJavaDataSetIntoDataFrame(data.i)
    data.i$id <- id[i]
    data.i$latDeg <- latDeg[i]
    data.i$longDeg <- longDeg[i]
    data.i$elevM <- elevM[i]
    outputDataFrame <- rbind(outputDataFrame, data.i)
  }

  firstFields <- c("id", "latDeg", "longDeg", "elevM")
  fieldnames <- colnames(outputDataFrame)
  fieldnames <- c(firstFields, fieldnames[which(!(fieldnames %in% firstFields))])

  return(outputDataFrame[,fieldnames])
}


#'
#' Generate climate and apply a model.
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
#' computational burden and inconsistencies if different models are applied on the same locations.
#' @param rep number of replicates of generated climate (is set to 1 by default)
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
#'                              F, rcp = "RCP85", climModel = "GCM4", additionalParms = addParms)}
#'
#' @export
getModelOutput <- function(fromYr, toYr, id, latDeg, longDeg, elevM, modelName, isEphemeral, rep = 1, rcp = "RCP45", climModel = "RCM4", additionalParms = NULL) {
  # For debugging
  # fromYr <- 1998
  # toYr <- 2006
  # id <- locations$id
  # latDeg <- locations$latDeg
  # longDeg <- locations$longDeg
  # elevM <- locations$elevM
  # modelName <- "DegreeDay_Annual"
  # isEphemeral <- F
  .connectToBioSIMClient()
  if (length(id) != length(latDeg)) {
    stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  }
  if (!is.null(additionalParms)) {
    if (is.null(names(additionalParms))) {
      stop("The vector additionalParms must be a named vector!")
    } else if (length(names(additionalParms)) != length(unique(names(additionalParms)))) {
      stop("The names in the named vector must be unique!")
    }
  }
  jPlots <- .createBioSimPlots(latDeg, longDeg, elevM)
  jRCP <- J4R::createJavaObject("biosimclient.BioSimEnums$RCP", rcp)
  jClimModel <- J4R::createJavaObject("biosimclient.BioSimEnums$ClimateModel", climModel)
  if (is.null(additionalParms)) {
    jAdditionalParms <- J4R::createJavaObject("biosimclient.BioSimParameterMap", isNullObject = T)
  } else {
    jAdditionalParms <- J4R::createJavaObject("biosimclient.BioSimParameterMap")
    for (name in names(additionalParms)) {
      jAdditionalParms$addParameter(name, additionalParms[name])
    }
  }


  maps <- J4R::callJavaMethod("biosimclient.BioSimClient",
                      "getModelOutput",
                      as.integer(fromYr),
                      as.integer(toYr),
                      jPlots,
                      jRCP,
                      jClimModel,
                      modelName,
                      as.integer(rep),
                      isEphemeral,
                      jAdditionalParms)
  listOfPlots <- J4R::getAllValuesFromListObject(jPlots)

  mapSize <- maps$size()
  listSize <- jPlots$size()
  if (mapSize != listSize) {
    print(paste("The map has size =", mapSize, "while the list has size =", listSize))
  }

  outputDataFrame <- .formatDataFrame(listOfPlots, maps, id)

  return(outputDataFrame)
}


#'
#' Clear the cache of the client
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
  J4R::callJavaMethod("biosimclient.BioSimClient", "clearCache")
}
