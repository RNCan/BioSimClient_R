########################################################
# Client for BioSim
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: October 2019
########################################################


.welcomeMessage <- function() {
  packageStartupMessage("Welcome to BioSIM!")
  packageStartupMessage("The BioSIM package implements a client that retrieves climate variable from the")
  packageStartupMessage("original BioSIM application hosted on a server. ")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/mrnfforesttools/biosimclient/wiki/Home/.")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}


.addToArray <- function(refArray, array) {
  if (length(refArray) != length(array)) {
    stop("Incompatible array length!")
  } else {
    for (i in 1:length(array)) {
      refArray[[i]] <- c(refArray[[i]], array[[i]])
    }
  }
  return(refArray)
}

.convertJavaDataSetIntoDataFrame <- function(dataSetObject) {
  refArray <- NULL
  observations <- J4R::callJavaMethod(dataSetObject, "getObservations")
  observations <- J4R::getAllValuesFromListObject(observations)
  for (obs in observations) {
    array <- J4R::callJavaMethod(obs, "toArray")
    array <- as.list(J4R::getAllValuesFromArray(array))
    if (is.null(refArray)) {
      refArray <- array
    } else {
      refArray <- .addToArray(refArray, array)
    }
  }
  dataFrame <- NULL
  for (i in 1:length(refArray)) {
    dataFrame <- as.data.frame(cbind(dataFrame, refArray[[i]]))
  }
  colnames(dataFrame) <- J4R::getAllValuesFromListObject(J4R::callJavaMethod(dataSetObject, "getFieldNames"))
  return(dataFrame)
}


.loadBioSIMClient <- function() {
  if (!J4R::checkIfClasspathContains("biosimclient.jar")) {
    J4R::addUrlToClassPath("biosimclient.jar", packageName = "BioSIM")
  }
}


.connectToBioSIMClient <- function() {
  if (!J4R::isConnectedToJava()) {
    J4R::connectToJava()
  }
  .loadBioSIMClient()
}



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
      J4R::callJavaMethod(jList, "add", jPlots)
      return(jList)
    }
  }
  stop("createBioSimPlots: The arguments of the function are not of the same length!")
}


.createVariableList <- function(variables) {
  myVariables <- J4R::createJavaObject("biosimclient.BioSimEnums$Variable", variables)
  jList <- J4R::createJavaObject("java.util.ArrayList")
  J4R::callJavaMethod(jList, "add", myVariables)
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
#' # summerMean <- getNormals("1981_2010", variables, locations$id, locations$latDeg,
#' #                        locations$longDeg, locations$elevM,
#' #                        c("June", "July", "August")) ## not run
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
    J4R::callJavaMethod(jAverageOverTheseMonths, "add", J4R::createJavaObject("biosimclient.BioSimEnums$Month", averageOverTheseMonths))
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
#' @export
getMonthlyNormals <- function(period, variables, id, latDeg, longDeg, elevM, rcp="RCP45", climModel = "RCM4") {
  return(getNormals(period, variables, id, latDeg, longDeg, elevM, NULL, rcp, climModel))
}

#'
#' Returns the list of models available in BioSim for post weather generation processing.
#'
#' @export
getModelList <- function() {
  .connectToBioSIMClient()
  return(J4R::getAllValuesFromListObject(J4R::callJavaMethod("biosimclient.BioSimClient", "getModelList")))
}


.formatDataFrame <- function(listOfPlots, maps, id) {
  latDeg <- J4R::callJavaMethod(listOfPlots, "getLatitudeDeg")
  longDeg <- J4R::callJavaMethod(listOfPlots, "getLongitudeDeg")
  elevM <- J4R::callJavaMethod(listOfPlots, "getElevationM")

  outputDataFrame <- NULL

  for (i in 1:length(listOfPlots)) {
    plot <- listOfPlots[[i]]
    if (J4R::callJavaMethod(maps, "containsKey", plot) == F) {
      stop(paste("Plot", i, "is not in the map"))
    }
    data.i <- J4R::callJavaMethod(maps,"get", plot)
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
#' Generates the climate for particular locations and applies a model on this generated climate.
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
#'
#' @return a data.frame object
#'
#' @examples
#'
#' locations <- BioSIM::twoLocationsInSouthernQuebec
#' # degreeDays <- getClimateVariables(2017, 2021, locations$id, locations$latDeg,
#' #                                locations$longDeg, locations$elevM, "DegreeDay_Annual",
#' #                                F, rcp = "RCP85", climModel = "GCM4") ## not run
#'
#'
#' @export
getClimateVariables <- function(fromYr, toYr, id, latDeg, longDeg, elevM, modelName, isEphemeral, rep = 1, rcp = "RCP45", climModel = "RCM4") {
  # For debugging
  # fromYr <- 1998
  # toYr <- 2006
  # id <- locations$id
  # latDeg <- locations$latDeg
  # longDeg <- locations$longDeg
  # elevM <- locations$elevM
  # modelName <- "DegreeDay_Annual"
  # isEphemeral <- F
  if (length(id) != length(latDeg)) {
    stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  }
  listOfModels <- getModelList()
  if (!(modelName %in% listOfModels)) {
    stop(paste("The model", modelName, "is not recognized by BioSim. Please see the list of available models. Call the getModelList() function."))
  }
  jPlots <- .createBioSimPlots(latDeg, longDeg, elevM)
  jRCP <- J4R::createJavaObject("biosimclient.BioSimEnums$RCP", rcp)
  jClimModel <- J4R::createJavaObject("biosimclient.BioSimEnums$ClimateModel", climModel)

  maps <- J4R::callJavaMethod("biosimclient.BioSimClient",
                      "getClimateVariables",
                      as.integer(fromYr),
                      as.integer(toYr),
                      jPlots,
                      jRCP,
                      jClimModel,
                      modelName,
                      as.integer(rep),
                      isEphemeral)
  listOfPlots <- J4R::getAllValuesFromListObject(jPlots)

  mapSize <- J4R::callJavaMethod(maps, "size")
  listSize <- J4R::callJavaMethod(jPlots, "size")
  if (mapSize != listSize) {
    print(paste("The map has size =", mapSize, "while the list has size =", listSize))
  }

  outputDataFrame <- .formatDataFrame(listOfPlots, maps, id)

  return(outputDataFrame)
}
