########################################################
# Client for BioSim
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: October 2019
########################################################


bioSimFilename <- "biosimclient-1.1.jar"

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
  fieldNames <- J4R::getAllValuesFromListObject(dataSetObject$getFieldNames())
  for (i in 0:(length(fieldNames) - 1)) {
    values <- J4R::getAllValuesFromListObject(dataSetObject$getFieldValues(i))
    if (i == 0) {
      dataFrame <- data.frame(values)
      colnames(dataFrame) <- fieldNames[i + 1]
    } else {
      dataFrame[, fieldNames[i+1]] <- values
    }
  }
  return(dataFrame)
}

.getLibraryPath <- function(packageName, myJavaLibrary) {
  filename <- system.file("inst", myJavaLibrary, package = packageName)
  if (file.exists(filename)) {
    filePath <- filename
  }
  else {
    filename <- system.file(myJavaLibrary, package = packageName)
    if (file.exists(filename)) {
      filePath <- filename
    }
    else {
      filePath <- NULL
    }
  }
  return(filePath)
}


.loadBioSIMClient <- function() {
  path <- .getLibraryPath("BioSIM", bioSimFilename)
  J4R::connectToJava(extensionPath = path)
  if (!J4R::checkIfClasspathContains(bioSimFilename)) {
    stop("It seems J4R has not been able to load the biosim library.")
  }
}


.connectToBioSIMClient <- function() {
  if (!J4R::isConnectedToJava()) {
    .loadBioSIMClient()
  } else if (!J4R::checkIfClasspathContains(bioSimFilename)) {
    stop("Java is running but biosim is not part of the classpath! Please shutdown Java through the shutdownClient method first!")
  }
}

#'
#' Shut down the Java server
#'
#' This method overrides the original function of the J4R package. It only adds
#' a call to the clearCache function before calling the original function of
#' the J4R package.
#'
#' @examples
#' \dontrun{
#' shutdownJava()}
#'
#' @export
shutdownJava <- function() {
  J4R::shutdownJava()
}


#'
#' Shut down the Java server
#'
#' This method overrides the original function of the J4R package. It only adds
#' a call to the clearCache function before calling the original function of
#' the J4R package.
#'
#' @examples
#' \dontrun{
#' shutdownClient()}
#'
#' @export
shutdownClient <- function() {
  J4R::shutdownClient()
}

#'
#' Configure the client
#'
#' The forceClimateGenerationEnabled argument forces BioSIM to generate climate for past dates instead of
#' using the observations from the climate stations. By default this option is set to false. The
#' nbNearestNeighbours argument sets the number of stations for the imputation of climate variables.
#'
#' If an argument is set to null, there is no effect at all. If all the arguments are set to null, then
#' the configuration is reset to its default value: the climate variables of past dates relies on observations
#' and the number of climate stations is set to 4.
#'
#' @examples
#' \dontrun{
#' ### enables the climate generation for past dates and uses 20 climate stations
#' biosimclient.config(T, 20)
#'
#' ### reset the configuration
#' biosimclient.config() }
#'
#' @param forceClimateGenerationEnabled a logical
#' @param nbNearestNeighbours an integer
#'
#' @export
biosimclient.config <- function(forceClimateGenerationEnabled = NULL, nbNearestNeighbours = NULL) {
  if (!is.null(forceClimateGenerationEnabled)) {
    if (!is.logical(forceClimateGenerationEnabled)) {
      stop("The forceClimateGenerationEnabled parameter must be a logical!")
    } else {
      .connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setForceClimateGenerationEnabled", forceClimateGenerationEnabled)
    }
  }

  if (!is.null(nbNearestNeighbours)) {
    if (!is.numeric(nbNearestNeighbours)) {
      stop("The nbNearestNeighbours parameter must be an integer!")
    } else {
      .connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setNbNearestNeighbours", as.integer(nbNearestNeighbours))
    }
  }

  if (is.null(forceClimateGenerationEnabled) && is.null(nbNearestNeighbours)) {
    .connectToBioSIMClient()
    J4R::callJavaMethod("biosimclient.BioSimClient", "resetClientConfiguration")
    message("The configuration of the client has been reset to its default value!")
  }
}

#'
#' Report of the climate generation settings
#'
#' The isForceClimateGenerationEnabled setting forces BioSIM to generate climate for past dates
#' instead of using the observations from the climate stations. By default this option is set
#' to false. The nbNearestNeighbours setting is the number of stations used to impute climate variables to a
#' particular location.
#'
#' All the settings can be changed through the biosimclient.config function.
#'
#' @return a data.frame object
#'
#' @export
biosimclient.getConfiguration <- function() {
  .connectToBioSIMClient()
  isForceClimateGenerationEnabled <- J4R::callJavaMethod("biosimclient.BioSimClient", "isForceClimateGenerationEnabled")
  nbNearestNeighbours <- J4R::callJavaMethod("biosimclient.BioSimClient", "getNbNearestNeighbours")
  setting <- c("isForceClimateGenerationEnabled", "nbNearestNeighbours")
  value <- c(as.character(isForceClimateGenerationEnabled), as.character(nbNearestNeighbours))
  return(data.frame(setting, value))
}

