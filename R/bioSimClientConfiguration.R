########################################################
# Client for BioSim
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: October 2019
########################################################


jarFilenames <- c("biosimclient-1.2.2.jar", "json-io-4.13.0.jar", "repicea-1.10.4.jar")


#'
#' The settings environment for this package
#'
#' This environment contains the general settings of the package.
#'
#' @export
settingEnv <- new.env()

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to BioSIM!")
  packageStartupMessage("The BioSIM package implements a client that retrieves climate variables from the")
  packageStartupMessage("original BioSIM application hosted on a server.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/RNCan/BioSimClient_R/wiki.")
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
  filename <- system.file(myJavaLibrary, package = packageName)
  if (file.exists(filename)) {
    filePath <- filename
  }
  else {
    filePath <- NULL
  }
  return(filePath)
}


.loadBioSIMClient <- function() {
  if (J4R::isConnectedToJava()) {
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems java is running but the class path does not contain this library: ", jarName, ". Shut down J4R using the shutdownClient function first and then re-run your code."))
      }
    }
  } else {
    path <- system.file(jarFilenames, package = "BioSIM", mustWork = T)
    J4R::connectToJava(extensionPath = path)
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems java has not been able to load the", jarName, "library."))
      }
    }
  }
}

connectToBioSIMClient <- function() {
  .loadBioSIMClient()
}


.isClientSupported <- function() {
  if (!exists("warningMessage", envir = settingEnv, inherits = F)) {  # means no prior connection to the server
#    .isClientSupported <- J4R::callJavaMethod("biosimclient.BioSimClient", "isClientSupported")
    warningMessage <- J4R::callJavaMethod("biosimclient.BioSimClient", "isClientSupported")
    assign("warningMessage", warningMessage, envir = settingEnv, inherits = F)
    if (warningMessage != "") {
      message(warningMessage)
    }
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
  if (exists("warningMessage", envir = settingEnv, inherits = F)) {  # means no prior connection to the server
    rm("warningMessage", envir = settingEnv, inherits = F)
  }
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
  if (exists("warningMessage", envir = settingEnv, inherits = F)) {  # means no prior connection to the server
    rm("warningMessage", envir = settingEnv, inherits = F)
  }
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
#' @param isLocalConnectionEnabled a logical (only for test purposes)
#' @param isTestModeEnabled a logical (only for test purpose)
#'
#' @export
biosimclient.config <- function(forceClimateGenerationEnabled = NULL,
                                nbNearestNeighbours = NULL,
                                isLocalConnectionEnabled = NULL,
                                isTestModeEnabled = NULL) {
  if (!is.null(forceClimateGenerationEnabled)) {
    if (!is.logical(forceClimateGenerationEnabled)) {
      stop("The forceClimateGenerationEnabled parameter must be a logical!")
    } else {
      connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setForceClimateGenerationEnabled", forceClimateGenerationEnabled)
    }
  }

  if (!is.null(nbNearestNeighbours)) {
    if (!is.numeric(nbNearestNeighbours)) {
      stop("The nbNearestNeighbours parameter must be an integer!")
    } else {
      connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setNbNearestNeighbours", as.integer(nbNearestNeighbours))
    }
  }

  if (!is.null(isLocalConnectionEnabled)) {
    if (!is.logical(isLocalConnectionEnabled)) {
      stop("The isLocalConnectionEnabled parameter must be a logical!")
    } else {
      connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setLocalConnectionEnabled", isLocalConnectionEnabled)
    }
  }

  if (!is.null(isTestModeEnabled)) {
    if (!is.logical(isTestModeEnabled)) {
      stop("The isTestModeEnabled parameter must be a logical!")
    } else {
      connectToBioSIMClient()
      J4R::callJavaMethod("biosimclient.BioSimClient", "setTestModeEnabled", isTestModeEnabled)
    }
  }



  if (is.null(forceClimateGenerationEnabled)
      && is.null(nbNearestNeighbours)
      && is.null(isLocalConnectionEnabled)
      && is.null(isTestModeEnabled)) {
    connectToBioSIMClient()
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
  connectToBioSIMClient()
  isForceClimateGenerationEnabled <- J4R::callJavaMethod("biosimclient.BioSimClient", "isForceClimateGenerationEnabled")
  nbNearestNeighbours <- J4R::callJavaMethod("biosimclient.BioSimClient", "getNbNearestNeighbours")
  isLocal <- J4R::callJavaMethod("biosimclient.BioSimClient", "isLocalConnectionEnabled")
  isTesting <- J4R::callJavaMethod("biosimclient.BioSimClient", "isTestModeEnabled")
  setting <- c("isForceClimateGenerationEnabled", "nbNearestNeighbours", "isLocalConnectionEnabled", "isTestModeEnabled")
  value <- c(as.character(isForceClimateGenerationEnabled),
             as.character(nbNearestNeighbours),
             as.character(isLocal),
             as.character(isTesting))
  return(data.frame(setting, value))
}

