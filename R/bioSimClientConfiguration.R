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


.loadBioSIMClient <- function() {
  if (!J4R::checkIfClasspathContains("biosimclient.jar")) {
    J4R::addToClassPath("biosimclient.jar", packageName = "BioSIM")
  }
}


.connectToBioSIMClient <- function() {
  if (!J4R::isConnectedToJava()) {
    J4R::connectToJava()
  }
  .loadBioSIMClient()
}


#'
#' Checks whether or not the multi threading is enabled.
#'
#' The multi threading allows to send multiple requests simultaneously. It
#' is handled by the back-end Java client. This method returns true if
#' this option is enabled (which is the default value).
#'
#' @return a logical
#'
#' @export
isMultithreadingEnabled <- function() {
  .connectToBioSIMClient()
  return(J4R::callJavaMethod("biosimclient.BioSimClient", "isMultithreadingEnabled"))
}


#'
#' Enables or disables the multi threading.
#'
#' The multi threading allows to send multiple requests simultaneously. It
#' is handled by the back-end Java client. It is enabled by default.
#'
#' @param enabled a logical
#'
#' @export
setMultithreadingEnabled <- function(enabled) {
  .connectToBioSIMClient()
  J4R::callJavaMethod("biosimclient.BioSimClient", "setMultithreadingEnabled", enabled)
}

