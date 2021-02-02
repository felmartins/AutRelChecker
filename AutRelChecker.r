if(!require(rel)){install.packages('rel')}
if(!require(irrCAC)){install.packages('irrCAC')}
if(!require(readxl)){install.packages('readxl')}
if(!require(irr)){install.packages('irr')}

library(rel)
library(irrCAC)
library(readxl)
library(irr)

AutRelChecker <- function(x, y, s.column = 1) {
  cat("### Automatic Reliability Checker for Nominal Data - FBSM ###\n")
  cat("### Version 1.0 - Highly unstable and under testing, comments refer to line(s) above ###\n")
  cat("### Code Revision C - Use github for updated version and changelog ###\n\n")
  Sys.sleep(4)
  cat("Starting Analysis...\n\n")
  reliabilityresults <- NULL
  ## Starts empty object
  for (i in s.column:length(x)) {
    #estextract <- NULL
  ### Loop for columns, starting at specific value in argument startcolumn, defailt is 1
    colA <- x[, i]
    colB <- y[, i]
  ### Select column in position based upon iterator i
    combinedmatrix <- as.matrix(cbind(colA, colB))
  ### Combine results in a matrix object
    GACresults <- gwet.ac1.raw(combinedmatrix)
  ### Obtain GWET AC1 from selected matrix
    kapparesult <- kappa2(combinedmatrix, weight = "equal")
  ###  Obtain Cohen Kappa from Matrix
    agreeresult <- t(agree(combinedmatrix))
  ### Obtain simple agreement from matrix, result is transposed based on default response from function agree()
    tempest <- GACresults[[1]]
    GWET <- tempest$coeff.val
    Cohen.K <- kapparesult$value
    PercAgree <- agreeresult[[5]]
    loopresults <- cbind(GWET, Cohen.K, PercAgree)
    reliabilityresults <- rbind(reliabilityresults, loopresults)
  ### Combine results into new object
    i =+ 1
  ### Increment iterator by one
  }
  names <- colnames(x)
  rownames(reliabilityresults) <- names[s.column:length(x)]
  reliabilityresults <- as.data.frame(reliabilityresults)
  write.csv(reliabilityresults, file = "AutRelChecker_AnalysisResults.csv")
  cat("### Analysis Complete - Print of Results or Saved Object to Follow ###\nData is also saved in a CSV file on your working directory\n\n")
  ### Add column names from x to resulting object rows
  return(reliabilityresults)
  ### Function return statement - object is a dataframe containing values for Gwet AC1, Cohen Kappa and Simple Agreement
}

AutRelChecker(data_Rater1, data_Rater2, s.column = 6)
