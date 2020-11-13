#' Use Root and Shoot Model to Predict Image Genotypes
#' @param RootShootPredictions data.frame: output of UseRootShootModel
#' @param ShootPredictions data.frame: output of UseShootModel
#' @param RootPredictions data.frame: output of UseRootModel
#' @export
# Written by John M. A. Wojahn November 2020
# This is Free and Open-Source Software (F.O.S.S.)
# © J.M.A. Wojahn et al.
# Provided under the GNU AGPLv3 License
# Funded by EPSCoR GEM3 at Boise State University

RS_S_R_ComparatoR <- function(RootShootPredictions, ShootPredictions, RootPredictions)
{
  ComparisonOut <- as.data.frame(matrix(nrow=3, ncol=6))
  colnames(ComparisonOut) <- c("ModelType","R","S","RS","PropSameAsHo","PropInt")
  ComparisonOut[,1] <- c("R","S","RS")
  modeltypes <- c("R","S","RS")
  modelpreds <- list(RootPredictions,ShootPredictions,RootShootPredictions)
  Roottmp <- RootPredictions[order(RootPredictions$Name),]
  Roottmp <- Roottmp[,c(1,4)]
  Shoottmp <- ShootPredictions[order(ShootPredictions$Name),]
  Shoottmp <- Shoottmp[,c(1,4)]
  RootShoottmp <- RootShootPredictions[order(RootShootPredictions$Name),]
  RootShoottmp <- RootShoottmp[,c(1,4)]
  #first compare R-S
  foo <- Roottmp$Call == Shoottmp$Call
  det <- length(which(foo == T))/length(foo)
  ComparisonOut[1,3] <- det
  ComparisonOut[2,2] <- det
  #now compare R-RS
  foo <- Roottmp$Call == RootShoottmp$Call
  det <- length(which(foo == T))/length(foo)
  ComparisonOut[1,4] <- det
  ComparisonOut[3,2] <- det
  #now compare S-RS
  foo <- Shoottmp$Call == RootShoottmp$Call
  det <- length(which(foo == T))/length(foo)
  ComparisonOut[2,4] <- det
  ComparisonOut[3,3] <- det
  #now for sames
  ComparisonOut[1,2] <- 1
  ComparisonOut[2,3] <- 1
  ComparisonOut[3,4] <- 1
  #now do prop same as Ho
  ComparisonOut[1,5] <- length(which(RootPredictions$Original_Is == "Correct"))/length(RootPredictions$Original_Is)
  ComparisonOut[2,5] <- length(which(ShootPredictions$Original_Is == "Correct"))/length(ShootPredictions$Original_Is)
  ComparisonOut[3,5] <- length(which(RootShootPredictions$Original_Is == "Correct"))/length(RootShootPredictions$Original_Is)
  #now do prop same as Ho
  ComparisonOut[1,6] <- length(which(RootPredictions$Call == "INT"))/length(RootPredictions$Original_Is)
  ComparisonOut[2,6] <- length(which(ShootPredictions$Call == "INT"))/length(ShootPredictions$Original_Is)
  ComparisonOut[3,6] <- length(which(RootShootPredictions$Call == "INT"))/length(RootShootPredictions$Original_Is)
  return(ComparisonOut)
}
