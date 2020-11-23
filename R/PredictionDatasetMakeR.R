#' Preprocess Shoots Images in Preparation for Prediction
#' @param mainfile character: name of main image file folder (simple)
#' @param mainDirect character vector: main directory
#' @param scalewidth numeric: pixelwise width
#' @param scaleheight numeric: pixelwise height
#' @param type character: R, S, or RS
#' @export
# Written by John M. A. Wojahn November 2020
# This is Free and Open-Source Software (F.O.S.S.)
# © J.M.A. Wojahn et al.
# Provided under the GNU AGPLv3 License
# Funded by EPSCoR GEM3 at Boise State University

PredictionDatasetMakeR <- function(mainfile,mainDirect,scalewidth,scaleheight,type)
{
  print("Reading in dataset...")
  folderpath <- sprintf("%s%s",mainDirect,mainfile)
  dataset <- as.data.frame(read.csv("Images_seedlings_RootShoot.csv"))
  tri <- as.data.frame(dataset[dataset$Subsp =="tridentata2x",])
  #wyo <- as.data.frame(dataset[dataset$Subsp =="wyomingensis4x",])
  imagenames <- sprintf("%s_crop.JPG",tri$Image_ID)
  genes <- tri$Genotype
  P1s <- imagenames[genes == "P1"]
  P2s <- imagenames[genes == "P2"]
  system(sprintf("mkdir %s%sPredictionImages",mainDirect,type))
  system(sprintf("mkdir %s%sPredictionImages/P1",mainDirect,type))
  system(sprintf("mkdir %s%sPredictionImages/P2",mainDirect,type))
  print("...filling P1")
  pb <- txtProgressBar(min = 0, max = length(P1s), style = 3)
  for(i in 1:length(P1s))
  {
    setTxtProgressBar(pb,i)
    system(sprintf("cp '%s%s/%s' %s%s%s%s",mainDirect,mainfile,P1s[i],mainDirect,
                   type,"PredictionImages/P1/",P1s[i]))

  }
  print("...filling P2")
  pb <- txtProgressBar(min = 0, max = length(P2s), style = 3)
  for(i in 1:length(P2s))
  {
    setTxtProgressBar(pb,i)
    system(sprintf("cp '%s%s/%s' %s%s%s%s",mainDirect,mainfile,P2s[i],mainDirect,
                   type,"PredictionImages/P2/",P2s[i]))

  }
  #change image dimensions
  print("...commencing normalization of image dimensions...")
  #make new directories
  system(sprintf("mkdir %s%sResizedPredictionImages",mainDirect,type))
  system(sprintf("mkdir %s%sResizedPredictionImages/P1",mainDirect,type))
  system(sprintf("mkdir %s%sResizedPredictionImages/P2",mainDirect,type))
  LookLocale <- c(sprintf("%s%sPredictionImages/P1",mainDirect,type),
                  sprintf("%s%sPredictionImages/P2",mainDirect,type))
  WriteLocale <- c(sprintf("%s%sResizedPredictionImages/P1",mainDirect,type),
                  sprintf("%s%sResizedPredictionImages/P2",mainDirect,type))
  for(i in 1:length(LookLocale))
  {
    print(sprintf("......normalizing dimensions for files in %s",c("P1","P2")))
    #how many in folder
    allimages <- list.files(LookLocale[i])
    pb <- txtProgressBar(min = 0, max = length(allimages), style = 3)
    for(j in 1:length(allimages))
    {
      setTxtProgressBar(pb,j)
      thisimage <- EBImage::readImage(sprintf("%s/%s",LookLocale[i],allimages[j]))
      # scale to a specific width and height
      resizedimage <- EBImage::resize(thisimage, w = scalewidth, h = scaleheight)
      EBImage::writeImage(resizedimage,sprintf("%s/%s",WriteLocale[i],allimages[j]))
    }
  }
}
