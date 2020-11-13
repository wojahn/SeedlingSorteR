#' Preprocess Shoots Images in Preparation for Machine Learning
#' @param mainDirect character vector: main directory
#' @param trainN numeric: proportion of images to select for training (<1)
#' @param valN numeric: proportion of images to select for validation (<1)
#' @param scalewidth numeric: pixelwise width
#' @param scaleheight numeric: pixelwise height
#' @export
# Written by John M. A. Wojahn August 2020
# This is Free and Open-Source Software (F.O.S.S.)
# © J.M.A. Wojahn et al.
# Provided under the GNU AGPLv3 License
# Funded by EPSCoR GEM3 at Boise State University

ShootDataPreProcessoR <-function(mainDirect,trainN,valN,scalewidth,scaleheight)
{
  print("Reading in dataset...")
  #read in dataset information
  dataset <- as.data.frame(read.csv("Images_seedlings_RootShoot.csv"))
  print("Extracting only quartile data")
  tri <- as.data.frame(dataset[dataset$Subsp =="tridentata2x",])
  Genos <- c("P1","P2")
  Treats <- c("T1","T2","T3")
  thechosen <- as.data.frame(matrix(nrow=0,ncol=ncol(tri)))
  colnames(thechosen) <- colnames(tri)
  for(i in 1:length(Treats))
  {
    print(i)
    halfthese <- tri[tri$Treatment == Treats[i],]
    for(j in 1:length(Genos))
    {
      these <- halfthese[halfthese$Genotype == Genos[j],]
      thesenumz <- log(these$Root_length/these$Leaves_H)
      sumstatz <- summary(thesenumz)
      foo <- as.data.frame(these[thesenumz >= sumstatz[2],])
      fooznumz <- log(foo$Root_length/foo$Leaves_H)
      theones <- as.data.frame(foo[fooznumz <= sumstatz[5],])
      thechosen <- rbind(thechosen,theones)
    }
  }
  rownames(thechosen) <- NULL
  dataset <- thechosen
  #dataset <- as.data.frame(dataset[dataset$Treatment == "T2",])
  print("Looking at what files exist...")
  #see what files exist
  ExistantFiles <- list.files(sprintf("%sShootsOnlyPix",mainDirect))
  print("Cleaning filenames...")
  tmp <- ExistantFiles[gsub("_crop.*","",ExistantFiles) %in% dataset$Image_ID]
  ExistantFiles <- ExistantFiles[gsub("_crop.*","",ExistantFiles) %in% dataset$Image_ID]
  print("Making training dataset...")
  #set seed for reproducibility
  set.seed(1993)
  print(sprintf("...randomly sampling %s...",trainN))
  #ExistantFilesCorrectedTiff <- RealExistantFilesCorrected[grep("tif",RealExistantFilesCorrected)]
  TrainingIDs <- gsub("_crop.*","",ExistantFiles)
  TrainingClasses <- c(1:length(ExistantFiles))
  print("......inferring classes...")
  #now infer classes for the training and validation data
  pb <- txtProgressBar(min = 0, max = length(ExistantFiles), style = 3)
  for(i in 1:length(TrainingIDs))
  {
    setTxtProgressBar(pb,i)
    TrainingClasses[i] <- as.character(dataset[as.character(dataset$Image_ID) == as.character(TrainingIDs[i]),10])
  }
  #randomly sample 25% for training
  Cyto1 <- ExistantFiles[TrainingClasses == "P1"]
  Cyto1TrainingClass <- TrainingClasses[TrainingClasses == "P1"]
  Cyto2 <- ExistantFiles[TrainingClasses == "P2"]
  Cyto2TrainingClass <- TrainingClasses[TrainingClasses == "P2"]
  Cyto1Sample <-sample(1:length(Cyto1),ceiling(trainN*length(Cyto1)))
  Cyto2Sample <-sample(1:length(Cyto2),ceiling(trainN*length(Cyto2)))
  TrainingNumsCyto1 <- Cyto1[Cyto1Sample]
  TrainingNumsCyto2 <- Cyto2[Cyto2Sample]
  SampledTrainingClassCyto1 <- Cyto1TrainingClass[Cyto1Sample]
  SampledTrainingClassCyto2 <- Cyto2TrainingClass[Cyto2Sample]
  rm(TrainingClasses)
  TrainingClasses <- as.vector(c(SampledTrainingClassCyto1,SampledTrainingClassCyto2))
  #put those files into their own folder
  TrainingData <- as.vector(c(TrainingNumsCyto1,TrainingNumsCyto2))
  #now fill up directories
  print("...creating dataset...")
  pb <- txtProgressBar(min = 0, max = length(TrainingData), style = 3)
  for(i in 1:length(TrainingData))
  {
    setTxtProgressBar(pb,i)
    if(i==1)
    {
      command <- sprintf("mkdir %sTrainingImages",mainDirect)
      system(command)
      command <- sprintf("mkdir %sTrainingImages/P1",mainDirect)
      system(command)
      command <- sprintf("mkdir %sTrainingImages/P2",mainDirect)
      system(command)
    }
    if(TrainingClasses[i] == "P1")
    {
      command <- sprintf("cp '%s%s%s' %s%s%s",mainDirect,"ShootsOnlyPix/",TrainingData[i],mainDirect,
                       "TrainingImages/P1/",TrainingData[i])
    }else{
      command <- sprintf("cp '%s%s%s' %s%s%s",mainDirect,"ShootsOnlyPix/",TrainingData[i],mainDirect,
                       "TrainingImages/P2/",TrainingData[i])
    }
      system(command)
  }
  print("Making validation dataset...")
  ExistantFilesNoTrain <- ExistantFiles[!ExistantFiles %in% TrainingData]
  #set seed for reproducibility
  set.seed(1993)
  print(sprintf("...randomly sampling %s...",valN))
  ValidationIDs <- gsub("_crop.*","",ExistantFilesNoTrain)
  ValidatingClasses <- c(1:length(ExistantFilesNoTrain))
  print("......inferring classes...")
  #now infer classes for the training and validation data
  pb <- txtProgressBar(min = 0, max = length(ExistantFilesNoTrain), style = 3)
  for(i in 1:length(ValidationIDs))
  {
    setTxtProgressBar(pb,i)
    ValidatingClasses[i] <- as.character(dataset[as.character(dataset$Image_ID) == as.character(ValidationIDs[i]),10])
  }
  #randomly sample 25% for training
  Cyto1 <- ExistantFilesNoTrain[ValidatingClasses == "P1"]
  Cyto1ValidatingClass <- ValidatingClasses[ValidatingClasses == "P1"]
  Cyto2 <- ExistantFilesNoTrain[ValidatingClasses == "P2"]
  Cyto2ValidatingClass <- ValidatingClasses[ValidatingClasses == "P2"]
  Cyto1Sample <-sample(1:length(Cyto1),ceiling(trainN*length(Cyto1)))
  Cyto2Sample <-sample(1:length(Cyto2),ceiling(trainN*length(Cyto2)))
  ValidatingNumsCyto1 <- Cyto1[Cyto1Sample]
  ValidatingNumsCyto2 <- Cyto2[Cyto2Sample]
  SampledValidatingClassCyto1 <- Cyto1ValidatingClass[Cyto1Sample]
  SampledValidatingClassCyto2 <- Cyto2ValidatingClass[Cyto2Sample]
  rm(ValidatingClasses)
  ValidatingClasses <- as.vector(c(SampledValidatingClassCyto1,SampledValidatingClassCyto2))
  #put those files into their own folder
  rm(ValidationData)
  ValidationData <- as.vector(c(ValidatingNumsCyto1,ValidatingNumsCyto2))
  #now fill directories
  print("...creating dataset...")
  pb <- txtProgressBar(min = 0, max = length(ValidationData), style = 3)
  for(i in 1:length(ValidationData))
  {
    setTxtProgressBar(pb,i)
    if(i==1)
    {
      command <- sprintf("mkdir %sValidationImages",mainDirect)
      system(command)
      command <- sprintf("mkdir %sValidationImages/P1",mainDirect)
      system(command)
      command <- sprintf("mkdir %sValidationImages/P2",mainDirect)
      system(command)
    }
    if(ValidatingClasses[i] == "P1")
    {
      command <- sprintf("cp '%s%s%s' %s%s%s",mainDirect,"ShootsOnlyPix/",ValidationData[i],mainDirect,
                       "ValidationImages/P1/",ValidationData[i])
    }else{
      command <- sprintf("cp '%s%s%s' %s%s%s",mainDirect,"ShootsOnlyPix/",ValidationData[i],mainDirect,
                       "ValidationImages/P2/",ValidationData[i])
    }
      system(command)
  }
  #change image dimensions
  print("Commencing normalization of image dimensions...")
  #make new directories
  command <- sprintf("mkdir %sResizedValidationImages",mainDirect)
  system(command)
  command <- sprintf("mkdir %sResizedValidationImages/P1",mainDirect)
  system(command)
  command <- sprintf("mkdir %sResizedValidationImages/P2",mainDirect)
  system(command)
  command <- sprintf("mkdir %sResizedTrainingImages",mainDirect)
  system(command)
  command <- sprintf("mkdir %sResizedTrainingImages/P1",mainDirect)
  system(command)
  command <- sprintf("mkdir %sResizedTrainingImages/P2",mainDirect)
  system(command)
  LookLocale <- c(sprintf("%sValidationImages/P1",mainDirect),
                  sprintf("%sValidationImages/P2",mainDirect),
                  sprintf("%sTrainingImages/P1",mainDirect),
                  sprintf("%sTrainingImages/P2",mainDirect))
  WriteLocale <- c(sprintf("%sResizedValidationImages/P1",mainDirect),
                  sprintf("%sResizedValidationImages/P2",mainDirect),
                  sprintf("%sResizedTrainingImages/P1",mainDirect),
                  sprintf("%sResizedTrainingImages/P2",mainDirect))
  for(i in 1:length(LookLocale))
  {
    print(sprintf("Normalizing dimensions for files in %s",LookLocale[i]))
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

