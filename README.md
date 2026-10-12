# SeedlingSorteR

`SeedlingSorteR` is an R package for analyzing seedling root-to-shoot ratios, preprocessing seedling image data, and constructing deep learning models (via Keras/TensorFlow) to classify and predict plant genotypes.

---

## Installation

Install the development version directly from GitHub using `devtools`:

```r
# Install devtools if needed:
# install.packages("devtools")

devtools::install_github("wojahn/SeedlingSorteR")
```

---

## Quick Start Guide

### 1. Set Main Directory

Define the working directory containing your image sub-directories and dataset files:

```r
mainDirect <- "/Users/Mike/BSU_DRIVE/SeedlingSorteR_Analysis/"
```

---

### 2. Preliminary Exploratory Analysis

Analyze root-to-shoot ratios across genotype populations and treatment groups, saving boxplots to a PDF:

```r
dataset <- as.data.frame(read.csv("Images_seedlings_RootShoot.csv"))
tri <- as.data.frame(dataset[dataset\$Subsp == "tridentata2x", ])

pdf("boxplots.pdf")
boxplot(
  log(tri$Root_length / tri$Leaves_H) ~ tri$Genotype * tri$Treatment,
  ylab = "Root-to-Shoot Ratio", 
  xlab = "Genotypes by Treatment"
)
boxplot(
  log(tri\$Root_length / tri$Leaves_H) ~ tri$Genotype,
  ylab = "Root-to-Shoot Ratio", 
  xlab = "Genotypes"
)
dev.off()
```

---

### 3. Preprocess Images

Organize and resize seedling images into target directory structures required by Keras and TensorFlow:

```r
trainN <- 0.5
valN <- 1
scalewidth <- 600
scaleheight <- 300

# Preprocess subsetted and complete image sets
SeedlingSorteR::RootShootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)
SeedlingSorteR::ShootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)
SeedlingSorteR::RootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)

SeedlingSorteR::AllDataRootShootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)
SeedlingSorteR::AllDataShootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)
SeedlingSorteR::AllDataRootDataPreProcessoR(mainDirect, trainN, valN, scalewidth, scaleheight)
```

---

### 4. Build, Validate, and Train Neural Networks

Train image classification neural network models:

```r
# Build models trained on interquartile datasets
plotRS <- SeedlingSorteR::RootShootNeuralNetworkBuildeR(mainDirect)
plot(plotRS)

plotS <- SeedlingSorteR::ShootNeuralNetworkBuildeR(mainDirect)
plotR <- SeedlingSorteR::RootNeuralNetworkBuildeR(mainDirect)

# Build models trained on full datasets (unfiltered)
plotADRS <- SeedlingSorteR::AllDataRootShootNeuralNetworkBuildeR(mainDirect)
plotADS  <- SeedlingSorteR::AllDataShootNeuralNetworkBuildeR(mainDirect)
plotADR  <- SeedlingSorteR::AllDataRootNeuralNetworkBuildeR(mainDirect)
```

---

### 5. Predict Genotypes Across Full Datasets

Apply trained models to identify individuals that may have been incorrectly assigned to a population:

```r
# Root-Shoot Predictions
image_files_path <- sprintf("%sRSPredictionImages", mainDirect)
RootShootPredictions <- SeedlingSorteR::UseRootShootModel(mainDirect, image_files_path)
table(RootShootPredictions\$Original_Is)
write.csv(RootShootPredictions, sprintf("%sRootShootPredictions.csv", mainDirect), row.names = FALSE)

# Shoot Predictions
image_files_path <- sprintf("%sPredictionImages", mainDirect)
ShootPredictions <- SeedlingSorteR::UseShootModel(mainDirect, image_files_path)
table(ShootPredictions\$Original_Is)
write.csv(ShootPredictions, sprintf("%sShootPredictions.csv", mainDirect), row.names = FALSE)

# Root Predictions
image_files_path <- sprintf("%sRtPredictionImages", mainDirect)
RootPredictions <- SeedlingSorteR::UseRootModel(mainDirect, image_files_path)
table(RootPredictions\$Original_Is)
write.csv(RootPredictions, sprintf("%sRootPredictions.csv", mainDirect), row.names = FALSE)
```

---

### 6. Compare Model Performance

Evaluate prediction metrics across models (Root-Shoot, Shoot-only, Root-only):

```r
ModelComparisons <- SeedlingSorteR::RS_S_R_ComparatoR(
  RootShootPredictions, 
  ShootPredictions, 
  RootPredictions
)

write.csv(ModelComparisons, sprintf("%sRS_S_R_Comparison.csv", mainDirect), row.names = FALSE)
```
