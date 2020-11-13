#' Use Root and Shoot Model to Predict Image Genotypes
#' @param mainDirect character vector: main directory
#' @param image_files_path character vector: path to image(s)
#' @export
# Written by John M. A. Wojahn August 2020
# This is Free and Open-Source Software (F.O.S.S.)
# © J.M.A. Wojahn et al.
# Provided under the GNU AGPLv3 License
# Funded by EPSCoR GEM3 at Boise State University

UseRootShootModel <- function(mainDirect,image_files_path)
{
  require(keras)
  output_n <- 2
  # image size to scale down to (original images are 100 x 100 px)
  img_width <- 100
  img_height <- 100
  target_size <- c(img_width, img_height)
  G1image_files <- list.files(sprintf("%s/P1",image_files_path))
  G2image_files <- list.files(sprintf("%s/P2",image_files_path))
  image_names <- c(G1image_files,G2image_files)
  image_names <- gsub("_crop.JPG","",image_names)
  thismodel <- load_model_hdf5(sprintf("%sRootShootModel.h5",mainDirect), custom_objects = NULL, compile = TRUE)
  test_datagen <- image_data_generator(rescale = 1/255)

  test_generator <- flow_images_from_directory(
          image_files_path,
          test_datagen,
          target_size = target_size,
          class_mode = "categorical",
          classes = c("P1","P2"),
          batch_size = 1,
          shuffle = FALSE,
          seed = 42)
  require(tidyverse)
  classes <- test_generator$classes %>%
    factor() %>%
    table() %>%
    as.tibble()
  colnames(classes)[1] <- "value"
  indices <- test_generator$class_indices %>%
    as.data.frame() %>%
    gather() %>%
    mutate(value = as.character(value)) %>%
    left_join(classes, by = "value")

  test_generator$reset()
  predictions <- thismodel %>%
    predict_generator(
      generator = test_generator,
      steps = as.integer(test_generator$n)
      ) %>%
    round(digits = 2) %>%
    as.tibble()

  colnames(predictions) <- indices$key

  predictions <- predictions %>%
    mutate(truth_idx = as.character(test_generator$classes)) %>%
    left_join(indices, by = c("truth_idx" = "value"))

  predictions <- as.data.frame(predictions)
  colnames(predictions)[3] <- "Call"
  colnames(predictions)[5] <- "Original"
  for(i in 1:nrow(predictions))
  {
    if(predictions[i,1] > .5)
    {
      predictions[i,3] <- "P1"
    }else if(predictions[i,2] > .5){
      predictions[i,3] <- "P2"
    }else{
      predictions[i,3] <- "Indeterminate"
    }
    if(predictions[i,3] == predictions[i,4])
    {
      predictions[i,5] <- "Correct"
    }else{
      predictions[i,5] <- "Wrong"
    }
  }
  predictions <- cbind(image_names,predictions)
  colnames(predictions) <- c("Name","P1","P2","Call","OriginalCall","Original_Is")
  return(predictions)
}
