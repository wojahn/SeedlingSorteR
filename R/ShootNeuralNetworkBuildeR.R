#' Makes Neural Network for Shoot Images
#' @param mainDirect character vector: main directory
#' @export
# Written by John M. A. Wojahn November 2020
# This is Free and Open-Source Software (F.O.S.S.)
# © J.M.A. Wojahn et al.
# Provided under the GNU AGPLv3 License
# Funded by EPSCoR GEM3 at Boise State University

ShootNeuralNetworkBuildeR <- function(mainDirect)
{
  library(keras)
  print("Preparing datasets to feed model")
  # number of output classes (i.e. fruits)
  output_n <- 2

  # image size to scale down to (original images are 100 x 100 px)
  img_width <- 100
  img_height <- 100
  target_size <- c(img_width, img_height)

  # RGB = 3 channels
  channels <- 3

  # path to image folders
  train_image_files_path <- sprintf("%s%s",mainDirect,"ResizedTrainingImages")
  valid_image_files_path <- sprintf("%s%s",mainDirect,"ResizedValidationImages")

  # optional data augmentation
  train_data_gen = image_data_generator(
    rescale = 1/255,
    rotation_range = 40,
  #  width_shift_range = 0.2,
  #  height_shift_range = 0.2,
  #  shear_range = 0.2,
  #  zoom_range = 0.2,
    horizontal_flip = TRUE,
  #  fill_mode = "nearest"
  )

  # Validation data shouldn't be augmented! But it should also be scaled.
  valid_data_gen <- image_data_generator(
    rescale = 1/255,
    )
  # training images
  train_image_array_gen <- flow_images_from_directory(train_image_files_path,
                                            train_data_gen,
                                            target_size = target_size,
                                            class_mode = "categorical",
                                            seed = 42)

  # validation images
  valid_image_array_gen <- flow_images_from_directory(valid_image_files_path,
                                            valid_data_gen,
                                            target_size = target_size,
                                            class_mode = "categorical",
                                            seed = 42)


  # number of training samples
  train_samples <- train_image_array_gen$n
  # number of validation samples
  valid_samples <- valid_image_array_gen$n

  # define batch size and number of epochs
  batch_size <- 13
  epochs <- 300

  print("Defining and compiling neural network...")
  # initialise model
  rm(model)
  # initialise model
  model <- keras_model_sequential()

  # add layers
  model %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
    layer_activation("relu") %>%

    # Second hidden layer
    layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
    layer_activation_leaky_relu(0.5) %>%
    layer_batch_normalization() %>%

    # Use max pooling
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.5) %>%

    # Flatten max filtered output into feature vector
    # and feed into dense layer
    layer_flatten() %>%
    layer_dense(100) %>%
    layer_activation("relu") %>%
    layer_dropout(0.27) %>%

    # Outputs from dense layer are projected onto output layer
    layer_dense(output_n) %>%
    layer_activation("softmax")

  # compile
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
    metrics = "accuracy"
  )
  print("Training neural network")

  # fit
  # fit
  hist <- model %>% fit_generator(
    # training data
    train_image_array_gen,

    # epochs
    steps_per_epoch = as.integer(train_samples / batch_size),
    epochs = epochs,

    # validation data
    validation_data = valid_image_array_gen,
    validation_steps = as.integer(valid_samples / batch_size),

    # print progress
    verbose = 2,
    callbacks = list(
      # save best model after every epoch
      callback_model_checkpoint("/Users/Mike/BSU_DRIVE/SeedlingSorteR/2checkpoints.h5", save_best_only = TRUE),
      # only needed for visualising with TensorBoard
      callback_tensorboard(log_dir = "/Users/Mike/BSU_DRIVE/SeedlingSorteR/logs")
    )
  )
  save_model_hdf5(model, "ShootModel.h5", overwrite = TRUE, include_optimizer = TRUE)
  return(hist)
}




