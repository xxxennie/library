#' Generate a new patient based on the provided training data
#'
#' This function generates a new patient based on the distribution of values in the training data.
#'
#' @return A data frame containing the new patient information
#' @export
generate_patient <- function() {
  # Load the training data
 # file_path <- "/Users/xxennie/iconnect/data/train.csv"
  file_path <- system.file("data", "train.csv", package = "iconnect")

  train <- read.csv(file_path)
  train_for_new <- train[, 1:7]
  probabilities <- lapply(train_for_new, function(col) {
    prop.table(table(col))
  })

  new_patient <- lapply(seq_along(train_for_new), function(i) {
    column_values <- names(probabilities[[i]])
    column_probabilities <- probabilities[[i]]

    new_value <- sample(column_values, 1, prob = column_probabilities)

    return(new_value)
  })

  new_patient <- as.data.frame(t(new_patient))
  colnames(new_patient) <- colnames(train_for_new)

  new_patient$USMER <- as.integer(new_patient$USMER)
  new_patient$PATIENT_TYPE <- as.integer(new_patient$PATIENT_TYPE)
  new_patient$PNEUMONIA <- as.integer(new_patient$PNEUMONIA)
  new_patient$AGE <- as.numeric(new_patient$AGE)
  new_patient$DIABETES <- as.integer(new_patient$DIABETES)
  new_patient$HIPERTENSION <- as.integer(new_patient$HIPERTENSION)
  new_patient$RENAL_CHRONIC <- as.integer(new_patient$RENAL_CHRONIC)
  new_patient$AGE <- round(new_patient$AGE, digits = 4)

  return(new_patient)
}

