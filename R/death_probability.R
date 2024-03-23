#' Calculate the probability of death for a new patient
#'
#' This function calculates the probability of death for a new patient based on logistic regression coefficients.
#'
#' @param new_patient A data frame containing information about the new patient
#' @return Probability of death for the new patient
#' @export
death_probability <- function(new_patient) {
  # Stabilize coefficients
  coefficients <- data.frame(
    `(Intercept)` = -4.223,
    `USMER` = -0.155,
    `PATIENT_TYPE` = 3.003,
    `PNEUMONIA` = -1.083,
    `AGE` = 0.710,
    `DIABETES` = -0.253,
    `HIPERTENSION` = -0.111,
    `RENAL_CHRONIC` = -0.343
  )

  # Calculate linear combination
  linear_combination <- coefficients["X.Intercept."] +
    coefficients["AGE"] * new_patient$AGE +
    coefficients["DIABETES"] * new_patient$DIABETES +
    coefficients["HIPERTENSION"] * new_patient$HIPERTENSION +
    coefficients["PNEUMONIA"] * new_patient$PNEUMONIA +
    coefficients["PATIENT_TYPE"] * new_patient$PATIENT_TYPE +
    coefficients["RENAL_CHRONIC"] * new_patient$RENAL_CHRONIC

  # Calculate probability of death
  probability_death <- 1 / (1 + exp(linear_combination))

  return(probability_death$X.Intercept.[1])
}

