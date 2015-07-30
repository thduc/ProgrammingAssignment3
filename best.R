best <- function(state, outcome) {
  ## Read outcome data
  dataset <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!(state %in% dataset$State)) {
    stop("invalid state")
  }
  column_names <- names(dataset)
  # Columns:
  #   [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  #   [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  #   [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  columns_indices <- c(11, 17, 23)
  outcome_preprocessed <- gsub(" ", ".", outcome)
  indices <- grep(outcome_preprocessed, column_names[columns_indices], ignore.case = TRUE)
  if (length(indices) <= 0) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  state_dataset <- dataset[dataset$State == state, ]
  index <- which.min(as.double(state_dataset[, column_names[columns_indices[indices]]]))
  return(state_dataset[index, "Hospital.Name"])
}