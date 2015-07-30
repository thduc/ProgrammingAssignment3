rankhospital <- function(state, outcome, num = "best") {
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
  ## Return hospital name in that state with the given rank 30-day death rate
  state_dataset <- dataset[dataset$State == state, ]
  sorted_state_dataset <- state_dataset[order(as.numeric(state_dataset[[column_names[columns_indices[indices]]]]), state_dataset[["Hospital.Name"]], decreasing = FALSE, na.last = NA), ]
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst") {
    num <- which.max(as.double(sorted_state_dataset[, column_names[columns_indices[indices]]]))
  }
  return(sorted_state_dataset[num, "Hospital.Name"])
}