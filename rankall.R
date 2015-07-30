rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dataset <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
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
  ## For each state, find the hospital of the given rank
  states <- sort(unique(dataset$State))
  hospitals = character(0)
  for (i in seq_along(states)) {
    state <- states[[i]]
    state_dataset <- dataset[dataset$State == state, ]
    sorted_state_dataset <- state_dataset[order(as.numeric(state_dataset[[column_names[columns_indices[indices]]]]), state_dataset[["Hospital.Name"]], decreasing = FALSE, na.last = NA), ]
    expected_num <- num
    if (num == "best") {
      expected_num <- 1
    }
    else if (num == "worst") {
      expected_num <- which.max(as.double(sorted_state_dataset[, column_names[columns_indices[indices]]]))
    }
    hospitals[i] <- sorted_state_dataset[expected_num, "Hospital.Name"]
  }
  return(data.frame(hospital = hospitals, state = states, row.names = states))
  ## Return a data frame with the hospital names and the (abbreviated) state name
}