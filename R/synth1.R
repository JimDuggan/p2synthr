library(dplyr)

#' \code{synth1} Disaggregates a time series by sub-groups. NA values return zeros.
#'
#' @param ts_data is the time series to be disaggregated
#' @param group_names the sub-group names
#' @param group_prob the probability of each sub-group appearing
#' @param setSeed allows setting of the seed for sampling
#' @param seedValue set the seed value to be used if needed
#' @return tibble with results, including original value. Each group has a column.
#' @export

synth1<- function(ts_data, group_names, group_prob, setSeed=F, seedValue=100){
  # Check input formats
  if(!is.atomic(ts_data))
    stop("Error, ts_data must be an atomic vector")
  else if(!is.numeric(ts_data))
           stop("Error, input ts_data must be numeric")
  else if(length(group_names) != length(group_prob))
          stop("Error, length of group_names and group_prob not equal")
  else if(sum(group_prob) != 1.0)
          stop("Error, sum of probabilities not equal to 1")
  
  if(setSeed)
    set.seed(seedValue)
  
  # Get the new columns to be added
  cols <- c("Input",group_names)
  
  # Create a list for each column, and include the original data
  d <- vector(mode="list",length=length(cols))
  
  # Loop through the list to allocate memory for the time series values
  for(i in seq_along(d)){
    d[[i]] <- vector("numeric",length(ts_data))
  }
  
  # Set the list names so they will be visible when the list is 
  # eventually cooerced to a tibble
  names(d) <- cols
  
  # Sub-divide each input time series value
  for(i in seq_along(ts_data)){
    # Store the input (original value) in the list element Input
    d[["Input"]][i] <- ts_data[i]
    
    # Sample the sub-groups according to the names and probabilities
    if(ts_data[i] > 0 & !is.na(ts_data[i])){
       sg <- sample(group_names,ts_data[i],group_prob,replace = TRUE)
    
       # Get the unique values from s (should be the same as group_names)
       us <- unique(sg)
    
       # Iterate through this and count the number of each
       for(j in us){
         d[[j]][i] <- sum(sg == j)
       }
    }
  }
  
  out_t <- tibble::as_tibble(d) |>
    mutate(Index=1:length(ts_data)) |>
    select(Index,everything())
  
  out_t
  
}