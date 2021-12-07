# Start
paired_permute <- function(x, y, nsim = 999, alternative = "two_sided"){
	
  # Take difference between x & y and then get the median difference
  diff <- x - y
  median_diff <- median(diff) 

  # Calculate number of pairs by taking length of diff
  n <- length(diff)
	
  # Create empty vector of length nsim for the permuted test scores
  exp_vect <- numeric(length(nsim))	

  # Loop nsim times
  # For each iteration, sample a vector of 1s and -1s of length n
  # Multiply the sign vector times the difference between x & y and then take the mean
  # Add each value to the empty vector

  for (i in 1:nsim){
    sign_vect <- sample(c(1, -1), n, replace = TRUE)
    rmedian_diff <- median(diff * sign_vect)
    exp_vect[i] <- rmedian_diff
  }

  # Add the observed value to the randomized values
  exp_vect <- append(exp_vect, rmedian_diff)

  # Calculate the P-value; evaluate left or right tail depending on sign of obs value
  if (median_diff >= 0){
    P_val <- sum(exp_vect >= median_diff) / length(exp_vect)
  } else if(median_diff < 0){
    P_val <- sum(exp_vect <= median_diff) / length(exp_vect)
  }

  if (alternative == "two_sided") {
    P_val <- 2 * P_val
  }

return(list(median_diff = median_diff, "P-value" = P_val))
}
# End
