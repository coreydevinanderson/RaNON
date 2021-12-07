lda_permute <- function(x, grouping, nsim = 999){
  
  fit_obs <- MASS::lda(x, grouping, CV = TRUE)
  obs_table <- table(grouping, fit_obs$class)
  obs_mean <- mean(diag(prop.table(obs_table, 1)))

  permuted_mean <- numeric(nsim)  	

  for(i in 1:nsim){
    permuted <- x[sample(nrow(x)),]  #permute the data
    fit_permuted <- MASS::lda(permuted, grouping, CV = TRUE)
    permuted_table <- table(grouping, fit_permuted$class)
    permuted_mean[i] <- mean(diag(prop.table(permuted_table, 1)))
  }

  permuted_mean <- append(permuted_mean, obs_mean)

  p_val <- sum(permuted_mean >= obs_mean) / length(permuted_mean)
  prior <- round(1 / length(diag(permuted_table)), 2)

return(list("observed_accuracy" = obs_mean, 
             "prior" = prior,
             "P-value" = p_val))
}

# Example call
lda_permute(x = iris[, c(1:3)], grouping = iris$Species)