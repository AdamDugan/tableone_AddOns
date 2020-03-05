
library(nnet)
library(stringr)

# model_object = mod_6e
# variable_summary_dataframe = variable_summary
# confidence_level = 0.95
# digits_estimates = 3
# digits_pvalues = 3

summarize_multinomial <- function(model_object,
                                  variable_summary_dataframe = data.frame(Variable, Label, stringsAsFactors = FALSE),
                                  confidence_level = 0.95,
                                  digits_estimates = 3,
                                  digits_pvalues = 3){
  
  ## Extract the model estimates
  tmp_ests <- data.frame( summary(model_object)$coefficients )
  tmp_ses <- data.frame( summary(model_object)$standard.errors )
  
  ## Extract the outcome levels
  tmp_out <- row.names(tmp_ests)
  
  ## Create a data.frame for the combined results
  results <- data.frame(Final_Labels = NA,
                        Parameter = names(tmp_ests),
                        stringsAsFactors = FALSE)
  
  ## Add the other columns
  for(i in 1:length(tmp_out)){
    results[, paste0("Est_", tmp_out[i]) ] <- as.numeric( tmp_ests[ row.names(tmp_ests) == tmp_out[i], ] )
    results[, paste0("OR_", tmp_out[i]) ] <- format( round( exp( as.numeric( tmp_ests[ row.names(tmp_ests) == tmp_out[i], ] ) ), digits_estimates ), nsmall = digits_estimates )
    results[, paste0("SE_", tmp_out[i]) ] <- as.numeric( tmp_ses[ row.names(tmp_ses) == tmp_out[i], ] )
    results[, paste0("CI_", tmp_out[i]) ] <- NA
    results[, paste0("Pvalue_", tmp_out[i]) ] <- NA
  }
  rm(i)
  
  ## Calculate the p-values
  for(i in 1:length(tmp_out)){
    
    ## Calculate the z-statisticas
    z <- results[, paste0("Est_", tmp_out[i]) ] / results[, paste0("SE_", tmp_out[i]) ]
    
    ## Calcul[ate the p-values
    tmp_pvals <- format( round( (1-pnorm(abs(z), 0, 1)) * 2, digits = digits_pvalues), nsmall = digits_pvalues)
    
    ## Handle small p-values
    tmp_pvals[ tmp_pvals == paste0("0.", paste0(rep("0", digits_pvalues), collapse = "") ) ] <- paste0("<0.", paste0(rep("0", digits_pvalues - 1), collapse = ""), "1")
    
    ## Save the p-value
    results[, paste0("Pvalue_", tmp_out[i]) ] <- tmp_pvals
    
  }
  rm(i, z, tmp_pvals)
  
  ## Calculate the confidence intervals
  multiplier <- abs( qnorm(p = (1 - confidence_level) / 2) )
  for(k in 1:length(tmp_out)){
    
    ## Identify variable names
    tmp_est <- paste0("Est_", tmp_out[k])
    tmp_se <- paste0("SE_", tmp_out[k])
    tmp_ci <- paste0("CI_", tmp_out[k])
    
    for(i in 1:nrow(results)){
      
      ## Calculate the lower and upper bounds
      lb <- exp( results[i, tmp_est ] - (multiplier * results[i, tmp_se ]) )
      ub <- exp( results[i, tmp_est ] + (multiplier * results[i, tmp_se ]) )
      
      ## Round the values
      lb <- format( round(lb, digits_estimates), nsmall = digits_estimates )
      ub <- format( round(ub, digits_estimates), nsmall = digits_estimates )
      
      ## Save the CI
      results[i, tmp_ci] <- paste0("(", lb, ", ", ub, ")")
    }
  }
  rm(i, k, multiplier, tmp_est, tmp_se, tmp_ci, lb, ub)
  
  ## Identify the final labels
  results$Final_Labels[ results$Parameter == "X.Intercept." ] <- "Model intercept"
  for(i in 2:nrow(results)){
    
    ## Identify which label contains the current variable
    for(k in 1:nrow(variable_summary)){
      if( stringr::str_detect(string = results$Parameter[i], pattern = variable_summary$Variable[k]) ){ var_summ_row <- k }
    }
    rm(k)
    
    ## Update the variable label
    if( length(var_summ_row) > 0 ){ results$Final_Labels[i] <- variable_summary$Label[ var_summ_row ] }
    rm(var_summ_row)
  }
  rm(i)
  
  return(results)
  
}


