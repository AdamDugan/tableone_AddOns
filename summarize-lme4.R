

#########################################
## A Function to Summarize lme4 Models ##
#########################################


# Model.Object = mod_reduced
# Variable.Summary.Dataframe = data.frame(Variable = c("Gender", "TimePoint"),
#                                         Label = c("Gender", "Time Point"),
#                                         stringsAsFactors = FALSE)
# Exponentiate.Estimates = TRUE
# Confidence.Level = 0.95
# Reference.Label = "ref."
# Digits_Estimate = 3
# Digits_Pvalue = 3
# Digits_SE = 3
# Digits_DF = 3
# Digits_Statistic = 3





lme4_table = function(Model.Object,
                      Variable.Summary.Dataframe = data.frame(Variable, Label, stringsAsFactors = FALSE),
                      Exponentiate.Estimates = FALSE,
                      Confidence.Level = 0.95,
                      Confidence.Mehtod = "profile",
                      Reference.Label = "ref.",
                      Digits_Estimate = 3,
                      Digits_Pvalue = 3,
                      Digits_SE = 3,
                      Digits_DF = 3,
                      Digits_Statistic = 3){
  
  
  
  
  ##############################
  ## Create the Model Summary ##
  ##############################
  
  ## Extract the model summary
  tmp_table = data.frame( summary(Model.Object)$coefficients, stringsAsFactors = FALSE )
  tmp_table = cbind("Parameter" = row.names(tmp_table), tmp_table)
  
  ## Name the columns
  if( class(Model.Object)[1] == "glmerMod" ){ names(tmp_table) = c("Parameter","Estimate","SE","Zvalue","Pvalue") }
  else if( class(Model.Object) %in% c("lmerModLmerTest","lmerMod") ){ names(tmp_table) = c("Parameter","Estimate","SE","DF","Tvalue","Pvalue") }
  else{ return("Not a lme4 model object???") }
  
  ## Calculate the confidence intervals
  tmp_ci = data.frame( confint(Model.Object,
                               level = Confidence.Level,
                               method = Confidence.Mehtod),
                       stringsAsFactors = FALSE)
  
  ## Format the CI table
  tmp_ci = cbind("Parameter" = row.names(tmp_ci), tmp_ci)
  names(tmp_ci) = c("Parameter","LowerCL","UpperCL")
  
  ## Exponentiate the values if requested
  if( Exponentiate.Estimates == TRUE ){
    tmp_table$Estimate = exp( tmp_table$Estimate )
    #tmp_table$SE = exp( tmp_table$SE )
    tmp_ci$LowerCL = exp( tmp_ci$LowerCL )
    tmp_ci$UpperCL = exp( tmp_ci$UpperCL )
  }
  
  ## Round the values
  tmp_table$Estimate = format(x = round(tmp_table$Estimate, Digits_Estimate), nsmall = Digits_Estimate)
  tmp_ci$LowerCL = format(x = round(tmp_ci$LowerCL, Digits_Estimate), nsmall = Digits_Estimate)
  tmp_ci$UpperCL = format(x = round(tmp_ci$UpperCL, Digits_Estimate), nsmall = Digits_Estimate)
  tmp_table$SE = format(x = round(tmp_table$SE, Digits_SE), nsmall = Digits_SE)
  tmp_table$Pvalue = format(x = round(tmp_table$Pvalue, Digits_Pvalue), nsmall = Digits_Pvalue)
  if( class(Model.Object)[1] == "glmerMod" ){ tmp_table$Zvalue = round(tmp_table$Zvalue, Digits_Statistic) }
  else if( class(Model.Object) %in% c("lmerModLmerTest","lmerMod") ){
    tmp_table$DF = format(x = round(tmp_table$DF, Digits_DF), nsmall = Digits_DF)
    tmp_table$Tvalue = format(x = round(tmp_table$Tvalue, Digits_Statistic), nsmall = Digits_Statistic) }
  else{ return("Not a lme4 model object???") }
  
  ## Handle p-value of 0
  tmp_table$Pvalue = as.character(tmp_table$Pvalue)
  for(i in 1:nrow(tmp_table)){
    tmp_zeros = paste0( rep("0", (Digits_Pvalue - 1)), collapse = "" )
    if( tmp_table$Pvalue[i] == "0" ){ tmp_table$Pvalue[i] = paste0("<0.", tmp_zeros, "1") }
  }
  rm(i, tmp_zeros)
  
  ## Combine the confidence limits
  tmp_ci$ConfidenceInterval = apply(X = tmp_ci[, c("LowerCL","UpperCL") ], MARGIN = 1,
                                    FUN = function(x){ return( paste0("(", x[1], ", ", x[2], ")") ) } )
  
  ## Merge the tables together
  tmp_table = merge(x = tmp_table,
                    y = tmp_ci[, c("Parameter","ConfidenceInterval")],
                    by = "Parameter",
                    all.x = TRUE,
                    all.y = FALSE,
                    sort = FALSE)
  
  ## Rearrange the columns
  if( class(Model.Object)[1] == "glmerMod" ){ 
    tmp_table = tmp_table[, c("Parameter","Estimate","SE","ConfidenceInterval","Zvalue","Pvalue") ] }
  else if( class(Model.Object) %in% c("lmerModLmerTest","lmerMod") ){
    tmp_table = tmp_table[, c("Parameter","Estimate","SE","ConfidenceInterval","DF","Tvalue","Pvalue") ] }
  else{ return("Not a lme4 model object???") }
  
  ## Remove R objects
  rm(tmp_ci)
  
  
  
  ################################
  ## Format the Variable Labels ##
  ################################
  
  ## Maybe later...
  
  
  
  ########################
  ## Return the Summary ##
  ########################
  
  return(tmp_table)
  
}







