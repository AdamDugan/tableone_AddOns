



################################
## Descriptions of the Inputs ##
################################

# Model.Object = an lm() or glm() model oject. This hasn't been tested on other types of models (cox, lme, etc.).
# Variable.Summary.Dataframe = a two columned data.frame with one column containing variable names (as they would be in the model) and
#                              the other containing the label you'd like to display for that variable in the resulting table.
#                              The columns should be named "Variable" and "Label", respectively, and may contain variables not in the model.
#                              An example: data.frame(Variable = c("age","Gender_Male"), Label = c("Age, Years","Male"), stringsAsFactors = FALSE)
# Exponentiate.Estimates = TRUE/FALSE whether or not to take the exp() of extimates and confidence limits.
# Confidence.Level = a numeric value between 0 and 1 describing the confidence level to use for the confidence intervals.
# Subset.of.Predictors.to.Report = an optional vector of variable names to report in the table.
# Reference.Label = a character string for what should be included in the reference cells of the table for categorical variables with more than 2 levels.
# Digits_Estimate = a numeric value describing to what digit the beta estimates should be rounded.
# Digits_Pvalue = a numeric value describing to what digit the p-values should be rounded.
# Digits_SE = a numeric value describing to what digit the standard error estimates should be rounded.
# Digits_Statistic = a numeric value describing to what digit the statistics should be rounded.






#####################################################################
## Function to Format the Summary of a lm()/glm() Regression Model ##
#####################################################################


Regression.Summary = function(Model.Object,
                              Variable.Summary.Dataframe = data.frame(Variable, Label, stringsAsFactors = FALSE),
                              Exponentiate.Estimates = FALSE,
                              Confidence.Level = 0.95,
                              Subset.of.Predictors.to.Report = NULL,
                              Reference.Label = "ref.",
                              Digits_Estimate = 3,
                              Digits_Pvalue = 3,
                              Digits_SE = 3,
                              Digits_Statistic = 3){
  
  library(stringr)
  
  
  
  
  #######################################################
  ## Check the Inputs - Subset.of.Predictors.to.Report ##
  #######################################################
  
  ## Subset found in model
  if( length(Subset.of.Predictors.to.Report) > 0 ){
    
    ## Identify the variables in the model
    mod.vars = names( Model.Object$model )
    
    ## Remove the dataset from the name if it exists
    mod.vars = apply(X = matrix(mod.vars), MARGIN = 1,
                     FUN = function(x){
                       
                       ## Identify names
                       temp = unlist( str_split(string = x, pattern = fixed("$")) )
                       
                       ## Remove dataset from name
                       if( length(temp) > 1){ temp = temp[2] }
                       
                       ## Remove quotes if they exist
                       temp = str_remove_all(string = temp, pattern = fixed("`") )
                       
                       ## Return the results
                       return( temp ) } )
    
    ## Check if these names are found in the subset
    if( !(sum( Subset.of.Predictors.to.Report %in% mod.vars, na.rm = TRUE ) == length(Subset.of.Predictors.to.Report)) ){
      return( "At least 1 variable in Subset.of.Predictors.to.Report is not found in the model!" ) }
  }
  
  
  
  
  ################################
  ## Prep the Summary Dataframe ##
  ################################
  
  ## Extract the model summary
  coefs = summary(Model.Object)$coefficients
  coefs = data.frame( cbind(row.names(coefs), coefs), stringsAsFactors = FALSE )
  row.names(coefs) = NULL
  names(coefs) = c("Parameter","Estimate","SE","Statistic","P-value")
  
  ## Make the elements of the summary numeric
  coefs$Estimate = round( as.numeric(coefs$Estimate), Digits_Estimate)
  coefs$SE = round( as.numeric(coefs$SE), Digits_SE)
  coefs$Statistic = round( as.numeric(coefs$Statistic), Digits_Statistic)
  coefs$`P-value` = round( as.numeric(coefs$`P-value`), Digits_Pvalue)
  
  
  ## Should the estimates be exponentiated?
  if( Exponentiate.Estimates == TRUE ){
    
    ## Calculate the exponentiated estimates
    coefs$Estimate = round( exp( coefs$Estimate ), Digits_Estimate)
    cis = exp( round(confint(Model.Object), Digits_Estimate) )
    cis = data.frame( cbind(row.names(cis), cis), stringsAsFactors = FALSE )
    if( ncol(cis) == 3 ){
      names(cis) = c("Parameter","CL_Lower","CL_Upper")
    } else if( ncol(cis) == 2 ){
      cis <- cbind(c(Parameter = coefs$Parameter[1]), cis)
      names(cis) = c("Parameter","CL_Lower","CL_Upper")
    }
  }
  else{
    ## Calculate the confidence intervals - non-exponentiated
    cis = round( confint(Model.Object), Digits_Estimate)
    cis = data.frame( cbind(row.names(cis), cis), stringsAsFactors = FALSE )
    if( ncol(cis) == 3 ){
      names(cis) = c("Parameter","CL_Lower","CL_Upper")
    } else if( ncol(cis) == 2 ){
      cis <- cbind(c(Parameter = coefs$Parameter[1]), cis)
      names(cis) = c("Parameter","CL_Lower","CL_Upper")
    }
  }
  
  ## Bring over the CI's
  coefs = merge(x = coefs, y = cis, by = "Parameter", sort = FALSE)
  rm(cis)
  
  ## Make the elements of the summary numeric
  coefs$CL_Lower = round(as.numeric(coefs$CL_Lower), Digits_Estimate)
  coefs$CL_Upper = round(as.numeric(coefs$CL_Upper), Digits_Estimate)
  
  ## Combine the confidence limits
  coefs$`Confidence Limits` = apply(X = cbind(coefs$CL_Lower, coefs$CL_Upper), MARGIN = 1, 
                                    FUN = function(x){ return( paste0("(", x[1],", ", x[2], ")") ) } )
  
  
  
  
  ########################
  ## Edit the Row Names ##
  ########################
  
  ## Edit the intercept row
  coefs$Parameter[ coefs$Parameter == "(Intercept)" ] = "Model Intercept"
  
  ## Identify the variable name for each row
  coefs$Variable = unlist( apply(X = matrix(coefs$Parameter), MARGIN = 1,
                                 FUN = function(x){
                                   temp = str_extract(string = x, pattern = names( Model.Object$model ) )
                                   temp = temp[ !is.na(temp) ]
                                   if( length(temp) < 1 ){ temp = NA }
                                   else if( length(temp > 1) ){ temp = temp[ nchar(temp) == max(nchar(temp)) ] } ## max() may not always work...
                                   return( temp ) } ) )
  
  ## Identify the unique variables contained in the output
  temp.vars = coefs$Variable[ !duplicated(coefs$Variable) & !is.na(coefs$Variable) ]
  
  ## For variables with more than 1 row, add a sequence number
  coefs$Variable_Sequence = NA
  for(i in 1:length(temp.vars)){
    if( sum( coefs$Variable == temp.vars[i], na.rm = TRUE ) > 1 ){
      temp.rows = which( coefs$Variable == temp.vars[i] )
      coefs$Variable_Sequence[ temp.rows ] = 2:( length(temp.rows) + 1 )
    }
  }
  rm(i, temp.rows)
  
  ## Make all values character
  coefs = data.frame(lapply(coefs, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  
  ## For variables with more than 1 row, add an overall and reference row
  for(i in 1:length(temp.vars)){
    
    ## Does this variable have more than 1 row?
    if( sum( coefs$Variable == temp.vars[i], na.rm = TRUE) > 1 ){
      
      ## Which rows does this variable occupy?
      temp.rows = which( coefs$Variable == temp.vars[i] )
      
      ## Create the new rows to include
      temp.coefs = coefs[ temp.rows[1:2], ]
      temp.coefs[, names(temp.coefs)[ !names(temp.coefs) %in% c("Variable") ] ] = NA
      
      ## Edit the overall row
      temp.coefs$Variable_Sequence[1] = "0"
      
      ## Edit the reference row
      temp.coefs$Variable_Sequence[2] = "1"
      if( Exponentiate.Estimates == FALSE ){ temp.coefs$Estimate[2] = str_pad(string = "0.", width = (Digits_Estimate + 2), side = "right", pad = "0")}
      else{ temp.coefs$Estimate[2] = str_pad(string = "1.", width = (Digits_Estimate + 2), side = "right", pad = "0") }
      temp.coefs[ 2, !names(temp.coefs) %in% c("Estimate","Variable","Variable_Sequence") ] = Reference.Label
      
      ## Add to the overall dataframe
      if( min(temp.rows) == 1 ){ coefs = rbind(temp.coefs, coefs) }
      else{
        temp.coefs = rbind(coefs[1:(min(temp.rows) - 1), ], temp.coefs)
        coefs = rbind(temp.coefs, coefs[min(temp.rows):nrow(coefs),])
        }
    }
    
  }
  rm(i, temp.coefs, temp.rows, temp.vars)
  
  ## Bring over the variable labels
  coefs$Label = NA
  coefs$Label[ coefs$Parameter == "Model Intercept" ] = "Model Intercept"
  for(i in 1:nrow(coefs)){
    
    ## Is this not the intercept?
    if( !is.na(coefs$Variable[i]) ){
      
      ## Is this variable in the variable summary?
      if( sum(coefs$Variable[i] %in% Variable.Summary.Dataframe$Variable) > 0 ){
        coefs$Label[i] = Variable.Summary.Dataframe$Label[ Variable.Summary.Dataframe$Variable == coefs$Variable[i] ]
      }
      else{ return( paste0(coefs$Variable[i]," was not found in the Variable.Summary.Dataframe!") ) }
    }
  }
  rm(i)
  
  ## Extract the multilevel variables and labels
  temp.vars.list = Model.Object$xlevels
  temp.vars = names(temp.vars.list)
  
  ## Keep the variables with more than 2 levels
  temp.vars.length = apply(X = matrix(temp.vars), MARGIN = 1, FUN = function(x){ return( length( unlist( temp.vars.list[x]) ) ) } )
  temp.vars = temp.vars[ temp.vars.length > 2 ]
  temp.vars.list = temp.vars.list[ temp.vars.length > 2 ]
  rm(temp.vars.length)
  
  ## Bring over the levels
  coefs$Level = NA
  for(i in 1:length(temp.vars)){
    temp.levels = unlist( temp.vars.list[ temp.vars[i] ] )
    coefs$Level[ coefs$Variable_Sequence %in% 1:length(temp.levels) & coefs$Variable == temp.vars[i] ] = temp.levels
  }
  rm(i, temp.vars, temp.vars.list, temp.levels)
  
  ## Create final labels
  coefs$Final_Labels = apply(X = cbind(coefs$Variable_Sequence, coefs$Level, coefs$Label), MARGIN = 1,
                             FUN = function(x){
                               if( is.na(x[1]) | x[1] == "0" ){ return(x[3]) }
                               else{ return(x[2]) } } )
  
  
  
  
  ######################
  ## Edit the Results ##
  ######################
  
  ## Move the labels to the far left
  coefs = coefs[, c("Final_Labels", names(coefs)[ !names(coefs) == "Final_Labels" ]) ]
  
  ## Remove unnecessary columns
  coefs = coefs[, !names(coefs) %in% c("CL_Lower","CL_Upper","Variable_Sequence","Level") ]
  
  ## Rearrange columns
  coefs = coefs[, c("Final_Labels","Variable","Parameter","Estimate","SE",
                    names(coefs)[ !names(coefs) %in% c("Final_Labels","Variable","Parameter","Estimate","SE","Confidence Limits","P-value")],
                    "Confidence Limits","P-value") ]
  
  ## Add the percentage to the CL column name
  if( "Confidence Limits" %in% names(coefs)){
    names(coefs)[ names(coefs) == "Confidence Limits" ] = paste0(Confidence.Level*100, "% ", "Confidence Limits")
  }
  
  ## Edit the shortened p-values
  if( "P-value" %in% names(coefs) ){
    coefs$`P-value` = as.character( coefs$`P-value` )
    zeros = paste0( rep("0", (Digits_Pvalue - 1) ), collapse = "")
    for(i in 1:nrow(coefs)){
      if( !is.na(coefs$`P-value`[i]) & !coefs$`P-value`[i] == Reference.Label ){
        if( coefs$`P-value`[i] == "0" ){ coefs$`P-value`[i] = paste0("<0.", zeros, "1") }
        else if( nchar(coefs$`P-value`[i]) < (Digits_Pvalue + 2) ){
          coefs$`P-value`[i] = str_pad(string = coefs$`P-value`[i], width = (Digits_Pvalue + 2), side = "right", pad = "0") }
      }
    }
    rm(i, zeros)
  }
  
  ## Remove NA's
  coefs[ is.na(coefs) | coefs == "NULL" ] = ""
  
  ## Only report specific variables if requested
  if( length( Subset.of.Predictors.to.Report ) > 0 ){ coefs = coefs[ coefs$Variable %in% Subset.of.Predictors.to.Report, ] }
  
  
  
  
  ########################
  ## Return the Results ##
  ########################
  
  return( coefs )
}

