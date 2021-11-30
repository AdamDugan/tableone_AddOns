
## A description of the function's arguments:
## dataset: the filtered and merged NSQIP data frame outputted by the "nsqip_subset_pufs" function

## The function
nsqip_clean_data <- function(dataset = dat){
  
  
  
  ###########################
  ## Handle Missing Values ##
  ###########################
  
  ## Replacing Null's with NA's
  dataset[ dataset == "NULL" ] <- NA
  
  ## Replacing -99's with NA's
  dataset[ dataset == -99 ] <- NA
  
  ## Replacing "Unknown" with NA
  dataset[ dataset == "Unknown" ] <- NA
  
  ## Replacing "Unknown" with NA
  dataset[ dataset == "None assigned" ] <- NA
  
  
  
  ############################
  ## Dichotomize Lab Values ##
  ############################
  
  ## Pre-Operative Albumin < 3
  dataset$pralbum_lt3 <- factor(x = as.numeric(dataset$pralbum < 3),
                                levels = c(0, 1),
                                labels = c("No","Yes"))
  
  ## Pre-Operative Alkaline Phosphatase > 125
  dataset$pralkph_gt125 <- factor(x = as.numeric(dataset$pralkph > 125),
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))
  
  ## Pre-Operative Bilirubin > 1
  dataset$prbili_gt1 <- factor(x = as.numeric(dataset$prbili > 1),
                               levels = c(0, 1),
                               labels = c("No","Yes"))
  
  ## Pre-Operative BUN > 40
  dataset$prbun_gt40 <- factor(x = as.numeric(dataset$prbun > 40),
                               levels = c(0, 1),
                               labels = c("No","Yes"))
  
  ## Pre-Operative Creatinine > 1.2
  dataset$prcreat_gt1p2 <- factor(x = as.numeric(dataset$prcreat > 1.2),
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))
  
  ## Pre-Operative Hematocrit < 30%
  dataset$prhct_lt30 <- factor(x = as.numeric(dataset$prhct < 30),
                               levels = c(0, 1),
                               labels = c("No","Yes"))
  
  ## Pre-Operative Hematocrit > 45%
  dataset$prhct_gt45 <- factor(x = as.numeric(dataset$prhct > 45),
                               levels = c(0, 1),
                               labels = c("No","Yes"))
  
  ## Pre-Operative Platelets < 150,000
  dataset$prplate_lt150 <- factor(x = as.numeric(dataset$prplate < 150),
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))
  
  ## Pre-Operative Platelets > 400,000
  dataset$prplate_gt400 <- factor(x = as.numeric(dataset$prplate > 400),
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))
  
  ## Pre-Operative SGOT > 40
  dataset$prsgot_gt40 <- factor(x = as.numeric(dataset$prsgot > 40),
                                levels = c(0, 1),
                                labels = c("No","Yes"))
  
  ## Pre-Operative Sodium < 135
  dataset$prsodm_lt135 <- factor(x = as.numeric(dataset$prsodm < 135),
                                 levels = c(0, 1),
                                 labels = c("No","Yes"))
  
  ## Pre-Operative Sodium > 145
  dataset$prsodm_gt145 <- factor(x = as.numeric(dataset$prsodm > 145),
                                 levels = c(0, 1),
                                 labels = c("No","Yes"))
  
  ## Pre-Operative White Blood Count <= 4,500
  dataset$prwbc_lte4p5 <- factor(x = as.numeric(dataset$prwbc <= 4.5),
                                 levels = c(0, 1),
                                 labels = c("No","Yes"))
  
  ## Pre-Operative White Blood Count > 11,000
  dataset$prwbc_gt11 <- factor(x = as.numeric(dataset$prwbc > 11),
                               levels = c(0, 1),
                               labels = c("No","Yes"))
  
  
  
  ######################################################
  ## Edit Existing Variables and Create New Variables ##
  ######################################################
  
  ## Impute the age values of "90+" to "90"
  dataset$age_90imp <- dataset$age
  dataset$age_90imp[ dataset$age_90imp == "90+" ] <- "90"
  dataset$age_90imp <- as.numeric(dataset$age_90imp)
  
  ## BMI
  dataset$bmi <- ((dataset$weight / (dataset$height * dataset$height)) * 703)
  
  ## BMI > 30
  dataset$bmi_gte30 <- factor(x = as.numeric(dataset$bmi >= 30),
                             levels = c(0, 1),
                             labels = c("BMI < 30","BMI 30+"))
  
  ## BMI Group
  dataset$bmi_group <- cut(dataset$bmi,
                           c(0, 18.5, 25, 30, 35, 40, 9999),
                           include.lowest = TRUE,
                           right = FALSE)
  dataset$bmi_group <- factor(x = dataset$bmi_group,
                              levels = c("[0,18.5)","[18.5,25)","[25,30)","[30,35)","[35,40)","[40,1e+04]"),
                              labels = c("<18.5","18.5-24.9","25.0-29.9","30.0-34.9","35.0-39.9","40.0+"))
  
  ## ASA Class Group
  dataset$asaclas[ dataset$asaclas == "None assigned" ] <- NA
  dataset$asaclas_group[ dataset$asaclas == "1-No Disturb" | dataset$asaclas == "2-Mild Disturb" ] <- "I-II"
  dataset$asaclas_group[ dataset$asaclas == "3-Severe Disturb" ] <- "III"
  dataset$asaclas_group[ dataset$asaclas == "4-Life Threat" | dataset$asaclas == "5-Moribund" ] <- "IV-V"
  dataset$asaclas_group[ is.na(dataset$asaclas) ] <- NA
  dataset$asaclas_group <- factor(x = dataset$asaclas_group,
                                  levels = c("I-II","III","IV-V"))
  
  ## ASA Class > III
  dataset$asaclas_gtIII <- factor(x = as.numeric(dataset$asaclas_group == "IV-V"),
                                  levels = c(0, 1),
                                  labels = c("ASA Class I, II, or III","ASA Class IV or V"))
  
  ## Race
  dataset$racenew <- factor(x = dataset$racenew,
                            levels = c("White","Black or African American","Asian","American Indian or Alaska Native",
                                       "Native Hawaiian or Pacific Islander","Unknown/Not Reported"))
  
  ## Non-white race
  dataset$racenew_nonwhite <- factor(x = dataset$racenew,
                                     levels = c("White","Black or African American","Asian","American Indian or Alaska Native",
                                                "Native Hawaiian or Pacific Islander","Unknown/Not Reported"),
                                     labels = c("White","Non-White","Non-White","Non-White",
                                                "Non-White","Unknown/Not Reported"))
  
  ## Post-operative hospital LOS
  dataset$hospital_los_postop <- (dataset$tothlos - dataset$htooday)
  
  ## Any diabetes
  dataset$diabetes_present <- factor(x = as.numeric(dataset$diabetes %in% c("INSULIN","NON-INSULIN","ORAL")),
                                     levels = c(0, 1),
                                     labels = c("No Diabetes","Diabetes"))
  
  ## Any dyspnea
  dataset$dyspnea_present <- factor(x = as.numeric(dataset$dyspnea %in% c("MODERATE EXERTION","AT REST")),
                                    levels = c(0, 1),
                                    labels = c("No Dyspnea","Dyspnea"))
  
  
  
  #######################################
  ## Edit and Create Outcome Variables ##
  #######################################
  
  
  
  
  ##########################################
  ## Create a Variable Summary Data Frame ##
  ##########################################
  
  ## Create a variable summary data frame
  variable_summary <- data.frame(Variable = names(dataset),
                                Label = names(dataset),
                                stringsAsFactors = FALSE)
  
  
  
  
  
  ######################################
  ## Return the Cleaned Up Data Files ##
  ######################################
  
  ## Remove empty factor levels
  for(i in 1:ncol(dataset)){
    if( is.factor(dataset[,i]) ){
      dataset[,i] <- droplevels(dataset[,i])
    }
  }
  rm(i)
  
  ## Return the cleaned up dataset and variable summary
  return(
    list(
      dataset,
      variable_summary)
    )
  
}
