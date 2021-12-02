
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
  
  ## Remove unused/erroneous variables
  dataset$admsyr <- NULL
  
  
  
  #############################
  ## Create Factor Variables ##
  #############################
  
  ## Race
  dataset$racenew <- factor(x = dataset$racenew,
                            levels = c("White","Black or African American","Asian","American Indian or Alaska Native",
                                       "Native Hawaiian or Pacific Islander","Unknown/Not Reported"))
  
  ## Deep incisional SSI
  dataset$wndinfd <- factor(x = dataset$wndinfd,
                            levels = c("No Complication","Deep Incisional SSI"))
  
  
  ## Renal failure
  dataset$oprenafl <- factor(x = dataset$oprenafl,
                             levels = c("No Complication","Acute Renal Failure"))
  
  ## DVT
  dataset$othdvt <- factor(x = dataset$othdvt,
                           levels = c("No Complication","DVT Requiring Therap","DVT Requiring Therapy"),
                           labels = c("No Complication","DVT Requiring Therapy","DVT Requiring Therapy"))
  
  ## Cardiac arrest
  dataset$cdarrest <- factor(x = dataset$cdarrest,
                             levels = c("No Complication","Cardiac Arrest Requiring CPR"))
  
  ## Myocardial infarction
  dataset$cdmi <- factor(x = dataset$cdmi,
                         levels = c("No Complication","Myocardial Infarction"))
  
  ## Factor variables that don't require re-ordering of levels
  dataset$caseid <- factor(x = dataset$caseid)
  dataset$inout <- factor(x = dataset$inout)
  dataset$emergncy <- factor(x = dataset$emergncy)
  dataset$wndclas <- factor(x = dataset$wndclas)
  dataset$orgspcssi <- factor(x = dataset$orgspcssi)
  dataset$supinfec <- factor(x = dataset$supinfec)
  dataset$dehis <- factor(x = dataset$dehis)
  dataset$oupneumo <- factor(x = dataset$oupneumo)
  dataset$othbleed <- factor(x = dataset$othbleed)
  dataset$urninfec <- factor(x = dataset$urninfec)
  dataset$reintub <- factor(x = dataset$reintub)
  dataset$failwean <- factor(x = dataset$failwean)
  dataset$othsysep <- factor(x = dataset$othsysep)
  dataset$othseshock <- factor(x = dataset$othseshock)
  dataset$renainsf <- factor(x = dataset$renainsf)
  dataset$pulembol <- factor(x = dataset$pulembol)
  dataset$cnscva <- factor(x = dataset$cnscva)
  
  
  
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
  
  
  
  ##########################
  ## Create New Variables ##
  ##########################
  
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
  
  ## Non-white race
  dataset$racenew_nonwhite <- factor(x = dataset$racenew,
                                     levels = c("White","Black or African American","Asian","American Indian or Alaska Native",
                                                "Native Hawaiian or Pacific Islander","Unknown/Not Reported"),
                                     labels = c("White","Non-White","Non-White","Non-White",
                                                "Non-White","Unknown/Not Reported"))
 
  ## Total hospital LOS
  dataset$hospital_los_total <- dataset$tothlos
  
  ## Pre-operative hospital LOS
  dataset$hospital_los_preop <- dataset$htooday
  
  ## Post-operative hospital LOS
  dataset$hospital_los_postop <- (dataset$hospital_los_total - dataset$hospital_los_preop)
  
  ## Any diabetes
  dataset$diabetes_present <- factor(x = as.numeric(dataset$diabetes %in% c("INSULIN","NON-INSULIN","ORAL")),
                                     levels = c(0, 1),
                                     labels = c("No Diabetes","Diabetes"))
  
  ## Any dyspnea
  dataset$dyspnea_present <- factor(x = as.numeric(dataset$dyspnea %in% c("MODERATE EXERTION","AT REST")),
                                    levels = c(0, 1),
                                    labels = c("No Dyspnea","Dyspnea"))
  
  
  
  ##################################################
  ## Create Post-Operative Complication Variables ##
  ##################################################
  
  ## Standalone complications
  dataset$comp_wound_disruption <- factor(x = dataset$dehis,
                                          levels = c("No Complication","Wound Disruption"),
                                          labels = c("No","Yes"))
  dataset$comp_pneumonia <- factor(x = dataset$oupneumo,
                                   levels = c("No Complication","Pneumonia"),
                                   labels = c("No","Yes"))
  dataset$comp_transfusion <- factor(x = dataset$othbleed,
                                     levels = c("No Complication","Transfusions/Intraop/Postop"),
                                     labels = c("No","Yes"))
  dataset$comp_uti <- factor(x = dataset$urninfec,
                             levels = c("No Complication","Urinary Tract Infection"),
                             labels = c("No","Yes"))
  
  ## Individual complication variables
  dataset$comp_ssi_super <- factor(x = dataset$supinfec,
                                   levels = c("No Complication","Superficial Incisional SSI"),
                                   labels = c("No","Yes"))
  dataset$comp_ssi_deep_incis <- factor(x = dataset$wndinfd,
                                        levels = c("No Complication","Deep Incisional SSI"),
                                        labels = c("No","Yes"))
  dataset$comp_ssi_organ_space <- factor(x = dataset$orgspcssi,
                                         levels = c("No Complication","Organ/Space SSI"),
                                         labels = c("No","Yes"))
  
  ## Any SSI
  dataset$comp_ssi_any <- apply(X = dataset[, c("comp_ssi_super","comp_ssi_deep_incis","comp_ssi_organ_space") ],
                                MARGIN = 1,
                                FUN = function(x){
                                  if( (!is.na(x[1]) & x[1] == "Yes") |
                                     (!is.na(x[2]) & x[2] == "Yes") |
                                     (!is.na(x[3]) & x[3] == "Yes") ){
                                    return("Yes")
                                  } else if( is.na(x[1]) & is.na(x[2]) & is.na(x[3]) ){
                                    return(NA)
                                  } else{ return("No") }
                                    } )
  dataset$comp_ssi_any <- factor(x = dataset$comp_ssi_any)
  
  ## Individual complication variables
  dataset$comp_unpl_intub <- factor(x = dataset$reintub,
                                    levels = c("No Complication","Unplanned Intubation"),
                                    labels = c("No","Yes"))
  dataset$comp_vent48 <- factor(x = dataset$failwean,
                                levels = c("No Complication","On Ventilator greater than 48 Hours"),
                                labels = c("No","Yes"))
  
  ## Unplanned intubation or ventilator > 48 hours
  dataset$comp_unpl_intub_or_vent48 <- apply(X = dataset[, c("comp_unpl_intub","comp_vent48") ],
                                             MARGIN = 1,
                                             FUN = function(x){
                                               if( (!is.na(x[1]) & x[1] == "Yes") |
                                                  (!is.na(x[2]) & x[2] == "Yes") ){
                                                 return("Yes")
                                               } else if( is.na(x[1]) & is.na(x[2]) ){
                                                 return(NA)
                                               } else{ return("No") }
                                                 } )
  dataset$comp_unpl_intub_or_vent48 <- factor(x = dataset$comp_unpl_intub_or_vent48)
  
  ## Individual complication variables
  dataset$comp_sepsis <- factor(x = dataset$othsysep,
                                levels = c("No Complication","Sepsis"),
                                labels = c("No","Yes"))
  dataset$comp_septic_shock <- factor(x = dataset$othseshock,
                                      levels = c("No Complication","Septic Shock"),
                                      labels = c("No","Yes"))
  
  ## Sepsis or septic shock
  dataset$comp_sepsis_or_septic_shock <- apply(X = dataset[, c("comp_sepsis","comp_septic_shock") ],
                                               MARGIN = 1,
                                               FUN = function(x){
                                                 if( (!is.na(x[1]) & x[1] == "Yes") |
                                                    (!is.na(x[2]) & x[2] == "Yes") ){
                                                   return("Yes")
                                                 } else if( is.na(x[1]) & is.na(x[2]) ){
                                                   return(NA)
                                                 } else{ return("No") }
                                                   } )
  dataset$comp_sepsis_or_septic_shock <- factor(x = dataset$comp_sepsis_or_septic_shock)
  
  ## Individual complication variables
  dataset$comp_renal_insuff <- factor(x = dataset$renainsf,
                                      levels = c("No Complication","Progressive Renal Insufficiency"),
                                      labels = c("No","Yes"))
  dataset$comp_renal_failure <- factor(x = dataset$oprenafl,
                                       levels = c("No Complication","Acute Renal Failure"),
                                       labels = c("No","Yes"))
  
  ## Renal insufficiency or failure
  dataset$comp_renal_insuff_or_failure <- apply(X = dataset[, c("comp_renal_insuff","comp_renal_failure") ],
                                                MARGIN = 1,
                                                FUN = function(x){
                                                  if( (!is.na(x[1]) & x[1] == "Yes") |
                                                     (!is.na(x[2]) & x[2] == "Yes") ){
                                                    return("Yes")
                                                  } else if( is.na(x[1]) & is.na(x[2]) ) ){
                                                    return(NA)
                                                  } else{ return("No") }
                                                    } )
  dataset$comp_renal_insuff_or_failure <- factor(x = dataset$comp_renal_insuff_or_failure)
  
  ## Individual complication variables
  dataset$comp_pulm_embolism <- factor(x = dataset$,
                                       levels = c("No Complication","Pulmonary Embolism"),
                                       labels = c("No","Yes"))
  dataset$comp_dvt <- factor(x = dataset$othdvt,
                             levels = c("No Complication","DVT Requiring Therapy"),
                             labels = c("No","Yes"))
  
  ## Pulmonary embolism or DVT
  dataset$comp_pulm_embolism_or_dvt <- apply(X = dataset[, c("comp_pulm_embolism","comp_dvt") ],
                                             MARGIN = 1,
                                             FUN = function(x){
                                               if( (!is.na(x[1]) & x[1] == "Yes") |
                                                  (!is.na(x[2]) & x[2] == "Yes") ){
                                                 return("Yes")
                                               } else if( is.na(x[1]) & is.na(x[2]) ) ){
                                                 return(NA)
                                               } else{ return("No") }
                                                 } )
  dataset$comp_pulm_embolism_or_dvt <- factor(x = dataset$comp_pulm_embolism_or_dvt)
  
  ## Individual complication variables
  dataset$comp_cardiac_arrest <- factor(x = dataset$cdarrest,
                                        levels = c("No Complication","Cardiac Arrest Requiring CPR"),
                                        labels = c("No","Yes"))
  dataset$comp_mi <- factor(x = dataset$cdmi,
                            levels = c("No Complication","Myocardial Infarction"),
                            labels = c("No","Yes"))
  dataset$comp_stroke <- factor(x = dataset$cnscva,
                                levels = c("No Complication","Stroke/CVA"),
                                labels = c("No","Yes"))
  
  ## Cardiac arrest, MI, or stroke
  dataset$comp_cardiac_arrest_mi_stroke <- apply(X = dataset[, c("comp_cardiac_arrest","comp_mi","comp_stroke") ],
                                                 MARGIN = 1,
                                                 FUN = function(x){
                                                   if( (!is.na(x[1]) & x[1] == "Yes") |
                                                      (!is.na(x[2]) & x[2] == "Yes") |
                                                      (!is.na(x[3]) $ x[3] == "Yes")){
                                                     return("Yes")
                                                   } else if( is.na(x[1]) & is.na(x[2]) & is.na(x[3]) ){
                                                     return(NA)
                                                   } else{ return("No") }
                                                     } )
  
  ## Mortality
  dataset$comp_mortality <- factor(x = as.numeric( !is.na(dataset$dopertod) | dataset$dopertod >= 0),
                                   levels = c(0, 1),
                                   labels = c("No","Yes"))
  
  ## Any complication, including mortality
  dataset$comp_any_including_mortality <- apply(X = dataset[, c("comp_wound_disruption","comp_pneumonia","comp_transfusion",
                                                                "comp_uti","comp_ssi_super","comp_ssi_deep_incis",
                                                                "comp_ssi_organ_space","comp_unpl_intub","comp_vent48",
                                                                "comp_sepsis","comp_septic_shock","comp_renal_insuff",
                                                                "comp_renal_failure","comp_pulm_embolism","comp_dvt",
                                                                "comp_cardiac_arrest","comp_mi","comp_stroke",
                                                                "comp_mortality") ],
                                                MARGIN = 1,
                                                FUN = functions(x){
                                                  if( sum(is.na(x)) == length(x) ){
                                                    return(NA)
                                                  } else if( sum(x == "Yes", na.rm = TRUE) > 0 ){
                                                    return("Yes")
                                                  } else{ return("No") }
                                                    } )
  
  ## Any complication, excluding mortality
  dataset$comp_any_excluding_mortality <- apply(X = dataset[, c("comp_wound_disruption","comp_pneumonia","comp_transfusion",
                                                                "comp_uti","comp_ssi_super","comp_ssi_deep_incis",
                                                                "comp_ssi_organ_space","comp_unpl_intub","comp_vent48",
                                                                "comp_sepsis","comp_septic_shock","comp_renal_insuff",
                                                                "comp_renal_failure","comp_pulm_embolism","comp_dvt",
                                                                "comp_cardiac_arrest","comp_mi","comp_stroke") ],
                                                MARGIN = 1,
                                                FUN = functions(x){
                                                  if( sum(is.na(x)) == length(x) ){
                                                    return(NA)
                                                  } else if( sum(x == "Yes", na.rm = TRUE) > 0 ){
                                                    return("Yes")
                                                  } else{ return("No") }
                                                    } )
  
  
  
  ##########################################
  ## Create a Variable Summary Data Frame ##
  ##########################################
  
  ## Create a variable summary data frame
  variable_summary <- data.frame(Variable = names(dataset),
                                 Label = names(dataset),
                                 Variable_Type = NA,
                                 stringsAsFactors = FALSE)
  
  ## Identify continuous variables
  vars_cont <- c("pufyear","workrvu","age","age_90imp","admyr","operyr","height","weight","bmi",
                 "dprna","dprbun","dprcreat","dpralbum","dprbili","dprsgot","dpralkph","dprwbc","dprhct","dprplate","dprptt",
                 "dprpt","dprinr",
                 "prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr",
                 "prpt",
                 "otherwrvu1","otherwrvu2","otherwrvu3","otherwrvu4","otherwrvu5","otherwrvu6","otherwrvu7","otherwrvu8",
                 "otherwrvu9","otherwrvu10",
                 "conwrvu1","conwrvu2","conwrvu3","conwrvu4","conwrvu5","conwrvu6","conwrvu7","conwrvu8","conwrvu9","conwrvu10",
                 "optime","hdisdt","yrdeath","tothlos","htooday","hospital_los_total","hospital_los_preop","hospital_los_postop",
                 "dsupinfec","dwndinfd","dorgspcssi","ddehis","doupneumo","dreintub","dpulembol","dfailwean","drenainsf",
                 "doprenafl","durninfec","dcnscva","dcdarrest","dcdmi","dothbleed","dothdvt","dothsysep","dothseshock",
                 "retorpodays","retor2podays","readmpodays1","readmpodays2","readmpodays3","readmpodays4","readmpodays5",
                 "dothcdiff")
  
  ## Identify ordinal variables
  vars_ord <- c("wndclas","asaclas_group","bmi_group","admqtr",
                "nsupinfec","nwndinfd","norgspcssi","ndehis","noupneumo","nreintub","npulembol","nfailwean","nrenainsf",
                "noprenafl","nurninfec","ncnscva","ncdarrest","ncdmi","nothbleed","nothdvt","nothsysep","nothseshock",
                "nothcdiff")
  
  ## Define variable types
  variable_summary$Variable_Type <- "Categorical"
  variable_summary$Variable_Type[ variable_summary$Variable %in% vars_cont ] <- "Continuous"
  variable_summary$Variable_Type[ variable_summary$Variable %in% vars_ord ] <- "Ordinal"
  
  ## A function to update variable labels
  update_label <- function(df = variable_summary, variable, label){
    variable_summary$Label[ variable_summary$Variable == variable ] <- label
    return(variable_summary)
  }
  
  ## Manually update variable labels
  #variable_summary <- update_label(variable = "", label = "")
  
  
  
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
      Data = dataset,
      VariableSummary = variable_summary)
    )
  
}
