# ---- LinearMixedEffects_Adversity-Analysis.R ----
# 
# Statistical analysis script used in:
# (Final Citation here)
#
# Exploratory analysis investigating the potential moderating effect of adversity in youth with internalizing disorders (ID group)
# Requires MPlus.
#
# General Model: 
# ROI parameter estimate (negative - neutral) ~ Adversity Exposure * Age * Task Instruction + 
#                                                       Psychopathology Severity + Sex + (1|Subject)
#
# The exact model differs based on what effects were significant in the group analysis,
# such that only the most complex significant group interaction was tested.
#
# Variables:
#   1. Adversity Exposure = mean-centered SLES scores
#           SLES: Stressful Life Events Schedule (Williamson et al., 2003)
#   2. Age = Age in years, zero centered around the youngest participant
#   3. Task Instruction =
#         EN-Back: 0-Back, 2-Back
#         Perspective: Close, Far
#   4. Psychopathology Severity = mean-centered CBCL-YSR total score
#           CBCL-YSR: Child Behavior Checklist – Youth Self Report (Achenbach, 1999)
#----------------------------------------------------------------------------#
# Exact Models:
#  1) For ROIs with only a Group main effect (no interactions with group):
#      -> ROI (neg-neut) ~ Adversity + Instruction + Age + Psychopathology Severity + Sex + (1|Subject)
#  2) For ROIs with a significant 3-way interaction of Group:Instruction:Age
#      -> ROI (neg-neut) ~ Adversity*Instruction*Age + Psychopathology Severity + Sex + (1|Subject)

#============================================================================#
# 1. Import Libraries ----
library(MplusAutomation)
library(readxl)
library(stringr)
library(dplyr)
library(utils)
#----------------------------------------------------------------------------#
# 2. Set Variables ----
xCBCL = 59.06
xSLES = 45.76
# Means taken from entire ID sample, averaged across both sessions 1&2
# Including subjects that were not used in fMRI analyses. 

# Script Inputs  
dataDIR = "path_to_ROI_data"
mplusDIR = "path_to_save_Mplus_outputs"

# Schaefer labels of ROIs with a Group Main effect
perspectiveROIs = c(134,167,186,185,353,86,170, 324, 314,308, 147, 183, 317, 316,
                    113,189,115, 391, 98, 345,358,104,303, 89, 102, 103,101,99)
enbackROIs = c(143, 347, 311, 107, 314, 350, 141, 307, 175, 318, 317)

#preallocate final matrix
allGroupModels <- data.frame(paramHeader=character(),
                             param=character(), 
                             est=character(), 
                             se=numeric(),
                             est_se=numeric(),
                             pval=numeric(),
                             BeetweenWithin=character(),
                             CI95=character(),
                             pval=numeric(),
                             pval2 =numeric(),
                             psig=character(),
                             task=character(),
                             stringsAsFactors=FALSE) 

#----------------------------------------------------------------------------#

# 3. Statistical Modelling ----

## 3.1. Main Effect of Adversity ----
# Model: ROI (neg-neut) ~ Adversity + Instruction + Age + Psychopathology Severity + Sex + (1|Subject)
#
# See Tables 2 & 3 in the manuscript for a list of the ROIs analysed here,
#   for the EN-Back and Perspective tasks, respectively
#
# This model was also used for Behavior (EN-Back accuracy, Perspective affective rating)

#Loop through tasks
for (task in c("Perspective", "EN-Back")){
  
  if (task == "Perspective"){
    roiList = perspectiveROIs
  }
  else if (task == "EN-Back"){
    roiList = enbackROIs
  }
  
  ## Looping through ROIs 
  for (roi in roiList){
    
    ### 3.1.1. Prep Data ----
    ## Read in data
    rawData = read_excel(paste(dataDIR,"ROI-", as.character(roi), "_task-", task, ".xlsx", sep=''))
    nameOfROI = as.character(roi)

    ## Keep only ID youth, remove CG subjects
    idData = subset(rawData, Group == "ID")
    
    ## Mean-center questionnaire scores
    idData$CBCL = idData$CBCL - xCBCL 
    idData$SLES = idData$SLES - xSLES 
    
    ## Center age around the youngest participant
    idData$Age = idData$Age - min(idData$Age)
    
    ## Calculate difference in ROI activity 
    # roiD = ROI difference (negative - neutral)
    roiData = idData %>% group_by(SubID, Session, Instruction) %>% 
      summarize(roiD = Beta[Stim == "Negative"] - Beta[Stim == "Neutral"], 
                Sex = unique(Sex), Age = unique(Age), 
                CBCL = unique(CBCL), SLES = unique(SLES))

    
    ## Dummy Coding 
      #Sex
    roiData$Sex[roiData$Sex == "Female"] = 0
    roiData$Sex[roiData$Sex == "Male"] = 1
      # Task Instruction
    if (task == "Perspective"){
      roiData$Intruction[roiData$Intruction == "Close"] = 0
      roiData$Intruction[roiData$Intruction == "Far"] = 1
    }
    else if (task == "EN-Back"){
      roiData$Intruction[roiData$Intruction == "0-Back"] = 0
      roiData$Intruction[roiData$Intruction == "2-Back"] = 1 
    }
    
    # Format data, grab only necessary subset of data for the model
    roiData = subset(roiData, select=c("SubID", "roiD", "Instruction", "Sex", "Age", "CBCL", "SLES"))
    colnames(roiData) = c("Sub", "roiD", "Ins", "Sex", "Age", "CBCL", "SLES")
    
    # Mark missing data for MPlus
    # For subjects missing CBCL and/or SLES scores
    roiData[is.na(roiData)] = -999.00 
    
    ## 3.1.2. MPlus Model ----
# roiD ~ SLES + Instr + Age + CBCL + Sex + (1|Subject)
    
    modelGROUP = mplusObject(
      TITLE = "RoiDiff Adversity-MainEffect;",
      VARIABLE ="USEVARIABLES= SubID roiD Ins Sex Age CBCL SLES;
    WITHIN = Ins CBCL SLES Age;
    BETWEEN = Sex;
    CLUSTER = SubID;
    MISSING = ALL (-999.00);",
      ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
      MODEL = "
    %WITHIN%
    roiD ON Stim Ins CBCL SLES Age
          ;
      CBCL SLES;
    %BETWEEN%
    roiD ON Sex;",     
      OUTPUT = "STANDARDIZED CINTERVAL;",
      rdata = roiData)
    
    mplusOutName = paste(mplusDIR, task,"-", nameOfROI, ".inp", sep='')
    
    ## Run Model 
    fits = mplusModeler(modelMplus, modelout = mplusOutName, run = 1L) 
    
    ## 3.1.3. Extract parameters ----
    ci = fits$results$parameters$ci.stdyx.standardized # extract standardized (STDYX) parameters
    cleanCI = subset(ci, paramHeader == "ROID.ON") 
    listCI = paste(cleanCI$low2.5, cleanCI$up2.5, sep=", ") #extract 95% confidence interval, upper and lower 2.5%
    params = fits$results$parameters$stdyx.standardized
    cleanParams = subset(params, paramHeader == "ROID.ON")
    cleanParams$paramHeader = nameOfROI
    cleanParams$CI95 = listCI
    cleanParams$pval2 = 2*pnorm(-abs(cleanParams$est_se)) # get more decimal places on the pval, 
                                                          # because MPLUS only provides up to 3 decimal places
                                                          # where anything < .001 is marked as 0.000
    
    # Mark significant effects
    cleanParams$psig[cleanParams$pval2 < 0.05] = "*"
    cleanParams$psig[cleanParams$pval2 < 0.01] = "**"
    cleanParams$psig[cleanParams$pval2 < 0.001] = "***"
    cleanParams$task = task
    
    allModels = rbind(allModels, cleanParams)
    
  } #end looping through ROIs
} #end looping through tasks  

#--------------------------------------------------------------------------------------------#
## 3.2. Adversity 3-way Interaction ----
# ROI (neg-neut) ~ Adversity*Instruction*Age + Psychopathology Severity + Sex + (1|Subject)
# 
# EN-Back ROI: The left ventral anterior insula (Schaefer 143)
# Perspective ROIs: left dorsal anterior insula (Schaefer 101) and left Rolandic Operculum (Schaefer 104) 


  ### 3.2.1. Prep Data ----
  # Same as 3.1.1.

  ### 3.2.2. MPlus Model ----
# roiD ~ SLES*Instr*Age + CBCL + Sex + (1|Subject)
  modelMPlus = mplusObject(
    TITLE = "RoiDiff 3-Way Adversity;",
    VARIABLE ="USEVARIABLES= SubID roiD Ins Sex Age CBCL SLES
      InsSLES InsAge AgeSLES
      InsAgeS;
    WITHIN = Ins CBCL SLES Age
    InsSLES InsAge InsSLES
    InsAgeS;
    BETWEEN = Sex;
    CLUSTER = SubID;
    MISSING = ALL (-999.00);",
  DEFINE =" InsSLES = Ins*SLES;
    InsAge = Ins*Age;
    AgeSLES = Age*SLES;
    InsAgeS = Ins*SLES*Age;",
  ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
  MODEL = "
    %WITHIN%
    roiD ON Ins CBCL SLES Age
      InsSLES InsAge AgeSLES InsAgeS;
    CBCL SLES InsSLES AgeSLES InsAgeS ;
    %BETWEEN%
    roiD ON Sex;",     
  OUTPUT = "STANDARDIZED CINTERVAL;",
  rdata = roiData)

  ### 3.2.3. Extract Parameters ----
  # Same as 3.1.3. 

