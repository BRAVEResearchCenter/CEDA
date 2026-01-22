# LinearMixedEffects_Analysis.R ----
# 
# Statistical analysis script used in:
#   Karpov, G., Heyn, S. A., Russell, J. D., Keding, T. J., & Herringa, R. J. (2026). 
#   Neurodevelopmental correlates of emotion regulation in adolescence: An accelerated longitudinal study. 
#   Developmental Cognitive Neuroscience, 78, 101664. https://doi.org/10.1016/j.dcn.2025.101664
#
# Requires MPlus.
# Uses data structured in long format - example provided at the end of the script.
#
# Model: ROI parameter estimate ~ deltaAge*baseAge*Stimulus Valence*Regulation + 
#                       Adversity exposure (SLES) + Psychopathology Symptoms (CBCL-YSR) + Sex + (1|Subject)
# Variables:
#   deltaAge = Change in Age (Age at current session - Age at session 1)
#   baseAge = Baseline Age (zero centered around the youngest participant)
#   Stimulus Valence = 
#         EN-Back: Positive Faces, Negative Faces, Neutral Faces
#         Perspective: Negative pictures, Neutral pictures
#   Regulation =
#         EN-Back: 0-Back, 2-Back
#         Perspective: Close (Emotionally Engage), Far (Emotionally Disengage)
#
# Missing data handled using Full Information Maximum Likelihood estimation (in MPlus: 'MLR')
# Note: Behavioral analysis used the same models, only changing the dependent variable
#----------------------------------------------------------------------------#
# Script Inputs  
task = "task-name-here" #'ENBack' or 'Perspective'
dataDIR = "path_to_ROI_data"
mplusDIR = "path_to_save_Mplus_outputs"
outputDIR = "path_to_store_Results"
#----------------------------------------------------------------------------#
## 1. Libraries ----
library(MplusAutomation)
library(readxl)
library(scipub)
library(dplyr)
library(utils)

## 2. Data formatting ----

#Get list of ROI data, one file per ROI per task.
filename = glob2rx(paste(task, "_ROI-*.xlsx", sep=''))
fileList = list.files(path = dataDIR, pattern = filename)

### 2.2 Set variables ----
xCBCL = 33.6 #mean CBCL score across all subjects and sessions
sdCBCL = 28 #standard deviation 
xSLES = 30 #mean SLES score across all subjects and sessions
sdSLES = 24.2 #standard deviation 

#preallocate final matrix
allModels <- data.frame(paramHeader=character(),
                        param=character(), 
                        est=character(), 
                        se=numeric(),
                        est_se=numeric(),
                        pval=numeric(),
                        BetweenWithin=character(),
                        CI95=character(),
                        pval2=numeric(),
                        stringsAsFactors=FALSE) 

## Looping through ROIs 
for (roiFile in fileList){
  
  rawData = read_excel(paste(dataDIR,roiFile, sep=''))
  nameOfROI = str_match(file, "ROI-\\s*(.*?)\\s*.xlsx")[2] #extract ROI name from file
  
  ##format data, grab only necessary subset of data for the model
  roiData = subset(rawData, select=c("SubID", "Beta", "Sex", "Stimulus", "Regulation", "CBCL-YSR", "SLES", "baselineAge", "deltaAge"))
  colnames(roiData) = c("Sub", "ROI", "Sex", "Stim", "Reg",  "CBCL", "SLES", "baseAge", "deltaAge")
  
  ### 2.3 Z-score CBCL-YSR and SLES ----
  roiData$CBCL = (roiData$CBCL - xCBCL) / sdCBCL
  roiData$SLES = (roiData$SLES - xSLES) / sdSLES
  # Winsorize
  roiData$CBCL = winsorZ(roiData$CBCL, zbound = 2)
  roiData$SLES = winsorZ(roiData$SLES, zbound = 2)
  
  # Mark missing data for MPlus
  roiData$CBCL[is.na(roiData$CBCL)] = -999 
  roiData$SLES[is.na(roiData$SLES)] = -999 
  
  # Dummy code categorical variables
  roiData$Sex[roiData$Sex == "Male"] = 1 
  roiData$Sex[roiData$Sex == "Female"] = 0
  
  # 3. EN-Back Task ----
  if (task == "ENBack") {
    roiData = subset(roiData, Stim != "Place") #remove "place" stimuli, leave in only faces
    
    ## 3.1 Dummy Coding EN-Back ----
      #Stimulus Valence
    roiData$Stim[roiData$Stim == "PositiveFace"] = 2 
    roiData$Stim[roiData$Stim == "NegativeFace"] = 1 
    roiData$Stim[roiData$Stim == "NeutralFace"] = 0 
      #Regulation
    roiData$Reg[roiData$Reg == "2-Back"] = 1 
    roiData$Reg[roiData$Reg == "0-Back"] = 0 
    
    #__________________________________________________
    ## 3.2 MPlus Model for EN-Back ----
    modelMplus = mplusObject(
    TITLE = "EN-Back Batch ROI;",
    
    VARIABLE ="USEVARIABLES= Sub ROI Sex Stim Reg CBCL SLES baseAge deltaAge 
    AAge RegDAge RegBAge RegAAge
    stimNeg stimPos CNeg CPos
    NegBAge NegDAge NegAAge
    PosBAge PosDAge PosAAge
    CNegDAge CNegBAge CNegAAge
    CPosDAge CPosBAge CPosAAge;
    Nominal is Stim;
    
    WITHIN = stimNeg stimPos Reg deltaAge CBCL SLES  
            CNeg CPos AAge
            RegDAge RegBAge RegAAge 
            NegBAge NegDAge NegAAge
            PosBAge PosDAge PosAAge
            CNegDAge CNegBAge CNegAAge
            CPosDAge CPosBAge CPosAAge;
    BETWEEN = baseAge Sex;
    CLUSTER = Sub;
    MISSING = ALL (-999);",
      
      DEFINE ="AAge = baseAge*deltaAge;
    stimNeg = Stim == 1;
    stimPos = Stim == 2;
    CNeg = stimNeg*Reg;
    CPos = stimPos*Reg;
    NegBAge = stimNeg*baseAge;
    NegDAge = stimNeg*deltaAge;
    NegAAge = stimNeg*baseAge*deltaAge;
    PosBAge = stimPos*baseAge;
    PosDAge = stimPos*deltaAge;
    PosAAge = stimPos*baseAge*deltaAge;
    RegBAge = Reg*baseAge;
    RegDAge = Reg*deltaAge;
    RegAAge = Reg*baseAge*deltaAge;
    CNegBAge = stimNeg*Reg*baseAge;
    CNegDAge = stimNeg*Reg*deltaAge;
    CNegAAge = stimNeg*Reg*baseAge*deltaAge;
    CPosBAge = stimPos*Reg*baseAge;
    CPosDAge = stimPos*Reg*deltaAge;
    CPosAAge = stimPos*Reg*baseAge*deltaAge;",
      
      ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
      
      MODEL = "
    %WITHIN%
    ROI ON  stimPos stimNeg Reg deltaAge CBCL SLES 
            CNeg CPos AAge
            RegBAge RegDAge RegAAge 
            NegBAge NegDAge NegAAge
            PosBAge PosDAge PosAAge
            CNegBAge CNegDAge CNegAAge
            CPosBAge CPosDAge CPosAAge;
    deltaAge AAge CBCL SLES;
    
    %BETWEEN%
    ROI ON baseAge Sex; 
    baseAge Sex;",     
      
    OUTPUT = "STANDARDIZED CINTERVAL;",
      rdata = roiData)
    #------------ EN-Back Model End -----------------------------#
  
  # 4. Perspective Task ---- 
  } else if (task == "Perspective") {
    
    ## 4.1 Dummy Coding Perspective ----
      #Stimulus Valence
    roiData$Stim[roiData$Stim == "Negative"] = 1 
    roiData$Stim[roiData$Stim == "Neutral"] = 0 
      #Regulation
    roiData$Reg[roiData$Reg == "Far"] = 1 
    roiData$Reg[roiData$Reg == "Close"] = 0 
    
    #------------------------------------------------------------#
    # 4.2 MPlus Model for Perspective ----
    modelMplus = mplusObject(
    TITLE = "Perspective Batch ROI;",
    
    VARIABLE ="USEVARIABLES= Sub ROI Sex Stim Reg CBCL SLES baseAge deltaAge 
    Cond AAge 
    StimDAge StimBAge StimAAge 
    RegDAge RegBAge RegAAge
    CondDAge CondBAge CondAAge;
    
    WITHIN = Stim Reg deltaAge CBCL SLES 
             Cond AAge 
             StimDAge StimBAge StimAAge
             RegDAge RegBAge RegAAge 
             CondDAge CondBAge CondAAge;
    BETWEEN = baseAge Sex;
    CLUSTER = Sub;
    MISSING = ALL (-999);",
      
      DEFINE ="AAge = baseAge*deltaAge;
    Cond = Stim*Reg;
    StimBAge = Stim*baseAge;
    StimDAge = Stim*deltaAge;
    StimAAge = Stim*baseAge*deltaAge;
    RegBAge = Reg*baseAge;
    RegDAge = Reg*deltaAge;
    RegAAge = Reg*baseAge*deltaAge;
    CondBAge = Stim*Reg*baseAge;
    CondDAge = Stim*Reg*deltaAge;
    CondAAge = Stim*Reg*baseAge*deltaAge;",
      
      ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
      
      MODEL = "
    %WITHIN%
    ROI ON  Stim Reg deltaAge CBCL SLES 
            Cond AAge 
            StimDAge StimBAge StimAAge
            RegDAge RegBAge RegAAge 
            CondDAge CondBAge CondAAge;
    deltaAge AAge CBCL SLES;
    
    %BETWEEN%
    ROI ON baseAge Sex; 
    baseAge Sex;",     
      
    OUTPUT = "STANDARDIZED CINTERVAL;",
      rdata = roiData)
    #-------Perspective Model End------------------------------#
  } #end task if statement
  
  mplusOutName = paste(mplusDIR, task,"-", nameOfROI, ".inp", sep='') #make filename for MPlus to use
  fits = mplusModeler(modelMplus, modelout = mplusOutName, run = 1L) #run model in MPlus, get fits
  
# 5. Extract parameters ----
    # Get unstandardized parameter estimates, error, test statistic, and p-values
  params = fits$results$parameters$unstandardized
  cleanParams = subset(params, paramHeader == "ROI.ON" & param!="CBCL" & param!="SLES" & param!="SEX") #remove variables of no interest
  cleanParams$paramHeader = nameOfROI #label each estimate with the current ROI
    # Get 95% confidence intervals from the unstandardized estimates
  ci = fits$results$parameters$ci.unstandardized
  cleanCI = subset(ci, paramHeader == "ROI.ON" & param!="CBCL" & param!="SLES" & param!="SEX") #remove variables of no interest
  listCI = paste(cleanCI$low2.5, cleanCI$up2.5, sep=", ") #get the lower and upper 2.5% interval
  cleanParams$CI95 = listCI

  # MPlus does not provide p-values with more than 3 decimal precision.
  # This causes issues for FDR correction, as any p-value < 0.001 has a value of 0.000
  # Thus, the p-value is recalculated in R to a higher decimal precision from the z-score ('est_se'):
  cleanParams$pval2 = 2*pnorm(-abs(cleanParams$est_se)) 
  
  # Add current ROI to master dataframe
  allModels = rbind(allModels, cleanParams)
  
} #end ROI loop

# After all ROIs have been modeled and amalgamated into one dataframe:

# 6. Multiple Comparisons Correction ----
  # Run FDR correction across all ROI/models
correctedModels = allModels %>% 
  group_by(param) %>%  #group by each fixed effect
  mutate(padj = p.adjust(pval2, method="BH")) %>% #use B-H method (AKA FDR)
  ungroup()

# Identify significant fixed-effects using FDR corrected p-values
sigRoiModels = subset(correctedModels, padj < 0.05)

#Save outputs
write.csv(sigRoiModels, paste(outputDIR, "ROI_", task, "_Results.csv", sep=""), row.names = FALSE)

#=================================================================================================#
# 7. Extra: Example data structure ----
## Perspective Task 
  
  # Create randomized values for each variable
numSubs = 4
numSessions = 2
numConditions = 4 #(Negative-Close, Negative-Far, Neutral-Close, Neutral-Far)
StimList = c("Negative", "Neutral")
RegList = c("Close", "Far")
SexList = c("Male", "Female")
CBCLlist = round(runif(n=numSubs*numSessions, min = 0, max = 125),0) #values taken from study sample 
SLESlist = round(runif(n=numSubs*numSessions, min = 0, max = 145),0) #values taken from study sample 
baseAgeList = round(runif(n=numSubs*numSessions, min = 0, max = 7),1) 
deltaAgeList = round(runif(n=numSubs, min = 0.9, max = 2.5),1)

SubID = rep(1:numSubs, each=numSessions*numConditions)
Session = rep(1:numSessions, each=numConditions, length.out=length(SubID))
ROI = round(runif(n=length(SubID), min =-3, max = 3),4)
Sex = rep(SexList, each=numSessions*numConditions, length.out=length(SubID))
Stim = rep(StimList, each=2, length.out=length(SubID))
Reg = rep(RegList, length.out=length(SubID))
CBCL = rep(CBCLlist, each=numConditions, length.out=length(SubID))
SLES = rep(SLESlist, each=numConditions, length.out=length(SubID))
baseAge = rep(baseAgeList, each=numConditions, length.out=length(SubID))
tempDeltaAge2 = rep(deltaAgeList, each=numConditions, length.out=numSubs*numConditions)
  
# Create data frame 
exampleData = as.data.frame(cbind(SubID, ROI, Sex, Stim, Reg, CBCL, SLES, baseAge, Session))
exampleData$deltaAge[Session == 1] = 0 #'change in age' at session 1 is always 0
exampleData$deltaAge[Session == 2] = tempDeltaAge2

  #randomly choose 2 people to have missing questionnaire data
rmSub = round(runif(n=2, min = 1, max = numSubs),0)
exampleData$CBCL[which(exampleData$Session %in% 1 & exampleData$SubID %in% rmSub[1])] = NA
exampleData$SLES[which(exampleData$Session %in% 2 & exampleData$SubID %in% rmSub[2])] = NA
