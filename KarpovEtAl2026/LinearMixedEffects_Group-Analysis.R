# ---- LinearMixedEffects_Group-Analysis.R ----
# 
# Statistical analysis script used in:
# (Final Citation here)
#
# Main analysis investigating Group differences. 
# Requires MPlus.
#
# Model: ROI parameter estimate (negative - neutral stimuli) ~ Group * Age * Task Instruction + 
#                                                              Sex + (1|Subject)
# Variables:
#   1. Group = HC (Healthy controls) or ID (youth with internalizing disorders)
#   2. Age = Age in years, zero centered around the youngest participant
#   3. Task Instruction =
#         EN-Back: 0-Back, 2-Back
#         Perspective: Close, Far
#
# Note 1: Behavioral analysis (RT, accuracy, affective rating) used the same models, only changing the dependent variable
# Note 2: Exploratory Full-Brain Analysis used this script, but included all ROIs in the Schaefer-Tian atlas
#----------------------------------------------------------------------------#
# Script Inputs  
dataDIR = "path_to_ROI_data"
mplusDIR = "path_to_save_Mplus_outputs"
#----------------------------------------------------------------------------#
# 1. Import Libraries ----
library(MplusAutomation)
library(readxl)
library(stringr)
library(stringi)
library(dplyr)
library(utils)

#----------------------------------------------------------------------------#

# 2. Set Variables ----

#Get list of ROI data, one file per ROI per task.
filename = glob2rx("*_ROI-*.xlsx")
fileListAll = list.files(path = dataDIR, pattern = filename)

#Preallocate final data frame
allModels <- data.frame(paramHeader=character(),
                        param=character(), 
                        est=character(), 
                        se=numeric(),
                        est_se=numeric(),
                        pval=numeric(),
                        BetweenWithin=character(),
                        CI95=character(),
                        pval=numeric(),
                        pval2=numeric(),
                        stringsAsFactors=FALSE) 
#----------------------------------------------------------------------------#
## Loop through each task
# So that FDR correction is only run within each task

for (task in c("Perspective", "EN-Back")){
  fileList = fileListAll(stri_detect_fixed(fileListAll, task)) #extract ROIs from relevant task
  
  ## Looping through ROIs 
  for (file in fileList){
  
  rawData = read_excel(paste(dataDIR,file, sep=''))
  nameOfROI = str_match(file, "ROI-\\s*(.*?)\\s*.xlsx")[2] #extract ROI name from filename

  # 3. Data formatting & prep ----

  ## 3.1. Calculate difference in ROI activity ----
  # Creates final dependent variable, "roiD" 
  # roiD = ROI difference (negative - neutral)
    roiData = rawData %>% group_by(SubID, Session, Instruction) %>% 
      summarize(roiD = Beta[Stim == "Negative"] - Beta[Stim == "Neutral"], 
                Sex = unique(Sex), Age = unique(Age), Group = unique(Group))
 
  ## 3.2. Dummy Coding ----
    # Sex
  roiData$Sex[roiData$Sex == "Female"] = 0
  roiData$Sex[roiData$Sex == "Male"] = 1
    #Group
  roiData$Group[roiData$Group == "CG"] = 0
  roiData$Group[roiData$Group == "ID"] = 1
    # Task Instruction
  if (task == "Perspective"){
    roiData$Intruction[roiData$Intruction == "Close"] = 0
    roiData$Intruction[roiData$Intruction == "Far"] = 1
    }
  else if (task == "EN-Back"){
    roiData$Intruction[roiData$Intruction == "0-Back"] = 0
    roiData$Intruction[roiData$Intruction == "2-Back"] = 1 
  }
  
  ## 3.3. Final Touches ----
  
  #format data, grab only necessary subset of data for the model
  roiData = subset(roiData, select=c("SubID", "roiD", "Instruction", "Sex", "Group", "Age"))
  colnames(roiData) = c("Sub", "roiD", "Instr", "Sex", "Group", "Age")
  
  #center age around the youngest participant
  roiData$Age = roiData$Age - min(roiData$Age) # 10.2 is the age of the youngest participant
  
  # 4. Statistical Modelling ----
  
  ## 4.1. Define MPlus Model ----
  modelMplus = mplusObject(
    TITLE = "ROI - Batch Processing;",
    VARIABLE ="USEVARIABLES= SubID roiD Instr Sex Group Age 
    InsGroup InsAge GroupAge InsGrAge;
    WITHIN = Inst Age 
    InsGroup InsAge GroupAge InsGrAge;
    BETWEEN = Group Sex;
    CLUSTER = SubID;
    MISSING = ALL (-999.00);",
    DEFINE =" InsGroup = Instr*Group;
    RegAge = Instr*Age;
    GroupAge = Group*Age;
    RegGrAge = Instr*Age*Group;",
    ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
    MODEL = "
    %WITHIN%
      roiD ON  Instr Age InsGroup InsAge GroupAge InsGrAge;
    %BETWEEN%
      roiD ON Group Sex;",     
    OUTPUT = "STANDARDIZED CINTERVAL;",
    rdata = roiData)
  
  mplusOutName = paste(mplusDIR, task,"-", nameOfROI, ".inp", sep='')
  
  ## 4.2. Run Model----
  fits = mplusModeler(modelMplus, modelout = mplusOutName, run = 1L) 
  
  ## 4.3. Extract parameters ----
  ci = fits$results$parameters$ci.stdyx.standardized # extract standardized (STDYX) parameters
  cleanCI = subset(ci, paramHeader == "ROID.ON") 
  listCI = paste(cleanCI$low2.5, cleanCI$up2.5, sep=", ") #extract 95% confidence interval, upper and lower 2.5%
  params = fits$results$parameters$stdyx.standardized
  cleanParams = subset(params, paramHeader == "ROID.ON")
  cleanParams$paramHeader = nameOfROI
  cleanParams$CI95 = listCI
  cleanParams$pval2 = 2*pnorm(-abs(cleanParams$est_se)) # get more decimal places on the pval for FDR correction, 
                                                        # because MPLUS only provides up to 3 decimal places
                                                        # where anything < .001 is marked as 0.000
  
  allModels = rbind(allModels, cleanParams)
  
  } #end looping through ROIs

#----------------------------------------------------------------------------#
# 5. FDR correction ----
# runs corrections across all ROIs in each fixed effect
correctedModels = allModels %>% 
  group_by(param) %>%  #group by each fixed effect
  mutate(padj = p.adjust(pval2,method="BH")) %>% #use B-H method (AKA FDR correction)
  ungroup()

# 6.Significant results ----
sigRoiModels = subset(correctedModels, padj < 0.05)

} #end looping through tasks
