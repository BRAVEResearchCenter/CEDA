# ---- LinearMixedEffects_Decomp-Analysis.R ----
# 
# Statistical analysis script used in:
# (Final Citation here)
#
# Decomposition analysis across individual stimulus valences (negative, neutral) for significant effects.
# Requires MPlus.
#
# Model: adds the variable 'Valence' to the interaction of interest. 
# Includes models for both Group- and Adversity- related analyses.
#
#============================================================================#
# Import Libraries 
library(MplusAutomation)
#----------------------------------------------------------------------------#
# 1. Group-Related Decomp ----
#
# Decomposition of the 3-way interaction (between group, instruction, age)
# Used for:
#   - EN-Back, left ventral anterior insula (Schaefer 143); supplementary Figure S2
#   - Perspective task, left dorsal anterior insula (Schaefer 101); supplementary Figure S3
#   - Perspective task, left Rolandic Operculum (Schaefer 104); supplementary Figure S3

## 1.1 Data prep ----
# See: 'LinearMixedEffects_Group-Analysis.R' code Section 3, 
# With these differences:
#   1) Raw ROI BOLD parameters are used as the DV -- 
#       difference between negative and neutral stimuli NOT taken
#   2) Additional new variable of Stimulus Valence:

#  Dummy Coding Stimulus Valence [[NEW]]
roiData$Stimulus_Valence[roiData$Stimulus_Valence == "Neutral"] = 0 
roiData$Stimulus_Valence[roiData$Stimulus_Valence == "Negative"] = 1 
  
# Format Data
roiData = subset(roiData, select=c("SubID", "Beta", "Sex", "Stimulus_Valence", "Instruction", "Group","Age"))
colnames(roiData) = c("Sub", "ROI", "Sex", "Stim", "Instr", "Group", "Age")

## 1.2 MPLUS Model  ----
# Model: ROI ~ Valence * Group * Age * Task Instruction + Sex + (1|Subject)
modelMplus = mplusObject(
  TITLE = "Group Stim Decomp;",
  VARIABLE ="USEVARIABLES = Sub ROI Sex Stim Instr Group Age 
            Cond StimAge InstAge AgeGrp
             StimGrp InstGrp CondGrp 
             CondAge InstAgeG StimAgeG CondAgeG;
    WITHIN = Stim Instr Age 
             Cond StimAge InstAge AgeGrp
             StimGrp InstGrp CondGrp 
             CondAge InstAgeG StimAgeG CondAgeG;
    BETWEEN = Sex Group;
    CLUSTER = Sub;
    MISSING = ALL (-999);",
  DEFINE ="
    Cond = Stim*Instr;
    StimAge = Stim*Age;
    InstAge = Instr*Age;
    StimGrp = Stim*Group;
    InstGrp = Instr*Group;
    AgeGrp = Age*Group;
    CondGrp = Stim*Instr*Group;
    CondAge = Stim*Instr*Age;
    InstAgeG = Instr*Age*Group;
    StimAgeG = Stim*Age*Group;
    CondAgeG = Stim*Instr*Age*Group;",
  ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
  MODEL = "
    %WITHIN%
    ROI ON  Stim Instr Age 
             Cond StimAge InstAge AgeGrp
             StimGrp InstGrp CondGrp 
             CondAge InstAgeG StimAgeG CondAgeG;
             
    %BETWEEN%
    ROI ON Group Sex;",     
  OUTPUT = "STANDARDIZED CINTERVAL;",
  rdata = roiData)

#----------------------------------------------------------------------------#
# 2. Adversity-Related Decomp ----

## 2.1. Data prep ----
# See: 'LinearMixedEffects_Adversity-Analysis.R' code Section 3.1.1, 
# With these differences:
#   1) Raw DV values (e.g. affective rating, ROI BOLD parameters) used -- 
#       difference between negative and neutral stimuli NOT taken
#   2) Additional new variable of Stimulus Valence:

#  Dummy Coding Stimulus Valence [[NEW]]
roiData$Stimulus_Valence[roiData$Stimulus_Valence == "Neutral"] = 0 
roiData$Stimulus_Valence[roiData$Stimulus_Valence == "Negative"] = 1 

# Format Data
roiData = subset(roiData, select=c("SubID", "Beta", "Sex", "Stimulus_Valence", "Instruction", "SLES","Age"))
colnames(roiData) = c("Sub", "ROI", "Sex", "Stim", "Instr", "SLES", "Age")

## 2.2. Adversity Main Effect ----
# Decomposition of the main effect of Adversity for:
#    - Perspective Task, Affective Rating (behavior); Supplementary Figure S4 
#    - Perspective Task, ROIs; Supplementary Figure S5
#         (see Table 4 in the manuscript for exact regions)
#                     


# Model: DV ~ Valence*SLES (Adversity) + Instr + Age + CBCL + Sex + (1|Subject)
modelMPLUS = mplusObject(
  TITLE = "Adversity Stim Decomp;",
  VARIABLE ="USEVARIABLES= SubID ROI Stim Instr Sex CBCL SLES Age StimSLES;
    WITHIN = Stim Instr CBCL SLES Age StimSLES;
    BETWEEN = Sex;
    CLUSTER = SubID;
    MISSING = ALL (-999.00);",
  DEFINE ="StimSLES = Stim*SLES;",
  ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
  MODEL = "
    %WITHIN%
    ROI ON Stim Instr CBCL SLES Age StimSLES;
    CBCL SLES StimSLES;
    %BETWEEN%
    ROI ON Sex;",     
  OUTPUT = "STANDARDIZED CINTERVAL;",
  rdata = roiData)


## 2.3. Adversity 3-way Interaction ----
# Decomposition of the adversity by instruction by age interaction
# for the Perspective Task, left dorsal anterior insula (Schaefer 101); Figure 4C

# Model: ROI ~ Valence*Instruction*Age*SLES (Adversity) + CBCL + Sex + (1|Subject)
modelMplus = mplusObject(
  TITLE = "Decomp Adversity 4-way;",
  VARIABLE ="USEVARIABLES = Sub ROI Sex Stim Instr CBCL SLES Age 
            Cond StimAge InstAge AgeSLES
             StimSLES InstSLES CondSLES 
             CondAge InsAgeSL StimAgeS CondAgeS;
    WITHIN = Stim Instr CBCL SLES Age 
             Cond StimAge InstAge AgeSLES
             StimSLES InstSLES CondSLES 
             CondAge InsAgeSL StimAgeS CondAgeS;
    BETWEEN = Sex;
    CLUSTER = Sub;
    MISSING = ALL (-999);",
  DEFINE ="
    Cond = Stim*Instr;
    StimAge = Stim*Age;
    InstAge = Instr*Age;
    StimSLES = Stim * SLES;
    InstSLES = Instr*SLES;
    AgeSLES = Age*SLES;
    CondSLES = Stim*Instr*SLES;
    CondAge = Stim*Instr*Age;
    InsAgeSL = Instr*Age*SLES;
    StimAgeS = Stim*Age*SLES;
    CondAgeS = Stim*Instr*Age*SLES;",
  ANALYSIS ="TYPE = TWOLEVEL;
    ESTIMATOR = MLR;",
  MODEL = "
    %WITHIN%
    ROI ON  Stim Instr CBCL SLES Age 
             Cond StimAge InstAge AgeSLES
             StimSLES InstSLES CondSLES 
             CondAge InsAgeSL StimAgeS CondAgeS;
             CBCL SLES AgeSLES StimSLES InstSLES 
             CondSLES InsAgeSL StimAgeS CondAgeS;
    %BETWEEN%
    ROI ON Sex;",     
  OUTPUT = "STANDARDIZED CINTERVAL;",
  rdata = roiData)