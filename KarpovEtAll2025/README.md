# Karpov, Heyn, & Herringa, 2025

This folder contains the code used in the following paper: 
Karpov, G., Heyn, S.A., & Herringa, R.J. (under review). Neurodevelopmental correlates of emotion regulation in adolescence: An accelerated longitudinal study.

### Overview of files
1. `LinearMixedEffects_Analysis.R` - R code detailing the data transformation and statistical models. Mplus software runs the models specified in the code.

2. `ROIs_atlas_Schaefer2018_desc-400Parcels7Networks.txt` - List of all 122 frontal lobe regions of interest as taken from the Schaefer Atlas (Schaefer et al., 2018), including the atlas index number. Accessed from: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI
> Schaefer A, Kong R, Gordon EM, Laumann TO, Zuo XN, Holmes AJ, Eickhoff SB, Yeo BTT. Local-Global parcellation of the human cerebral cortex from intrinsic functional connectivity MRI. Cerebral Cortex, 29:3095-3114, 2018. 

3. `fmri_Preprocessing.sh` - Bash code detailing the preprocessing of MRI data before statistical analysis. Calls fMRIPrep and AFNI. 
