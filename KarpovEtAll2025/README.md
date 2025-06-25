# Karpov, Heyn, & Herringa, 2025

This folder contains the code used in the following paper: 
Karpov, G., Heyn, S.A., & Herringa, R.J. (under review). Neurodevelopmental correlates of emotion regulation in adolescence: An accelerated longitudinal study.

### Overview of files
1. `LinearMixedEffects_Analysis.R` - R code detailing the data transformation and statistical models. Mplus software runs the models specified in the code.

2. `ROIs_atlas_Schaefer2018_desc-400Parcels7Networks.txt` - List of all 122 frontal lobe regions of interest as taken from the Schaefer Atlas (Schaefer et al., 2018), including the atlas index number. Accessed from: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI
> Schaefer, A., Kong, R., Gordon, E. M., Laumann, T. O., Zuo, X.-N., Holmes, A. J., Eickhoff, S. B., & Yeo, B. T. T. (2018). Local-Global Parcellation of the Human Cerebral Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9), 3095–3114. https://doi.org/10.1093/cercor/bhx179 

3. `fmri_Preprocessing.sh` - Bash code detailing the preprocessing of MRI data before statistical analysis. Calls fMRIPrep and AFNI. 
