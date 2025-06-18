#!/bin/bash

# fMRI preprocessing script used in:
# (Citation here)

# Done in 3 steps:
#       1) Run basic preprocesing in fMRIPrep (v23.2.0)
#       2) Extract necessary outputs from fMRIPrep
#       3) Run first-level model in AFNI (v24.3.06)
#-------------------------------------------------------------------------

## Step 1: fMRIprep command
fmriprep ${BIDS_dataDIR} ${fMRIPrep_outputDIR} participant     \
    --participant-label "$SubID" --longitudinal --use-syn-sdc  \
    --output-spaces MNI152NLin6Asym:res-2

## Step 2: Extract necessary information from fMRIPrep
    #E.g. Motion parameters, nuisance regressors (CSF, White matter), censored volumes

## Step 3: Run final preprocessing and first-level modeling in AFNI
#-----------------EN-Back Task----------------------------------
afni_proc.py -subj_id "$SubID"                                  \
        -out_dir ${outputDIR}/sub-${SubID}                      \
        -blocks mask scale regress                              \
        -copy_anat $anatomical_file                             \
        -dsets  $BOLD_files                                     \
        -tcat_remove_first_trs 5                                \
        -regress_motion_file "$motion_file"                     \
        -regress_motion_per_run                                 \
        -regress_stim_times                                     \
            "$stimPosFace0B" "$stimPosFace2B"                   \
            "$stimNegFace0B" "$stimNegFace2B"                   \
            "$stimNtrFace0B" "$stimNtrFace2B"                   \
            "$stimPlace0B" "$stimPlace2B"                       \
            "$instruction" "$response"                          \
        -regress_stim_labels                                    \
            PosFace0B PosFace2B                                 \
            NegFace0B NegFace2B                                 \
            NtrFace0B NtrFace2B                                 \
            Place0B Place2B                                     \
            Intstruction Response                               \
        -regress_basis_multi                                    \
            'BLOCK(25,1)' 'BLOCK(25,1)'                         \
            'BLOCK(25,1)' 'BLOCK(25,1)'                         \
            'BLOCK(25,1)' 'BLOCK(25,1)'                         \
            'BLOCK(25,1)' 'BLOCK(25,1)'                         \
            'GAM(8.6,.547,2.5)' 'GAM(8.6,.547)'                 \
        -regress_extra_ortvec "$CSF_file" "$WM_file"            \
        -regress_extra_ortvec_labels csf whitematter            \
        -regress_stim_times_offset 0.8                          \
        -regress_censor_extern "$censor_file"                   \
        -regress_local_times                                    \
        -regress_reml_exec                                      \
        -regress_compute_fitts                                  \
        -regress_make_ideal_sum sum_ideal.1D                    \
        -regress_run_clustsim no                                \
        -html_review_style pythonic                             \
        -execute

#---------------Perspective Task--------------------------------
afni_proc.py -subj_id "$SubID"                                  \
        -out_dir ${outputDIR}/sub-${SubID}                      \
        -blocks mask scale regress                              \
        -copy_anat $anatomical_file                             \
        -dsets  $bold_files                                     \
        -tcat_remove_first_trs 5                                \
        -regress_motion_file "$motion_file"                     \
        -regress_motion_per_run                                 \
        -regress_stim_times                                     \
            "$stimNegClose" "$stimNegFar"                       \
            "$stimNtrClose" "$stimNtrFar"                       \
            "$rate"                                             \
        -regress_stim_labels                                    \
            NegClose NegFar NtrClose NtrFar Rating              \
        -regress_basis_multi                                    \
            'GAM(8.6,.547, 8)' 'GAM(8.6,.547, 8)'               \
            'GAM(8.6,.547, 8)' 'GAM(8.6,.547, 8)'               \
            'GAM(8.6,.547, 5)'                                  \
        -regress_extra_ortvec "$CSF_file" "$WM_file"            \
        -regress_extra_ortvec_labels csf whitematter            \
        -regress_stim_times_offset 0.8                          \
        -regress_censor_extern "$censor_file"                   \
        -regress_local_times                                    \
        -regress_reml_exec                                      \
        -regress_compute_fitts                                  \
        -regress_make_ideal_sum sum_ideal.1D                    \
        -regress_run_clustsim no                                \
        -html_review_style pythonic                             \
        -execute

# Note: 'regress_stim_times_offset' is the TR divided by two (1.6/2), 
#       as fMRIPrep defaults slice-timing correction to
#       the middle of the TR. 