# Growth-Metab-MV-2025


Data and scripts in the repository relate to the following research experiment:
'Exploring metabolism-growth relationships within and across individual fish'
Elizabeth C. Hoots, Luis L. Kuchenmueller, Peter A. Biro, Timothy D. Clark
*corresponding author: Elizabeth C. Hoots

This R project is used to take raw respirometry slopes measured on *Galaxias maculatus* at Deakin's Marine Research Centre in Queenscliff, Victoria, Australia and calculate their standard, routine, and maximum metabolic rate. These metabolic measurements are made repeatedly on known individuals held under one of two temperature treatments (18°C or 23°C), and paired with repeated mass and length measurements over five month of data collection (May - September, 2023). After reading in all data, a multivariate mixed effects modeling approach is undertaken in order to assess covariance between growth and metabolism traits among- and within-individuals.

This data and code supplement contains the following folders: 

00_raw_data: This folder contains all raw data collected in this project, as well as the data outputs from the first stage of the R analysis

	* Firesting Text Files: this folder contains text files saved directly from the Pyroscience oxygen logging software. Files are named with the date (YYYYMMDD) followed by the respirometry chamber (or Firesting unit) where the measurements were recorded. These are modified with an R Script (STEP 1 - Firesting to Labchart) to reformat them for LabChart.
	* Labchart Files: this folder contains all raw LabChart files (format LabChart Data File), named with the date, followed by "CoralAll" indicating that all small "coral" respirometers are merged into that file. Separate files with measurements from the MMR recording period are denoted with "_MMR". These are modified with an R Script (STEP 2 - Labchart to RespScript) to reformat them for making respirometry calculations.
	* MMR: this folder contains raw slopes extracted in LabChart as well as temperature and related background respiration measurements that will be used to calculate MMR for each fish. Files are named with the date, followed by "_MMR".
	* MR_Slopes: this folder contains all slopes extracted in LabChart collected every 20 minutes for use in calculating SMR and RMR, as well as their associated temperature and background respiration measurements. Files are named with the date, followed by "_MR_slopes"
	* MRcalcs: this folder contains output files from the R analysis (stage 1 of markdown file, or STEP 3 - RespScript_20231114). Files are named with the date followed by "_MRcalcs" and include calculations of SMR, RMR (and its associated variance, % variance), and MMR for each fish, along with metadata including the fish ID numbers, their mass and length, what temperature the system was at and what chamber each fish was in. This file also includes chamber volume adjusted for fish mass, as well as start and end times for each trial.
	* BinCounts: this excel file gives for each month and rack/bin combination (merged as tankID) the number of fish lost total (from initial 8 per tank), the number of fish lost in each month period, and the population of fish. A final column, Population_tagged, indicates the number of experimental fish in each tank, which only applies to tankID 3_8 where two untagged fish were added to supplement the population after the May respirometry trial.
	* FinalBiometrics: this excel file gives for each sampled fish (denoted by ID_fish as well as its unique TagID within each rack/bin combination) its final mass, final length, gonad mass, sex (M = male, F = Female, I = Immature), fat mass, ventrical mass, and age in years based on otolith assessment. Otoliths and ventricals were only extracted for a subset of fish, all other metrics apply to all fish which survived until the end of the experiment.

01_metadata: This folder contains all metadata associated with this project, including monthly digitized "notebooks", the final sampling datasheet (blank template and filled in), all temperature data logged in the holding tanks throughout the experiment, otolith ageing data, maturation assessments by GutRatio, and a duplicate copy of BinCounts as described above.

02_r_scripts: This folder contains the following R scripts used to reformat datasheets (note that a further markdown file compiling the steps to make metabolic calculations and run analysis for this study is included separately, not in a folder).
	* STEP 1 - Firesting to Labchart: this script was used to reformat text files from Firesting to be opened in Labchart.
	* STEP 2 - Labchart to RespScript: this script merges together all slopes extracted in LabChart for each fish with its metadata (mass, length etc.) into files which are saved into the MR_slopes folder for further analysis.

03_figures: This folder contains the output figures from the R project and associated markdown file "Growth-metabolism-MV-2025"

Additionally, uploaded here are an R project file, "Growth-Metab-MV-2025" which includes the code for calculating metabolic rates and all data analysis including the multivariate modeling assessment and related covariance matrix. An html file titled "Growth-metabolism-MV-2025" shows all analyses undertaken here with annotated code, and the associated RMD file is also included here for use within the R project. 
