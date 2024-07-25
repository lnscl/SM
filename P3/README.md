This repository containes the code files associated to the following paper submitted for review: Lines and Werner: How useful are seasonal forecasts for farmers facing drought? A user-based modelling approach

The code used for the analysis described in the paper consists of three files:

   -  run_model_8c.Rmd: This is the main file which contains the code for the decision model and most of the figures included in the results section of the paper.
   -  WB.Rmd: This file containes the code for the water balance. The results of the water balance are used as input for run_model_8c.Rmd
   - F1_score: This file containes the code to calculate the forecast verification scores (as described in section 2.4 of the paper) and to create the associated figure.

The data files used as input for the code are included in subfolder input_files:

   - table_wr: table of water requirements for alfalfa (ALF), peach (PCH), LCM (long-cycle maize) and SCM (short-cycle maize) in mm from Cropwat (column name: crop irrigation schedule - Gr. Irr).
   - 9013_9047_accuvol.txt: accumulated inflow volumes since the beginning of the season, combining data from two measurement stations (9013 - Graus and 9047 - Capella).
   - percentiles_9013_9047_accuvol: percentiles for the accumulated inflow volumes
   - SFprediction_ARIMA_v3.csv: bias corrected seasonal forecast data (method described in section 2.2 of the paper).

The subfolder seasonal forecast contains seasonal forecast data (raw and bias corrected) sampled to the catchment.

