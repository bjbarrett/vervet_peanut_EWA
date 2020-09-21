# vervet_peanut_EWA
#data and code for vervet peanuts EWA

1) raw data folder contains individual level variables and raw behavioral data needed to create data frame used for EWA analysis

2) peanut_data_cleaning.R contains code necessary for creating the EWA analyzed dataset of varying time windows

3) Peanut_Vervet_XXmin.csv are files used in EWA analysis with XX long window widths

4) ewa_cue.stan model is stan code for the payoff and cue-biased learning models

5) ewa_freq.stan model is stan code for the frequency-dependent learning model

6) ewa_individual.stan model is stan code for the reinforcement aka individual learning model

7) ewa_global.stan model is stan code for the global log0linear additive model

8) peanut_data_fits.R is R code to fit stan models

9) peanut_data_graphs.R is code to reproduce graphs in paper, plus some additional diagnostic graphs no in paper

10) script_GLM_latency.R in GLM script for latency stats

11) latency_success_peanuts.csv is data for above GLMs
