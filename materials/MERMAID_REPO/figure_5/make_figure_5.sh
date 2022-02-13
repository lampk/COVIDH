#!/usr/bin/bash

## Fit MERMAID model and write plot for Texas (state #44)

Rscript US_analysis_scripts/fit_state.R 44 figure_5

# Rename output file
mv figure_5/State_TX_Model_single.fit.rds_plot_TX_s1.pdf figure_5/figure_5.pdf


