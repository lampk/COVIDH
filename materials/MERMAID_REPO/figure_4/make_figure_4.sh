#!/usr/bin/bash

## Fit MERMAID model and write plot for Michigan (state #23)

Rscript US_analysis_scripts/fit_state.R 23 figure_4

# Rename output file
mv figure_4/State_MI_Model_single.fit.rds_plot_MI_s1.pdf figure_4/figure_4.pdf

