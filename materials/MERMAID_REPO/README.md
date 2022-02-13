

# MERMAID Code and Data 

This repository contains code for fitting for the Multilevel Epidemic Regression Model to Account for Incomplete Data (MERMAID), and scripts for simulation studies, data pre-processing, and analysis for the manuscript, "Regression Models for Understanding COVID-19 Epidemic Dynamics with Incomplete Data."

## Directories

* `src/`  contains R scripts for fitting the MERMAID model. 
* `figure_*/` contain TeX and R scripts for recreating plots in the manuscript. 
* `simulation_scripts/` contains R scripts for simulation studies presented in the manuscript. 
* `US_analysis_scripts/` contains R scripts for pre-processing and analysis of COVID-19 reported case counts and seroprevalence surveys in the US from February to December 2020. 
* `data/` contains raw and preprocessed data files. 

## Dependencies

* LaTeX and the TikZ package for Figure 1. 
* R version 3.6.3 and the following R packages: "Matrix", "data.table", "EpiEstim", "ggplot2", "gridExtra", "cowplot".
	* To install any missing packages, run `sapply(setdiff(c("Matrix", "data.table", "EpiEstim", "ggplot2", "gridExtra","cowplot"), installed.packages()), install.packages)`

## Workflow
### Generating figures
The workflow below uses bash CLIs to run analyses and generate figures in the main text. 

* To generate Figure 1 using pdfTeX, run `pdflatex --output-directory=figure_1/  figure_1/figure_1.tex`. 
* To  generate Figures `i = 2, 3` run the commands `Rscript figure_${i}/make_figure_${i}.R`.
* To fit the model for MI and generate Figure 4, run `bash figure_4/make_figure_4.sh`. 
* To fit the model for TX and generate Figure 5, run `bash figure_5/make_figure_5.sh`. 
* To  generate Figures `i = 6, 7` run the commands `Rscript figure_${i}/make_figure_${i}.R`.
	* To generate the data for Figure 7, run `Rscript figure_7/fit_bs_models_figure_7.R`

### Running simulations
* To generate output for a single simulation replicate of a given model model, run `Rscript simulation_scripts/run_simrep_custom_${type}.R 1 $rep`
	* `${type}` specifies the Rt regression model, which can be `PC1` or `PC2` for piecewise constant models 1 or 2, or `NSS` for smooth spline models. 
	*  The first command line argument specifies the number of regions (fixed at 1 in the paper), and the second argument (`$rep`) specified the simulation replicate number, to be included in output file names.  
* The script `simulation_scripts/load_sim_fun.R` provides R functions for aggregating 
 
### Fitting single-state models
* To fit the models and generate plots for all US states, run the bash command `for i in {1..51}; do Rscript US_analysis_scripts/fit_state.R ${i}; done`
	* Warning: This may take a while. 

## Contact

Author contact information: corbinq AT gmail.com, rdey AT hsph.harvard.edu, or xlin AT hsph.harvard.edu.

## Package citations

  R Core Team (2020). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

  Anne Cori (2020). EpiEstim: Estimate Time Varying Reproduction
  Numbers from Epidemic Curves. R package version 2.2-3.
  https://CRAN.R-project.org/package=EpiEstim

  Douglas Bates and Martin Maechler (2019). Matrix: Sparse and Dense
  Matrix Classes and Methods. R package version 1.2-18.
  https://CRAN.R-project.org/package=Matrix

  Matt Dowle and Arun Srinivasan (2019). data.table: Extension of
  `data.frame`. R package version 1.12.8.
  https://CRAN.R-project.org/package=data.table

  H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
  Springer-Verlag New York, 2016.

  Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for "Grid"
  Graphics. R package version 2.3.
  https://CRAN.R-project.org/package=gridExtra

  Claus O. Wilke (2019). cowplot: Streamlined Plot Theme and Plot
  Annotations for 'ggplot2'. R package version 1.0.0.
  https://CRAN.R-project.org/package=cowplot
