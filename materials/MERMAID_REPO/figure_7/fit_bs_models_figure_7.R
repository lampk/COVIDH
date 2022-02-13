
require(data.table)
require(ggplot2)
require(splines)

source("src/glm_family_c.R")

# -----------------
# Note: 
#  As described in supplementary materials, fitting a Poisson 
#  GLM to MERMAID predicted values, E(Y|C,K), from single-state models 
#  is equivalent to running the EM to convergence under a saturated 
#  model (where no parameters are shared across states), and then 
#  running a single M-step iteration under the constrained model with 
#  shared Rt parameters. Here, we use a block bootstrap over states to 
#  obtain confidence bands. 
# -----------------


#-----------------
# Load data 
#-----------------

# Read US state-level policy time series
dt_policy <- readRDS("data/US_processed/US_policy_time_series_data.rds")

# Column descriptions for data/US_policy_time_series_data.rds :

#	state = two-letter state abbreviations
#	pop_size = state population size
#   [Remaining columns] = policy level time series from OxCGRT

# Read fitted values from single-state MERMAID fits
dt_state_fits <- readRDS("data/US_processed/US_single_state_fitted_values.rds")

# Column descriptions for data/US_single_state_fitted_values.rds :

#	state = two-letter state abbreviations
#	pop_size = state population size
#	date = date
#	c_cases = confirmed infections
#	y_cases = predicted total infections (MERMAID)
#	Lambda = predicted infection potential (MERMAID)
#   R_t = estimated effective reproductive number R_t (MERMAID)
#   pi_t = estimated ascertainment probability (MERMAID)

# Merge policy time series and fitted values
dm <- as.data.frame(merge(
	x = dt_state_fits[,list(state, date, y_cases, Lambda),],
	y = dt_policy,
	all.x = FALSE, all.y = FALSE,
	by = c("state", "date")
))

rm(dt_state_fits)
rm(dt_policy)

#-------------------------------------------------------

# function to sample state blocks with replacement
bootstrap_states <- function(data){

	groups <- unique(data$state)
	N <- length(groups)

	do.call(rbind, lapply(1:N, function(i){
		s_i <- sample(groups, 1)
		out <- data[data$state == s_i,]
		out$state <- paste0("state_", i)
		out
	}))

}


# time period to include
start_date <- "2020-03-15"
end_date <- "2021-01-01"

# Rt spline knots (where applicable)
spline_df = 9
date_knots <- seq(as.Date(start_date) + 30, as.Date(end_date) - 30, length.out = spline_df)


# baseline model formula: state-specific intercepts and shared policy effects only 
C_base_eqn <- ~  as.character(state) + factor(Facial_coverings) + factor(Stay_home) + factor(Close_public_transport) + factor(Gathering_restrictions_collapsed) + factor(Workplace_closing) 

# Model 0: state-specific intercepts and shared policy effects only 
bs_formula_0 <- update.formula(C_base_eqn, y_cases~offset(log(Lambda))+.)

# Model 2: add a shared US-wide time trend
bs_formula_2 <- update.formula(C_base_eqn, y_cases~offset(log(Lambda))+splines::bs(date, knots = date_knots)+.)

# --------------------------------

fit_0 <- glm(formula = bs_formula_0, data= dm, family = poisson_c, na.action = 'na.omit')

fit_2 <- glm(formula = bs_formula_2, data= dm, family = poisson_c, na.action = 'na.omit')

# which coefficient names to keep
keep_coef_names <- names(coef(fit_0)[grepl("^factor",names(coef(fit_0)))])

# Function to fit model and extract estimates for a single bootstrap replicate
fit_bs_rep <- function(iter, fit_formula, kp_coef = keep_coef_names){
	cat(paste(iter, '             \r'))
	fit_bs <- glm(
		formula = fit_formula, 
		data= bootstrap_states(dm), 
		family = poisson_c, 
		na.action = 'na.omit'
	)
	coef(fit_bs)[kp_coef]
}

bs_coef_0 <- do.call(rbind, lapply(1:1000, fit_bs_rep, fit_formula = bs_formula_0))

saveRDS(bs_coef_0, "figure_7/US_bootstrap_model_0.rds")

bs_coef_2 <- do.call(rbind, lapply(1:1000, fit_bs_rep, fit_formula = bs_formula_2))

saveRDS(bs_coef_0, "figure_7/US_bootstrap_model_2.rds")
