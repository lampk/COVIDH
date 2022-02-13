

source("simulation_scripts/plot_2panel_estimates_and_SE_over_time.R")

# Read aggregated simulation data
sim_summ_dt <- readRDS("data/simulations/sim_calib_data.rds")

# Specify suffix for output files
out_suffix <- ""

# Specify output directory 
out_dir <- "figure_3/"

# Specify seroprevalence likelihood weighting factor used to fit model
sero_lik_weight = 25

# Specify plot dimensions
pl_height <- 6
pl_width <- 8

# ----------------------------------
# Ascertainment

## Create plot for ascertainment (Pi_it) estimates

xlab_list <- list(
	NULL, expression("True "~italic(pi[t])), expression("Standard deviation of"~hat(italic(pi[t])))
)

ylab_list <- list(
	expression(italic(pi[t])), 
	expression("Mean"~hat(italic(pi[t]))),
	expression("Mean estimated SE of "~hat(italic(pi[t])))
)

param_list <- c("pi_t", "pi_t", "pi_t")

tmp <- subset(sim_summ_dt, param %in% param_list)

# Adjustment factor
tmp$SE <- sqrt(sero_lik_weight)*tmp$SE

cbn <- make_2panel_plot(data = tmp, param_list, xlab_list = xlab_list, ylab_list = ylab_list, MERMAID_only = TRUE)

ggsave(paste0(out_dir, "panel_A_pi_simulation_results",out_suffix,".pdf"), height = pl_height, width = pl_width, cbn)


# ----------------------------------
# Incidence

## Create calibration plot for incidence (daily new infections) estimates

Const <- 100/(8000000)

xlab_list <- list(
	NULL, 
	expression("True daily incidence (%)"), 
	expression("Standard deviation of estimates")
)

ylab_list <- list(
	expression("Daily incidence (%)"), 
	expression("Mean of estimates"),
	expression("Mean estimated SE")
)

param_list <- c("Infections", "Infections_0", "Infections_1")

tmp <- subset(sim_summ_dt, param %in% param_list)

kp_var <- subset(tmp[,unique(variable),by=param][,.N,by=V1], N == length(param_list))$V1
tmp <- subset(tmp, variable %in% kp_var)

tmp[,unique(variable),by=param]
# Scale as percentage
tmp$SE <- Const*tmp$SE
tmp$Mean <- Const*tmp$Mean
tmp$Truth <- Const*tmp$Truth
tmp$Bias <- Const*tmp$Bias
tmp$StdDev <- Const*tmp$StdDev

cbn <- make_2panel_plot(data = tmp, param_list=param_list, xlab_list = xlab_list, ylab_list = ylab_list, scales='free', ylim=c(0,NA))

ggsave(paste0(out_dir, "panel_B_incid_simulation_results",out_suffix,".pdf"), height = pl_height, width = pl_width, cbn)


# ----------------------------------
# Prevalence

## Create plot for prevalence (prob. any past infection) estimates

Const_prev <- 100

xlab_list <- list(
	NULL, 
	expression("True cumulative prevalence (%)"), 
	expression("Standard deviation of estimates")
)

ylab_list <- list(
	expression("Cumulative prevalence (%)"), 
	expression("Mean of estimates"),
	expression("Mean estimated SE")
)

param_list <- c("Prevalence", "Prevalence_0", "Prevalence_1")

tmp <- subset(sim_summ_dt, param %in% param_list)
# Scale as percentage
tmp$SE <- Const_prev*tmp$SE
tmp$Mean <- Const_prev*tmp$Mean
tmp$Truth <- Const_prev*tmp$Truth
tmp$Bias <- Const_prev*tmp$Bias
tmp$StdDev <- Const_prev*tmp$StdDev

cbn <- make_2panel_plot(data = tmp, param_list=param_list, xlab_list = xlab_list, ylab_list = ylab_list, scales='free', ylim=c(0,NA))

ggsave(paste0(out_dir, "panel_C_prev_simulation_results",out_suffix,".pdf"), height = pl_height, width = pl_width, cbn)


