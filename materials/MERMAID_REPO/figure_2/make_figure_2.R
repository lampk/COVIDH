

source("simulation_scripts/plot_2panel_estimates_and_SE_over_time.R")

# Read aggregated simulation data
sim_summ_dt <- readRDS("data/simulations/sim_calib_data.rds")

# Specify suffix for output files
out_suffix <- ""

# Specify output directory 
out_dir <- "figure_2/"

# Specify seroprevalence likelihood weighting factor used to fit model
sero_lik_weight = 25

# Specify plot dimensions
pl_height <- 6
pl_width <- 8

# ----------------------------------
# Rt

## Create plot for Rt estimates 

xlab_list <- list(
	NULL, expression("True "~italic(R[t])), expression("Standard deviation of"~hat(italic(R[t])))
)

ylab_list <- list(
	expression(italic(R[t])), 
	expression("Mean"~hat(italic(R[t]))),
	expression("Mean estimated SE of "~hat(italic(R[t])))
)

param_list <- c("Rt", "Rt_fit_0", "Rt_fit_1")

tmp <- subset(sim_summ_dt, param %in% param_list)

cbn <- make_2panel_plot(data = tmp, param_list, xlab_list = xlab_list, ylab_list = ylab_list)

ggsave(paste0(out_dir, "rt_simulation_results",out_suffix,".pdf"), height = pl_height, width = pl_width, cbn)

