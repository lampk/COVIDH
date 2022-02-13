#!/usr/bin/env Rscript

library(splines)
library(usmap)
library(fastglm)
library(data.table)
library(Matrix)

source("src/helper_functions.R")
source("src/cumulative_incidence.R")
source("src/fit_MERMAID.R")

options(stringsAsFactors=F)

# Takes a single integer as a command-line argument
args <- commandArgs( TRUE )
i_state <- as.integer(args[1])

# Base directory of the github repository 
base_dir <- getwd() ##
setwd(base_dir)

# Directory where output files will be written
if( length(args) >= 2 ){
	out_dir <- args[2]
}else{
	out_dir <- paste0(base_dir, "/US_output")
}
dir.create(out_dir, showWarnings = FALSE)

# States to be excluded
exclude_states <- c('AK', 'HI', 'MT', 'ND', 'SD', 'VT', 'WY')

#------------------------------------------
# Set fixed parameters
#------------------------------------------

# onset/reporting lag distribution
lag_p <- dnbinom(0:20, mu = 5, size = 5)
lag_p <- lag_p/sum(lag_p)

# serial interval distribution
si_weights <- discr_si(1:30, mu = 4.7, sigma = 2.9)

# time period to include
start_date <- "2020-03-15"
end_date <- "2021-01-01"

# Rt spline knots (if applicable)
spline_df = 9
date_knots <- seq(as.Date(start_date) + 30, as.Date(end_date) - 30, length.out = spline_df)

# Rt regression model
Rt_model <- "single"
rt_eqn <- ~ 1 + bs(date, knots = date_knots)

# Value to initialize ascertainment prob parameter
init_ascertain_prob <- 0.5

# pi mean model
ctr <- function(x) scale(x, scale = FALSE, center = TRUE)
pi_eqn <- ~ ctr(log(ntest_w))


# ---------------------------------------------
# Load data
# ---------------------------------------------

# Load table of US states & regions
dt_state <- readRDS("data/US_processed/dt_states.rds")

# Extract sorted list of US states
all_states <- sort(unique(subset(dt_state, !is.na(region))$state))

# Extract names of states to analyze
states <- all_states[i_state]

#-------
# Check for issues with state specification

if( length(states) == 0 ) stop(paste("State not specified (max index =",length(all_states),")."))

if( Rt_model == "single" & length(states) > 1 ) stop("Multiple states in single-state model.")

if(is.na(states)) stop("State undefined.")

if( any(states %in% exclude_states) ){
	stop(paste(c("Specified states",intersect(states, exclude_states), "are on exclusion list."), collapse = " "))
}

#-------

# Define output file name
out_name <- paste0(out_dir, '/', paste0("State_", states, "_Model_", Rt_model), ".fit.rds")

# If output file name already exists, exit now
if( file.exists(out_name) ){
	cat(paste0(states, " already complete.\n"))
	q( save = 'no' )
}

#-------
# Load merged case count data
#-------
dt <- as.data.table(readRDS("data/US_processed/merged_final_US_S2S.rds"))

dt[,date := as.Date(date),]
data.table::setorder(dt, state, date)

# subset to dates prior to the end of the year
dt <- subset(dt, date < end_date)

# subset to the state(s) of interest
dt <- subset(dt, state %in% states)

#-------
# Load seroprevalence survey data
#-------

sero_dt <- subset(fread("data/US_processed/merged_sero_data.csv"), state %in% states)

# format and rename variables 
sero_dt <- sero_dt[,list(
        npos = pos_count,
        ntested = sample_size,
        state = state,
        date = as.Date(date),
        lower = prev_lb,
        upper = prev_ub,
        pop_size = pop_size
),][order(state,date)]

sero_dt <- subset(sero_dt, date < end_date & !is.na( lower + upper + npos + ntested ) )


# Create data set with key variables
odt <- dt[,list(
	date, 
	incid = incid_S2S, 
	incid_w = incid_S2S, 
	pop_size = pop_size, 
	ntest_w = 1.00 + ntest_S2S,
	ppos_w = data.table::shift(
		data.table::frollmean( npos_S2S, n = 7, align = 'right', fill = 0.0)/data.table::frollmean( ntest_S2S, n = 7, align = 'right', fill = 1)
        , n = 1)
), by = list(state)]

odt <- odt[order(state,date)]

# Initialize the infection potential
odt[,
	initialize_Lambda := max(
	calc_Lam(y = fill_na(incid[date <= start_date]), t = length(incid[date <= start_date]), w = si_weights, fill = 0)/init_ascertain_prob,
	max(1, pop_size[1] * 1e-6)
	),
	by=state
]


# -------------------------------
# Calculate value to initialize pi (heuristic)

tmp_s <- subset(sero_dt, state %in% states)
tmp_c <- subset(odt, state %in% states)

get_c <- function(date_){
  sum(subset(tmp_c, date <= date_)$incid, na.rm = TRUE)
}

init_pi_bar <- with(tmp_s[,list(
        pi_tilde_hat = get_c(date)/max(pop_size * npos/ntested, na.rm = TRUE),
        weight = ntested
),by=date], sum(pi_tilde_hat * weight)/sum(weight))



# ------------
# model fitting options
print("Now fitting model")

print(rt_eqn)

options_list <- list(
	rt_formula = rt_eqn,
	pi_formula = pi_eqn,
	pi_intercept = 0, 
	pi_init_mean = init_pi_bar,
	min_pi_t = 1e-6,
	pi_eps = 1e-6,
	data = subset(odt, date >= start_date & !is.na(incid + ntest_w)),
	max_lag = length(lag_p)-1,
	prev_data = subset(sero_dt, date >= start_date),
	prev_weight = 25,
	lag_probs = lag_p,
	si_weights = si_weights,
	si_nts = length(si_weights),
	confirmed_cases_var = "incid",
	date_var = "date",
	init_Lambda = "initialize_Lambda",
	adj_Lambda = 1,
	subset_var = NULL,
	region_var = "state",
	pop_size_var = "pop_size",
	max_it_E_step = 50,
	tol = 1e-4,
	max_it = 5000,
	plot_every = NA,
	recovered_offset = TRUE,
	recycle_start = FALSE,
	return_vcov = TRUE,
	accelerated_em = TRUE,
	reweight_prev = FALSE,
	prev_method = 'hyper',
	print_mem = TRUE
)

print(paste("Now fitting:", states))

state_fit <- do.call(fitMERMAID, options_list)


require(ggplot2)
require(gridExtra)

make_plot <- function(pl_data){

	# Width of confidence bands ( Estimate +/- adj_se * SE)
	adj_se <- 2 
	
	# Transparency of confidence bands
	transp <- 0.2

	ggplot(pl_data, aes(x = date)) + geom_line(colour='red', aes(y = ppos_w)) + geom_line(colour='black', aes(y = c_cases/max(c_cases)))

	p1 <- ggplot(pl_data,  aes(x = date, y = R_t,  ymin = R_t - adj_se*R_t_SE, ymax = R_t + adj_se*R_t_SE )) + theme_bw() + geom_ribbon(colour = NA, alpha = transp, fill = "blue") + geom_line(colour = "blue") + xlab(NULL) + ylab(expression(italic(R[t]))) + geom_hline(yintercept=1, colour="black", linetype="dashed") + guides(colour=F,fill=F) + coord_cartesian(expand = FALSE, ylim = c(0, 2.5)) + facet_grid(rows="region") + geom_line(aes(y = Infections/Lambda), colour = 'darkgreen', size = 0.5, linetype = 'dotted')

	pl_data$Infections_SE[is.na(pl_data$Infections_SE)] <- 0

	sero_dt_ss <- subset(sero_dt, state %in% pl_data$state)
	sero_dt_ss$region <- sero_dt_ss$state

	pprev <- ggplot(pl_data,  aes(x = date, y = Prevalence,  ymin = Prevalence - adj_se*Prevalence_SE, ymax = Prevalence + adj_se*Prevalence_SE )) + theme_bw() + geom_ribbon(colour = NA, alpha = transp, fill = "magenta") + geom_line(colour = "magenta") + xlab(NULL) + ylab("Prevalence") + guides(colour=F,fill=F) + facet_grid(rows="region")+ geom_errorbar(data = sero_dt_ss, aes(ymin = lower, ymax = upper, x = date), width = 0, size = 1, colour = 'green', inherit.aes = FALSE) + geom_point(data = sero_dt_ss, aes(y = npos/ntested, x = date), inherit.aes = FALSE, colour = 'black', size = 2) + geom_point(data = sero_dt_ss, aes(y = npos/ntested, x = date), inherit.aes = FALSE, colour = 'green', size = 1.5) + coord_cartesian(expand = FALSE)

	dp2 <- melt(pl_data[,list("Predicted infections" = Infections, "Confirmed cases" = c_cases, "Unascertained/asymptomatic" = u_cases, date, region ,ppos_w,Infections_SE),], id.var = c("date", "region", "ppos_w", "Infections_SE"))

	dp2$Infections_SE[dp2$variable != "Predicted infections"] <- NA

	p2 <- ggplot(dp2,  aes( x = date, ymin = 0, colour = variable, fill = variable, y = value, ymax = value)) + theme_bw() + xlab(NULL) + ylab("New cases") + guides(colour=guide_legend(title=NULL),fill=guide_legend(title=NULL)) + geom_ribbon(colour = NA, alpha = transp) + facet_grid(rows="region") + geom_point(data = subset(dp2, date <= min(date[!is.na(Infections_SE)]) | variable=="Predicted infections"), size = 0.6,shape=15)  + theme(legend.position = "top") + geom_errorbar(aes( ymin = value - adj_se*Infections_SE, ymax = value + adj_se*Infections_SE ), width = 0) + coord_cartesian(expand=FALSE)

	dp2 <- melt(pl_data[,list("Ascertainment" = pi_t, "Test positivity" = ppos_w, "No. tests" = (ntest_w/pop_size)/max((ntest_w/pop_size)),pi_t_SE, date, region),], id.var = c("date", "region", "pi_t_SE"))

	dp2$pi_t_SE[dp2$variable != "Ascertainment"] <- NA

	p3 <- ggplot(dp2,  aes(colour = variable, y=value, x = date)) + theme_bw() + geom_line() + scale_colour_manual(values = c("blue", "red", "black")) + geom_ribbon(data = subset(dp2, variable=="Ascertainment" ), aes(ymin = value - adj_se*pi_t_SE, ymax = value + adj_se*pi_t_SE), colour = NA, fill = "blue", alpha = transp) + guides(colour=guide_legend(title=NULL)) + theme(legend.position = "top") + xlab(NULL) + coord_cartesian(expand=FALSE) + facet_grid(rows="region") 
	
	ff <- function(x) {
	x + theme_minimal()+ theme(legend.position = "top") + theme(axis.line = element_line(colour = "black"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	panel.border = element_blank(),
	panel.background = element_blank())
	}

	pl_1 <- grid.arrange(ff(p2),
	grid.arrange(ff(p1),ff(p3), ff(pprev), nrow = 1, widths = c(2,2,2))
	,nrow=2
	)

	return(invisible(list(combined = pl_1, y_plot = p2, rt_plot = p1, pi_plot = p3, prev_plot = pprev)))

}


plot_list <- list()

uni_states <- unique(state_fit$data$state)

for(st in uni_states ){

	pl_data <- subset(state_fit$data, state == st & obs_weight > 0.8)
	plot_list[[st]] <- 	make_plot(pl_data)

	dev.off()
	pl_1 <- plot_list[[st]]$combined

	plot_prefix <- paste0(out_name, "_plot_", st)


	ggsave(paste0(plot_prefix, "_s1.svg"), height = 6, width = 12, pl_1)
	ggsave(paste0(plot_prefix, "_s1.pdf"), height = 6, width = 12, pl_1)
	ggsave(paste0(plot_prefix, "_s1.png"), height = 6, width = 12, pl_1)

}

saveRDS(
        object = list(fit = state_fit, options = options_list, plot_list = plot_list),
        file = out_name
)
