
# set up ------------------------------------------------------------------

# library
library(splines)
library(fastglm)
library(data.table)
library(Matrix)
library(gridExtra)
library(ggplot2)
library(kza)

options(stringsAsFactors = FALSE)

# Directory where output files will be written
out_dir <- file.path("output", "MERMAID", "HCMC_output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Data path
data_path <- file.path("materials", "MERMAID-master", "data")

# Code
source(file.path("R", "MERMAID_functions.R"))


# Set fixed parameters ----------------------------------------------------

## onset/reporting lag distribution
lag_p <- dnbinom(0:20, mu = 5, size = 5)
lag_p <- lag_p/sum(lag_p)

## serial interval distribution
si_weights <- discr_si(1:30, mu = 4.7, sigma = 2.9)

## time period to include
start_date <- "2020-03-15"
end_date <- "2021-01-01"

## Rt spline knots (if applicable)
spline_df = 9
date_knots <- seq(as.Date(start_date) + 30, as.Date(end_date) - 30, length.out = spline_df)

## Rt regression model
Rt_model <- "single"
rt_eqn <- ~ 1 + bs(date, knots = date_knots)

## Value to initialize ascertainment prob parameter
init_ascertain_prob <- 0.5

## pi mean model
ctr <- function(x) scale(x, scale = FALSE, center = TRUE)
pi_eqn <- ~ ctr(log(ntest_w))


# Load data ---------------------------------------------------------------

usdata_proc <- file.path(usdata_path, "US_processed")
dt_state <- readRDS(file.path(usdata_proc, "dt_states.rds"))
states <- "NE"
out_name <- file.path(out_dir, paste0("State_", states, "_Model_", Rt_model, ".fit.rds"))

if( file.exists(out_name) ){
  cat(paste0(states, " already complete.\n"))
  q( save = 'no' )
}

## Load merged case count data
dt <- as.data.table(readRDS(file.path(usdata_proc, "merged_final_US_S2S.rds")))

dt[,date := as.Date(date),]
data.table::setorder(dt, state,date)
dt <- subset(dt, date < end_date)

### subset to the state(s) of interest
dt <- subset(dt, state %in% states)

## Load seroprevalence survey data
sero_dt <- subset(fread(file.path(usdata_proc, "merged_sero_data.csv")), state %in% states)
sero_dt[,date := as.Date(date),]
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

## Create data set with key variables
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

## Initialize the infection potential
odt[,
    initialize_Lambda := max(
      calc_Lam(y = fill_na(incid[date <= start_date]), t = length(incid[date <= start_date]), w = si_weights, fill = 0)/init_ascertain_prob,
      max(1, pop_size[1] * 1e-6)
    ),
    by=state
]


# Calculate value to initialize pi (heuristic) ----------------------------

tmp_s <- subset(sero_dt, state %in% states)
tmp_c <- subset(odt, state %in% states)

get_c <- function(date_){
  sum(subset(tmp_c, date <= date_)$incid, na.rm = TRUE)
}

init_pi_bar <- with(tmp_s[,list(
  pi_tilde_hat = get_c(date)/max(pop_size * npos/ntested, na.rm = TRUE),
  weight = ntested
),by=date], sum(pi_tilde_hat * weight)/sum(weight))


# model fitting options ---------------------------------------------------

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


# fit ---------------------------------------------------------------------

print(paste("Now fitting:", states))
state_fit <- do.call(fitMERMAID, options_list) # E-M iter 780: plogLik = -16422, delta = 6.11e-05, prev dif = 0.0242, max|grad| = 0.00858


# plot --------------------------------------------------------------------

plot_list <- list()
pl_data <- subset(state_fit$data, state == states & obs_weight > 0.8)
plot_list <- 	make_plot(pl_data)
dev.off()
pl_1 <- plot_list$combined
ggsave(file.path(out_dir, "NE.png"), height = 6, width = 12, pl_1)


# save --------------------------------------------------------------------

saveRDS(object = list(fit = state_fit, options = options_list, plot_list = plot_list),
        file = out_name)
