
simulate_epidemic <- function( R_t, pi_t, Lambda_0, si_w = NULL){

	if( is.null( si_w ) ){
		si_w <- discr_si(1:60, mu = 4.7, sigma =  2.9)
	}


	n <- length(R_t)
	Y <- rep(NA, n)
	Lambda <- rep(NA, n)

	for(i in 1:n){
		Lambda[i] <- calc_Lam(Y, si_w, i, fill = Lambda_0)
		Y[i] <- rpois(1, lambda = Lambda[i] * R_t[i])
	}
	
	C <- rbinom(n, size = Y, prob = pi_t)

	data.table::data.table(Lambda = Lambda, Y = Y, C = C, U = Y - C, time = seq_along(Y))
}

sim_lags <- function(counts, probs, prob_mat = NULL, lags = seq_along(probs) - 1){
	require(data.table)

	#mat <- apply(matrix(counts,ncol=1), 1, rmultinom, n=1, prob = probs)
	n <- length(counts)
	
	if( !is.null(prob_mat) ){
	
		mat <- do.call(cbind, lapply(1:n, function(ii){
			x <- counts[ii]
			if( is.na(x) ){
				x <- 0
			}
			rmultinom(n=1, size = as.integer(x), prob = prob_mat[ii,])
		}))
	
	}else{
			
		mat <- do.call(cbind,
			lapply(counts, function(x){
				if( is.na(x) ){
					x <- 0
				}
				rmultinom(n=1, size = as.integer(x), prob = probs)
			})
		)
	
	}
	
	out <- rep(0, length(counts))
	
	for(i in 1:nrow(mat)){
		out <- out + data.table::shift(mat[i,], lags[i], fill = 0)
	}
	
	out
}

simulate_data <- function(time_points, X_rt, Lambda_0 = 100, mean_rt = 1.05, rt_beta, mean_pi = 0.70, ntest, ppos, alpha_tests = 0.0001, alpha_ppos = -0.75, lag_probs = dpois(0:14, 7), weekday_lag_probs = NULL, XC = NULL, Beta_XC = NULL, si_weights = NULL ){
	# as.Date("2020-01-01") + seq_along(Y) - 1

	require(splines)	

	lag_probs <- lag_probs/sum(lag_probs)
	
	sbm <- function(n) cumsum(rnorm(n))/sqrt(seq(n))
	
	expit <- function(x) 1/(1 + exp(-x))
	logit <- function(x) log(x) - log(1-x)
	
	dates <- as.Date("2020-01-01") + (1:time_points) - 1
	time_int <- 1:time_points
	
	wdays <- factor(weekdays(dates))
	#print(head(wdays))
	
#	if( rev_time ){
#		X_rt <- splines::bs( max(time_int) - time_int, df = spline_df, degree = 3)
#	}else{
#		X_rt <- splines::bs(time_int, df = spline_df, degree = 3)
#	}	

	cycle_days <- seq_along(time_int) %% 21
	X_cycle <- (model.matrix(~0+ factor(cycle_days) ))
	n_cycle <- ncol(X_cycle)
	
	#print("OK")

	log_R_t <- (X_rt %*% rt_beta)[,1]
	rt_beta <- c(
		log(mean_rt) - mean(log_R_t),
		rt_beta
	)
	
	if( !is.null(XC) ){
		log_R_t <- log_R_t + (XC %*% Beta_XC)[,1]
		
		X_rt <- cbind(X_rt, XC)
		
		rt_beta <- c(
			rt_beta, Beta_XC
		)
		rt_beta[1] <- log(mean_rt) - mean(log_R_t)
	}
	
	X_rt <- cbind(1, X_rt)
	
	X_pi <- cbind(ntest, ppos)
	pi_beta <- c(alpha_tests, alpha_ppos)
	pi_beta <- c(
		logit(mean_pi) - mean( X_pi %*% pi_beta ),
		pi_beta
	)
	X_pi <- cbind(1, X_pi)
	
	R_t <- exp( (X_rt %*% rt_beta)[,1] )
	pi_t <- expit( (X_pi %*% pi_beta)[,1] )
	
	params <- list(rt_beta = rt_beta, pi_beta = pi_beta)

	data <- data.table::data.table( date = dates, time = seq_along(dates), ntest=ntest, ppos = ppos, R_t_true = R_t, pi_t_true = pi_t)

	out <- simulate_epidemic( R_t = data$R_t_true, pi_t = data$pi_t_true, Lambda_0 =Lambda_0, si_w = si_weights )

#	print( head(data$R_t_true, 25))
#	print( head(out$Y, 25)) 	

	out <- merge(data, out, by = 'time')
	
	lag_prob_mat <- NULL
	
	if( !is.null(weekday_lag_probs) ){

		weekday_lag_probs <- (weekday_lag_probs/sum(weekday_lag_probs))
		
		lag_prob_mat <- do.call(rbind, lapply(1:nrow(X_pi), function(i){
		
			dates_i <- dates[i] + (1:length(lag_probs)) - 1
			
			XX <- model.matrix(~0+weekdays(dates_i))
			
			pp <- lag_probs * ( XX %*% weekday_lag_probs )[,1]
			pp/sum(pp)
		}))

	}
	
	out[,Y_lag := sim_lags(counts = Y, probs = lag_probs, prob_mat = lag_prob_mat),]
	out[,C_lag := rbinom(length(Y_lag), size = Y_lag, prob = pi_t),]
	out[,U_lag := Y_lag - C_lag,]
	
	list(
		data = out, 
		par = params, 
		lag_prob_mat = lag_prob_mat
	)
}


