# helper_functions.R ----

expit <- function(x) 1/(1 + exp(-x))
logit <- function(x) log(x) - log(1-x)

#   fill NA values at start or end ----
fill_na <- function(x, region = NULL, init_val = NA){
  if( is.null(region) ){
    mi <- which(is.na(x))
    nm <- which(!is.na(x))
    f0 <- mi[mi < min(nm)]
    init_val <- init_val[1]
    if( is.na(init_val) ){
      init_val <- x[nm[1]]
    }
    x[f0] <- init_val
    mi <- which(is.na(x))
    for(i in mi){
      x[i] <- x[i-1]
    }
    x
  }else{
    if(length(init_val) == 1){
      init_val <- rep(init_val, length(x))
    }
    Reduce(c, lapply(base::split(x = data.frame(x,init_val), f = region), function(df){fill_na(x = df[[1]], init_val = df[[2]])}))
  }
}

## simple "deconvolution" (technically, convolution) ----
simple_deconv <- function(x, probs, lags = seq_along(probs) - 1){
  out <- rep(0, length(x))

  for(i in 1:length(probs)){
    out <- out + data.table::shift(x, -lags[i], fill = 0)*probs[i]
  }
  out
}

logit <- function(x) log(x) - log(1 - x)

calc_shift <- function(vec, shifts, skip_cols = NULL){
  nk <- length(shifts)
  mat <- matrix(c(vec), ncol = nk)
  out <- rep(0, nrow(mat))
  for(i in 1:nk){
    if( i %in% skip_cols ) next;
    out <- out + data.table::shift(mat[,i],shifts[i],fill=0)
  }
  out
}

lag_mat <- function(i,n, reverse = FALSE){
  require(Matrix)
  if( reverse ){
    Matrix::Diagonal(n = n) - lag_mat(i=i, n=n, reverse = FALSE)
  }else{
    Matrix::sparseMatrix(
      i = (1:(n-i))+i, j = (1:(n-i)),
      dims = c(n,n)
    )
  }
}

## Calculate the marginal moments of Y = C + U ----
calc_Y_moments <- function(R_t, w_t, Lambda_0 = 1, max_window = Inf, calc_var = TRUE){

  require(Matrix)

  nr <- length(R_t)
  nb <- length(w_t)

  if( length(Lambda_0) == 1 ){
    Lambda_0 <- c(1,rep(0, nr-1))
  }

  Sigma <- NULL
  EY <- rep(NA, nr)

  for(i in 1:nr){

    if( i == 1 ){
      EY[i] <- R_t[i] * Lambda_0[i]
    }else{
      EY[i] <- R_t[i] * (sum(  w_t[min(nb, max(i-1,1)):1] * EY[max(i - nb, 1):(i-1)] ) + Lambda_0[i])
    }
  }

  if( calc_var ){
    Sigma <- matrix(NA, nr, nr)
    for(i in 1:nr){

      if( i == 1 ){
        Sigma[i,i] <- EY[i]
      }else{
        ww <- w_t[min(nb, max(i-1,1)):1]
        ii <- max(i - nb, 1):(i-1)
        Sigma[i,i] <- EY[i] + (R_t[i]^2) * sum(crossprod(Sigma[ii,ii], ww) * ww)
      }


      if( i == nr ) break;
      for( j in (i+1):nr ){

        if( j - i > max_window ){
          Sigma[i,j:nr] <- Sigma[j:nr,i] <- 0
          break;
        }

        Sigma[i,j] <- Sigma[j,i] <- R_t[j] * sum( w_t[min(nb, max(j-1,1)):1] * Sigma[i,max(j - nb, 1):(j-1)] )

      }

    }
    list( mu = EY, Sigma = as(Sigma, "RsparseMatrix"))
  }else{

    list( mu = EY)
  }

}

## Calculate marginal moments of Y = C + U across regions ----
calc_Y_moments_by_region <- function(R_t, w_t, region, Lambda_0 = 1, max_window = Inf, calc_var = TRUE){

  require(Matrix)

  Lambda_0_list <- NULL
  if( length(Lambda_0) != 1 ){
    Lambda_0_list <- base::split(x = Lambda_0, f = region)
  }

  ii <- 0
  out_list <- lapply(base::split(x = R_t, f = region), function(x){
    ii <<- ii + 1
    if( is.null(Lambda_0_list) ){
      Lambda_0_region <- Lambda_0
    }else{
      Lambda_0_region <- Lambda_0_list[[ii]]
    }
    calc_Y_moments(R_t = x, w_t = w_t, Lambda_0 = Lambda_0_region, max_window = max_window, calc_var = calc_var)
  })

  list(
    mu = Reduce(c, lapply(out_list, function(x) x$mu)),
    Sigma = if(calc_var){
      Reduce(Matrix::bdiag, lapply(out_list, function(x) x$Sigma))
    }else{
      NULL
    }
  )
}

## Calculate Lambda recursively ----
calc_Lam <- function(y, w, t, fill){
  if( t <= 1 ){
    return(fill)
  }
  np <- min(length(w), t - 1)
  yy <- y[(t - 1):(t - np)]
  if(length(yy) < length(w)) yy <- c(yy, rep(fill, length(w) - length(yy)))
  sum(yy * w)
}

## Make weight matrix for calculating Lambda ----
makeLTW <- function(vals, nr, npt = NA){
  require(Matrix)

  npt <- min(npt, length(vals), na.rm=TRUE)
  vals <- vals[1:npt]

  dd <- do.call(rbind.data.frame, lapply( 1:nr, function(ii){
    data.frame( ii = ii, jj =  ii - (1:npt), xx = vals)
  } ))

  with(
    subset(dd, jj > 0 & jj < ii),
    Matrix::sparseMatrix( i = ii, j = jj, x = xx, dims = c(nr, nr))
  )
}



calc_log_lik <- function(C, U, pi_t, R_t, W){
  Y <- U + C
  Lambda <- (W %*% Y)[,1] + 1
  RL <- R_t * Lambda
  eta_pi <- log(pi_t) - log(1- pi_t)

  logLik_Y <- sum(Y * log( RL )) - sum(RL) - sum(lgamma(Y + 1))
  logLik_C_Y <- sum( C * eta_pi ) + sum( Y * log(1 - pi_t) ) + sum(lgamma(Y + 1)) - sum(lgamma(Y-C + 1)) - sum(lgamma(C + 1))

  logLik_Y + logLik_C_Y
}

get_log_lik_fun <- function(pi_t, R_t, W){

  function(x){
    if(min(x) < 0) return(-Inf)

    U <- x[1:length(pi_t)]
    C <- x[(1:length(pi_t)) + length(pi_t)]
    Y <- U + C
    Lambda <- (W %*% Y)[,1] + 1
    RL <- R_t * Lambda
    eta_pi <- log(pi_t) - log(1- pi_t)

    logLik_Y <- sum(Y * log( RL )) - sum(RL) - sum(lgamma(Y + 1))
    logLik_C_Y <- sum( C * eta_pi ) + sum( Y * log(1 - pi_t) ) + sum(lgamma(Y + 1)) - sum(lgamma(Y-C + 1)) - sum(lgamma(C + 1))

    logLik_Y + logLik_C_Y
  }
}

get_grad_fun <- function(pi_t, R_t, W, const = 1){

  function(x){
    U <- x[1:length(pi_t)]
    C <- x[(1:length(pi_t)) + length(pi_t)]
    Y <- U + C
    Lambda <- (W %*% Y)[,1] + const

    eta0 <- log(1- pi_t)
    eta1 <- log(pi_t) - eta0
    dg_Y_C <- digamma(Y - C + 1)

    vec <- (t(W) %*% diag(1/Lambda)  %*% Y )[,1]

    grad_Y <- log(Lambda) + log(R_t) + vec - dg_Y_C  + eta0 - (t(W) %*% R_t)[,1]

    grad_C <- eta1 + dg_Y_C - digamma(C + 1)

    c(
      grad_Y,
      grad_C
    )
  }
}

get_hessian_fun <- function(pi_t, R_t, W, const = 1){

  function(x){
    U <- x[1:length(pi_t)]
    C <- x[(1:length(pi_t)) + length(pi_t)]
    #U <- Y - C
    Y <- U + C
    Lambda <- (W %*% Y)[,1] + const

    eta0 <- log(1- pi_t)
    eta1 <- log(pi_t) - eta0
    dg_Y_C <- digamma(Y - C + 1)

    vec <- (t(W) %*% diag(1/Lambda)  %*% Y )[,1]

    grad_Y <- log(Lambda) + log(R_t) + vec - dg_Y_C  + eta0 - (t(W) %*% R_t)[,1]

    grad_C <- eta1 + dg_Y_C - digamma(C + 1)

    Wtfp <- crossprod(W, diag(1/Lambda))

    hess_YC <- Matrix::Diagonal( x = trigamma(Y - C + 1))
    hess_C <- (-1)*hess_YC - Matrix::Diagonal( x = trigamma(C + 1))
    hess_Y <- crossprod(W, diag(-Y/(Lambda^2) ) %*% W ) + Wtfp + t(Wtfp) - hess_YC

    rbind(
      cbind(hess_Y, hess_YC),
      cbind(hess_YC, hess_C)
    )
  }
}

# cumulative_incidence.R ----

# The following two functions are copied directly
# from EpiEstim (https://github.com/mrc-ide/EpiEstim as described in Cori et al., 2013)*:

vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}

discr_si <- function(k, mu, sigma)
{
  if (sigma < 0) {
    stop("sigma must be >=0.")
  }
  if (mu <= 1) {
    stop("mu must be >1")
  }
  if (any(k < 0)) {
    stop("all values in k must be >=0.")
  }

  a <- ((mu - 1) / sigma)^2
  b <- sigma^2 / (mu - 1)

  cdf_gamma <- function(k, a, b) stats::pgamma(k, shape = a, scale = b)

  res <- k * cdf_gamma(k, a, b) +
    (k - 2) * cdf_gamma(k - 2, a, b) - 2 * (k - 1) * cdf_gamma(k - 1, a, b)
  res <- res + a * b * (2 * cdf_gamma(k - 1, a + 1, b) -
                          cdf_gamma(k - 2, a + 1, b) - cdf_gamma(k, a + 1, b))
  res <- vnapply(res, function(e) max(0, e))

  return(res)
}


# * Citation, as per https://github.com/mrc-ide/EpiEstim
# @misc{Cori2021, author={Cori, A and Kamvar, ZN and Stockwin, J and Jombart, T and Dahlqwist, E and FitzJohn, R and Thompson, R},
# year={2021},
# title={{EpiEstim v2.2-3: A tool to estimate time varying instantaneous reproduction number during epidemics}},
# publisher={GitHub}, journal={GitHub repository},
# howpublished = {\url{https://github.com/mrc-ide/EpiEstim}}, commit={c18949d93fe4dcc384cbcae7567a788622efc781},
# }


## This function calculates the infection potential, Lambda ----
getLambda <- function(x, t, mu, sd, NTS = 30){

  # x : daily increase in number of positives
  # t : date or integer time

  ti = as.integer(t)

  to = t - min(t) + 1

  if( !all( to == sort(to) ) ) stop("Input must be sorted")

  # full range of dates, in case some are missing
  tt = min(to):max(to)

  # fill in missing values with zeros
  xx = 0*tt
  xx[to] <- x

  # calculate cumulative incidence
  Lambda = colSums(do.call(rbind, lapply(1:NTS, function(i) discr_si(i,mu,sd)*data.table::shift(xx, n = i, fill = 0))))

  # map these back to dates present in input
  Lambda = Lambda[to]

  # return
  Lambda
}


# glm_family_c.R ----

# Poisson and binomial families that do not
# return errors for non-integer response.

dpois_c <- function(x, lambda, log = FALSE){
  out <- x*log(lambda) - lambda - lgamma(x+1)
  if( !log ){
    exp(out)
  }else{
    out
  }
}

lchoose_c <- function(n, k) lgamma(n+1) - lgamma(n-k+1) - lgamma(k+1)

dbinom_c <- function(x, size, prob, log = FALSE){
  out <- lgamma(size+1) - lgamma(size-x+1) - lgamma(x+1) + x * log(prob) + (size-x) * log(1-prob)
  if( !log ){
    exp(out)
  }else{
    out
  }
}

# The following functions are copied from stats::poisson and modified to omit error messages for non-integer values

poisson_c <- function (link = "log")
{
  linktemp <- substitute(link)
  if (!is.character(linktemp))
    linktemp <- deparse(linktemp)
  okLinks <- c("log", "identity", "sqrt")
  if (linktemp %in% okLinks)
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  }
  else {
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name))
        linktemp <- stats$name
    }
    else {
      stop(gettextf("link \"%s\" not available for poisson family; available links are %s",
                    linktemp, paste(sQuote(okLinks), collapse = ", ")),
           domain = NA)
    }
  }
  variance <- function(mu) mu
  validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
  dev.resids <- function(y, mu, wt) {
    r <- mu * wt
    p <- which(y > 0)
    r[p] <- (wt * (y * log(y/mu) - (y - mu)))[p]
    2 * r
  }
  aic <- function(y, n, mu, wt, dev) -2 * sum(dpois_c(y, mu,
                                                      log = TRUE) * wt)
  initialize <- expression({
    if (any(y < 0)) stop("negative values not allowed for the 'Poisson' family")
    n <- rep.int(1, nobs)
    mustart <- y + 0.1
  })
  simfun <- function(object, nsim) {
    wts <- object$prior.weights
    if (any(wts != 1))
      warning("ignoring prior weights")
    ftd <- fitted(object)
    rpois(nsim * length(ftd), ftd)
  }
  structure(list(family = "poisson", link = linktemp, linkfun = stats$linkfun,
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids,
                 aic = aic, mu.eta = stats$mu.eta, initialize = initialize,
                 validmu = validmu, valideta = stats$valideta, simulate = simfun),
            class = "family")
}

binomial_c <- function (link = "logit")
{
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
  if (linktemp %in% okLinks)
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    ## what else shall we allow?  At least objects of class link-glm.
    if(inherits(link, "link-glm")) {
      stats <- link
      if(!is.null(stats$name)) linktemp <- stats$name
    } else {
      stop(gettextf('link "%s" not available for binomial family; available links are %s',
                    linktemp, paste(sQuote(okLinks), collapse =", ")),
           domain = NA)
    }
  }
  variance <- function(mu) mu * (1 - mu)
  validmu <- function(mu) all(is.finite(mu)) && all(mu>0 &mu<1)
  dev.resids <- function(y, mu, wt) .Call(stats:::C_binomial_dev_resids, y, mu, wt)
  aic <- function(y, n, mu, wt, dev) {
    m <- if(any(n > 1)) n else wt
    -2*sum(ifelse(m > 0, (wt/m), 0)*
             dbinom_c(m*y, m, mu, log=TRUE))
  }
  initialize <- expression({
    if (NCOL(y) == 1) {
      ## allow factors as responses
      ## added BDR 29/5/98
      if (is.factor(y)) y <- y != levels(y)[1L]
      n <- rep.int(1, nobs)
      ## anything, e.g. NA/NaN, for cases with zero weight is OK.
      y[weights == 0] <- 0
      if (any(y < 0 | y > 1))
        stop("y values must be 0 <= y <= 1")
      mustart <- (weights * y + 0.5)/(weights + 1)
      m <- weights * y
      # if(any(abs(m - round(m)) > 1e-3))
      # warning("non-integer #successes in a binomial glm!")
    }
    else if (NCOL(y) == 2) {
      # if(any(abs(y - round(y)) > 1e-3))
      # warning("non-integer counts in a binomial glm!")
      n <- y[, 1] + y[, 2]
      y <- ifelse(n == 0, 0, y[, 1]/n)
      weights <- weights * n
      mustart <- (n * y + 0.5)/(n + 1)
    }
    else stop("for the 'binomial' family, y must be a vector of 0 and 1\'s\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
  })
  simfun <- function(object, nsim) {
    ftd <- fitted(object)
    n <- length(ftd)
    ntot <- n*nsim
    wts <- object$prior.weights
    if (any(wts %% 1 != 0))
      stop("cannot simulate from non-integer prior.weights")
    ## Try to fathom out if the original data were
    ## proportions, a factor or a two-column matrix
    if (!is.null(m <- object$model)) {
      y <- model.response(m)
      if(is.factor(y)) {
        ## ignote weights
        yy <- factor(1+rbinom(ntot, size = 1, prob = ftd),
                     labels = levels(y))
        split(yy, rep(seq_len(nsim), each = n))
      } else if(is.matrix(y) && ncol(y) == 2) {
        yy <- vector("list", nsim)
        for (i in seq_len(nsim)) {
          Y <- rbinom(n, size = wts, prob = ftd)
          YY <- cbind(Y, wts - Y)
          colnames(YY) <- colnames(y)
          yy[[i]] <- YY
        }
        yy
      } else
        rbinom(ntot, size = wts, prob = ftd)/wts
    } else rbinom(ntot, size = wts, prob = ftd)/wts
  }
  structure(list(family = "binomial",
                 link = linktemp,
                 linkfun = stats$linkfun,
                 linkinv = stats$linkinv,
                 variance = variance,
                 dev.resids = dev.resids,
                 aic = aic,
                 mu.eta = stats$mu.eta,
                 initialize = initialize,
                 validmu = validmu,
                 valideta = stats$valideta,
                 simulate = simfun),
            class = "family")
}


# fit_MERMAID.R ----

##  construct multinomial covariance matrix ----

make_multinom_cov_mat <- function(pmat, sizes){
  require(Matrix)
  require(data.table)

  nt <- nrow(pmat)
  nk <- ncol(pmat)

  out_list <- list()

  for(j in 1:nk){
    for(k in j:nk){
      if( j == k ){
        vals <- pmat[,j]*(1 - pmat[,k])
      }else{
        vals <- (-1)*pmat[,j]*pmat[,k]
      }
      out_list[[length(out_list) + 1]] <- data.table(
        ii = nt*(j - 1) + 1:nt,
        jj = nt*(k - 1) + 1:nt,
        xx = sizes * vals
      )

    }
  }

  with(rbindlist(out_list),
       Matrix::sparseMatrix(i = ii, j = jj, x = xx, symmetric = TRUE)
  )
}

##  Estimate lag distribution ----

get_lag_probs <- function(val, nn, mu){
  pp <- dnbinom(0:(nn-1), mu = mu, size = val)
  pp/sum(pp)
}

optim_lag_probs <- function(count_matrix, mu_par, return_par = FALSE){

  nk <- ncol(count_matrix)
  count_matrix <- t(count_matrix)

  obj_fun <- function(x){
    sum(log(get_lag_probs(val=x, nn = nk, mu = mu_par)) * count_matrix)
  }

  par <- (optim(par = 1, fn = obj_fun, control = list(fnscale = -1), method = "Brent", lower = 0.05, upper = 500 ))$par

  if(return_par) return(par)

  list( opt_par = par,
        probs = get_lag_probs(val=par, nn = nk, mu = mu_par)
  )
}

ar1_matrix <- function(n, rho, sigma = 1, inverse = FALSE){
  if( inverse ){
    L <- lag_mat(i = 1, n = n)
    I <- Matrix::Diagonal(n = n)

    (
      I*(1 - rho^2) - (t(L) + L)*rho + ( crossprod(L) + tcrossprod(L) )*(rho^2)
    )/sigma
  }else{
    (rho^abs(outer(1:n, 1:n, `-`)))*sigma/(1 - rho^2)
  }
}


##  Main model-fitting function ----

fitMERMAID <- function(rt_formula, pi_formula, pi_intercept, data, prev_data = NULL, prev_weight = 1, mean_lag = 7, max_lag = 15, confirmed_cases_var = "new_cases", date_var = "date", subset_var = NULL, region_var = NULL, pop_size_var = NULL, si_nts = 60, si_weights, lag_probs = NULL, fix_lag_probs = FALSE, estimate_weekday_lag = TRUE, adj_Lambda = 1, init_Lambda = 0, min_pi_t = 0.01, glm_method = 2, tol = 1e-3, recycle_start = TRUE, plot_every = NA, state_name = "", init_Rt = NA, recovered_offset = TRUE, return_vcov = FALSE, max_it = 5000, max_it_E_step = 5, constrain_Rt_E_step = FALSE, pi_start = NULL, pi_init_mean = 0.5, min_p_obs = 0.8, accelerated_em = TRUE, lag_intensity_knots = NULL, pi_eps = 0.05, reweight_prev = FALSE, prev_method = "gaussian", print_mem = TRUE, info_prev_weight = 1 ){

  prev_method <- tolower(prev_method)
  if( any(sapply(c("gaussian", "normal"), grepl, x = prev_method)) ){
    prev_method <- "normal"
  }else if(  grepl("binom", prev_method ) ){
    prev_method <- "binom"
  }else if( grepl("hyper", prev_method ) ){
    prev_method <- "hyper"
  }else{
    stop("prev_method invalid. Use 'normal'/'gaussian', 'hyper', or 'binom',")
  }

  prev_weight_0 <- prev_weight

  expit <- function(x) 1/(1 + exp(-x))
  logit <- function(x) log(x) - log(1 - x)

  truncate_pi <- function(x, eps = pi_eps) pmax(pmin(x, 1 - eps), eps)

  require(fastglm)
  require(data.table)
  require(splines)
  require(Matrix)

  na.action.prev <- options('na.action')
  #options(na.action='na.pass')

  adjust_prevalence <- !is.null(prev_data)


  #   Update formula ----

  rt_formula <- update.formula(rt_formula,  ~ .)

  pi_formula <- update.formula(pi_formula,  ~ .)
  if( !adjust_prevalence ){
    pi_formula <- update.formula(pi_formula,  ~ -1 + .)
  }

  #   Format data ----

  dt <- as.data.table(data)
  dt_prev <- as.data.table(prev_data)

  if( is.numeric(pi_intercept) ){
    dt[,pi_offset := pi_intercept,]
    pi_intercept <- "pi_offset"
  }

  if( is.null(region_var) ){
    region_var <- "region"
    dt[,region := 1,]
    dt_prev[,region := 1,]
  }

  if( is.null(subset_var) ){
    subset_var <- "use_ss"
    dt[,use_ss := TRUE,]
  }

  if( recovered_offset & !is.null(pop_size_var) ){
    setnames(dt, pop_size_var, "pop_size")
  }else{
    recovered_offset <- FALSE
  }

  setnames(dt, c(pi_intercept, confirmed_cases_var, date_var, subset_var), c("pi_offset", "c_cases", "date", "use_ss"))


  if( adjust_prevalence ){
    setnames(dt_prev, c(date_var, region_var), c("date", "region"))
    if(!all( c("npos", "ntested") %in% names(dt_prev) )){
      stop("Prevalence matrix must include columns npos, ntested")
    }
  }

  dt$region <- dt[[region_var]]

  dt[,time_int := as.integer(as.Date(date)),]
  setorder(dt, region, time_int)

  ss_nm <- complete.cases(cbind(
    as.matrix(model.matrix( update.formula(pi_formula, c_cases ~ .), dt )),
    as.matrix(model.matrix( update.formula(rt_formula, c_cases ~ .), dt ))
  ))

  if( any(dt$c_cases[!is.na(dt$c_cases)] < 0) ){
    rp0 <- which( dt$c_cases[!is.na(dt$c_cases)] < 0 )
    warning(paste0(length(rp0), " negative case counts coerced to 0."))
    dt$c_cases[!is.na(dt$c_cases)][rp0] <- 0
  }

  if( any(is.na(dt$c_cases)) ){
    rp0 <- which( is.na(dt$c_cases) )
    warning(paste0(length(rp0), " NA-valued case counts coerced to 0."))
    dt$c_cases[rp0] <- 0
  }

  if( mean(ss_nm) < 1 ) warning(paste0(sum(!ss_nm), " observations removed due to missing data."))

  ss <- ((dt$use_ss > 0) & ss_nm)

  #   Create matrices for seroprevalence ----

  W_prev <- NULL

  B_prev <- NULL
  B_cs <- NULL
  S_prev <- c()
  K_prev <- c()
  N_pop <- c()

  if( adjust_prevalence ){

    W_prev <- matrix(0, nrow(dt_prev), nrow(dt_prev))

    B_prev <- matrix(NA, nrow(dt), nrow(dt_prev))
    B_cs <- matrix(NA, nrow(dt), nrow(dt_prev))

    for(i in 1:nrow(dt_prev)){

      S_prev[i] <- dt_prev$npos[i]
      K_prev[i] <- dt_prev$ntested[i]

      region_ <- dt_prev$region[i]
      date_ <- dt_prev$date[i]
      pop_size_ <- dt$pop_size[  which(dt$region == region_ & dt$date >= date_ )[1] ]
      N_pop[i] <- pop_size_

      B_prev[,i] <- 1*( dt$region == region_ & dt$date <= date_ )/pop_size_
      B_cs[,i] <- 1*( dt$region == region_ & dt$date <= date_ )

      pp_i <- dt_prev$npos[i]/dt_prev$ntested[i]
      SE_sq_i <- pp_i * (1 - pp_i ) / dt_prev$ntested[i]

      W_prev[i,i] <-  1 / ( SE_sq_i * (pop_size_^2) )

    }

  }

  #   Create matrices for lag times ----

  A_start <- NULL

  if ( !is.null(lag_probs) ) max_lag <- length(lag_probs) - 1

  J <- NULL
  S <- NULL
  nk <- max_lag + 1

  lags <- (1:nk) - 1

  if( is.null(lag_probs ) ){
    #fix_lag_probs <- FALSE
    lag_probs <- get_lag_probs(val = 2, nn = nk, mu = mean_lag)
  }else{
    fix_lag_probs <- TRUE
  }

  lag_prob_mat <- t(replicate(nrow(dt), lag_probs))


  nt_list <- dt[,list(n = .N),by=region]$n

  J_list <- lapply(nt_list, function(nt) do.call(cbind, replicate(nk, Matrix::Diagonal(n = nt), simplify=FALSE)))
  S_list <- lapply(nt_list, function(nt) do.call(cbind, lapply((1:nk) - 1, lag_mat, n = nt, reverse = FALSE)))

  J <- Matrix::bdiag(J_list)
  S <- Matrix::bdiag(S_list)

  #   Split/rep lags by region ----

  split_vec <- function(mat, fac = dt$region){
    c(base::split(as.matrix(mat), fac), recursive=TRUE, use.names=FALSE)
  }

  rep_vec <- function(vec, fac = dt$region, nrep = nk){
    c(lapply(base::split(vec, fac), rep, times= nrep), recursive=TRUE, use.names=FALSE)
  }

  region_vec <- rep_vec(dt$region)

  unsplit_vec <- function(vec, fac = region_vec, ncols = nk){
    do.call(rbind, lapply(base::split(vec, fac), matrix, ncol = ncols))
  }

  is_obs_mat <- unsplit_vec(
    split_vec(do.call(rbind, lapply(as.Date(dt$date), function(x) 1*( x + lags <= max(dt$date) & x + lags >= min(dt$date) ) )))
  )
  ss <- ss & ( rowSums(is_obs_mat * lag_prob_mat) > min_p_obs )

  ss_l <- rep_vec(ss)

  #   Create covariate matrices ----

  X_pi <- as.matrix(model.matrix( pi_formula, dt ))
  X_rt <- model.matrix( rt_formula, dt )

  wkday <- function(x) (weekdays(x,TRUE))

  weekday_vec <- split_vec(do.call(rbind, lapply(as.Date(dt$date), function(x) wkday(x + lags))))
  lag_vec <- split_vec(do.call(rbind, lapply(as.Date(dt$date), function(x) lags )))
  date_vec <- split_vec(do.call(rbind, lapply(as.numeric(dt$date), function(x) (x + lags))))

  n_brk <- lag_intensity_knots

  interval_vec <- as.factor(cut(date_vec, breaks = n_brk))

  if( length(unique(dt$region)) == 1 ){

    if( !is.null(lag_intensity_knots) ){
      lag_formula <- ~factor(weekday_vec):factor(interval_vec)
    }else{
      lag_formula <- ~factor(weekday_vec)
    }
  }else{
    if( !is.null(lag_intensity_knots) ){
      lag_formula <- ~ (factor(weekday_vec):interval_vec)*factor(region_vec)
    }else{
      lag_formula <- ~factor(weekday_vec)*factor(region_vec)
    }
  }
  X_lag <- model.matrix( lag_formula )

  #   Create infection potential weight matrices ----

  W_list <- lapply(dt[,list(n = .N),by=region]$n, function(NR){
    makeLTW(vals = si_weights, nr = NR, npt = si_nts)
  })

  W <- Matrix::bdiag(W_list)

  if(is.character(init_Lambda)){
    init_Lambda <- dt[[init_Lambda]]
  }

  Lambda_pre <- (1 - rowSums(W)) * init_Lambda

  #   fill NA values at start or end ----

  fill_na <- function(x, region = NULL, init_val = NA){
    if( is.null(region) ){
      mi <- which(is.na(x))
      nm <- which(!is.na(x))
      f0 <- mi[mi < min(nm)]
      init_val <- init_val[1]
      if( is.na(init_val) ){
        init_val <- x[nm[1]]
      }
      x[f0] <- init_val
      mi <- which(is.na(x))
      for(i in mi){
        x[i] <- x[i-1]
      }
      x
    }else{
      if(length(init_val) == 1){
        init_val <- rep(init_val, length(x))
      }
      Reduce(c, lapply(base::split(x = data.frame(x,init_val), f = region), function(df){fill_na(x = df[[1]], init_val = df[[2]])}))
    }
  }

  #   Initialize parameters ----

  if( is.null(pi_start)){
    dt[,`:=`( pi_t = expit( logit(pi_init_mean) + pi_offset-mean(pi_offset) ), R_t = 1 ),]
  }else{
    pi_eta <- dt$pi_offset + (X_pi %*% pi_start)[,1]
    dt[,`:=`( pi_t = expit( logit(pi_init_mean) + pi_eta-mean(pi_eta) ), R_t = 1 ),]
    pi_start[1] <- mean(logit(dt$pi_t))
  }

  coef_delta <- Inf

  rt_start <- NULL
  rt_sigma <- 0*dt$R_t

  # initial rough estimate ----
  dt[,u_cases := c_cases*(1 - pi_t)/pi_t ,]

  #   utility functions ----

  calc_information <- function(prev_w = prev_weight){

    Y <- dt$y_cases
    #Y[]
    M <- (dt$c_cases + dt$u_cases)
    C <- dt$c_cases

    AA <- crossprod(W, Matrix::Diagonal(x = 1/dt$Lambda))

    A_mat <- (AA + t(AA) - (t(W) %*% Matrix::Diagonal( x = N/(dt$Lambda^2) ) %*% W))

    if( adjust_prevalence ){
      if( prev_method == "hyper" ){
        CS_Y <- crossprod(B_cs, Y)
        CS_Y <- pmax(CS_Y, S_prev - 0.99)
        hd <- trigamma(CS_Y+1) - trigamma(CS_Y-S_prev+1) + trigamma(N_pop - CS_Y+1) - trigamma(N_pop - CS_Y-K_prev+S_prev+1)
        Sero_Hess <- prev_w * B_cs %*% Matrix::Diagonal(x = hd) %*% t(B_cs)
      }else if( prev_method == "normal" ){
        # CS_Y <- crossprod(B_cs, Y)

        Sero_Hess <- (-prev_w) * B_cs %*% W_prev %*% t(B_cs)

      }else{
        sero_pi <- crossprod(B_prev, Y)
        hd <- S_prev/(sero_pi^2) + (K_prev - S_prev)/( (1 - sero_pi)^2 )
        Sero_Hess <- prev_w * (-1) * B_prev %*% Matrix::Diagonal(x = hd) %*% t(B_prev)
      }

      A_mat <- A_mat + Sero_Hess
    }

    A_i <- solve( A_mat[ss,ss] )

    R_i_diag <- (-1)/trigamma(A_vec[ss_l] + 1)
    R_i <- Matrix::Diagonal(x = R_i_diag )
    D_i <- Matrix::Diagonal(x = 1/(trigamma(M + 1)  - trigamma(M - C + 1)) )[ss,ss]

    J. <- J[ss,ss_l]
    S. <- S[ss,ss_l]

    J_naive <- Matrix::bdiag(
      crossprod(X_rt[ss,], Matrix::Diagonal(x = dt$Lambda[ss] * dt$R_t[ss]) %*% X_rt[ss,]),
      crossprod(X_pi[ss,], Matrix::Diagonal(x = M[ss] * dt$pi_t[ss] * (1 - dt$pi_t[ss]) ) %*% X_pi[ss,])
    )

    # ----

    T <- rbind(
      crossprod(X_rt[ss,], Matrix::Diagonal(n = sum(ss)) - Matrix::Diagonal( x = dt$R_t[ss] ) %*% W[ss,ss] ) %*% J.,
      (-crossprod(X_pi[ss,], Matrix::Diagonal( x = dt$pi_t[ss] ) ) %*% S. )
    )

    grad_vec <- c(
      crossprod(X_rt[ss,], Y[ss] -  dt$Lambda[ss] * dt$R_t[ss])[,1],
      crossprod(X_pi[ss,],  C[ss] - M[ss]*dt$pi_t[ss] )[,1]
    )

    if( estimate_weekday_lag ){


      not_n1 <- matrix(TRUE, nt, nk)
      not_n1[,1] <- FALSE

      not_n1 <- split_vec(not_n1)

      mu_phi <- split_vec(lag_prob_mat)[ss_l]

      mat_phi <- Matrix::Diagonal(n = length(mu_phi)) - Matrix::Diagonal(x = mu_phi) %*% t(J.) %*% J.

      grad_vec <- c(
        grad_vec,
        crossprod(X_lag[ss_l & not_n1,], A_vec[ss_l & not_n1] - crossprod(J[ss,ss_l & not_n1], Y[ss])[,1] * mu_phi[not_n1[ss_l]] )[,1][-1]
      )

      T <- rbind(
        T,
        (-crossprod(X_lag[ss_l & not_n1,], mat_phi[not_n1[ss_l],]))[-1,]
      )

      J_naive <- Matrix::bdiag(J_naive,
                               crossprod(X_lag[ss_l & not_n1,], make_multinom_cov_mat(lag_prob_mat[ss,1:(nk-1)], Y[ss]) %*% X_lag[ss_l & not_n1,])[-1,-1]
      )
    }

    JRiSt <- tcrossprod(tcrossprod(J.,R_i), S.)
    JRiJt <- Matrix::Diagonal( x = ( J. %*% R_i_diag)[,1] )
    SRiSt <- Matrix::Diagonal( x = ( S. %*% R_i_diag)[,1] )

    inner_1 <- solve( D_i + SRiSt )

    JBiJt <- JRiJt - tcrossprod(JRiSt %*% inner_1, JRiSt)

    inner_2 <- solve( A_i + JBiJt )

    # ----

    TRiTt <- tcrossprod(tcrossprod(T,R_i), T)
    TRiSt <- tcrossprod(tcrossprod(T,R_i), S.)
    TRiJt <- tcrossprod(tcrossprod(T,R_i), J.)

    # ----

    TBiTt <- TRiTt - tcrossprod(TRiSt %*% inner_1, TRiSt)
    TBiJt <- TRiJt - tcrossprod(TRiSt %*% inner_1, JRiSt)
    JBiSt <- JRiSt - tcrossprod(JRiSt %*% inner_1, SRiSt)
    SBiSt <- SRiSt - tcrossprod(SRiSt %*% inner_1, SRiSt)

    # ----

    THiTt <- TBiTt - tcrossprod(TBiJt %*% inner_2, TBiJt)
    JHiJt <- JBiJt - tcrossprod(JBiJt %*% inner_2, JBiJt)
    SHiSt <- SBiSt - tcrossprod(t(JBiSt) %*% inner_2, t(JBiSt))
    JHiTt <- t(TBiJt) - tcrossprod(JBiJt %*% inner_2, TBiJt)

    # ----

    inner <- as.matrix(J_naive + THiTt)


    list(
      grad_vec = grad_vec,
      adj_mat = THiTt,
      info_adj = J_naive + THiTt,
      info_naive = J_naive,
      VarY_C = (-1)*JHiJt + JHiTt %*% MASS::ginv(inner) %*% t(JHiTt)
    )
  }


  calc_ll <- function(theta_, m_step = TRUE){

    lgamma1p <- function(x) lgamma(1+x)

    w_rt <- (1:ncol(X_rt))
    w_pi <- ncol(X_rt) + (1:ncol(X_pi))
    w_lag <- ncol(X_rt) + ncol(X_pi) + (1:(ncol(X_lag)-1))

    pi_hat <- expit(X_pi %*% theta_[w_pi] + dt$pi_offset)

    rt_hat <- exp( X_rt %*% theta_[w_rt] )

    if( recovered_offset ){
      rt_hat <- rt_hat * (1 - p_past_infected)
    }

    if( !is.na(min_pi_t) ){
      pi_hat[pi_hat < min_pi_t] <- min_pi_t
    }

    if( estimate_weekday_lag ){
      LPM <- exp((X_lag %*% c(0, theta_[w_lag]) )[,1] + lag_eta_vec)
    }else{
      LPM <- exp( lag_eta_vec)
    }
    LPM <- unsplit_vec(LPM)
    LPM <- LPM/rowSums(LPM)

    yy <- dt$y_cases
    cc <- dt$c_cases
    uu <- dt$u_cases
    Lam <- dt$Lambda
    M <- rowSums(A_start)

    if( !m_step ){

      ll <- sum( yy[ss] * log( rt_hat[ss])  + yy[ss] * log(Lam[ss] ) - Lam[ss] * rt_hat[ss]  - lgamma1p(yy[ss]) ) +
        sum( cc[ss]*log(pi_hat[ss]) + (uu[ss])*log(1-pi_hat[ss])  + lchoose_c(cc[ss]+uu[ss],cc[ss]) ) +
        sum(A_start[ss,] * log(LPM[ss,])) + sum(lgamma1p(rowSums(A_start[ss,]))) - sum(lgamma1p(A_start[ss,]))

    }else{

      ll <- sum( yy[ss] * log( rt_hat[ss])  - Lam[ss] * rt_hat[ss]  ) +
        sum( cc[ss]*log(pi_hat[ss]) + (uu[ss])*log(1-pi_hat[ss])   ) +
        sum(A_start[ss,] * log(LPM[ss,]))

    }

    if( adjust_prevalence ){

      sero_pi <- crossprod(B_prev, yy)

      ll <- ll + prev_weight*(sum( S_prev * log(sero_pi) ) + sum( (K_prev - S_prev) * log(1 - sero_pi) ) + sum(lchoose_c( K_prev, S_prev )))
    }


    ll
  }


  calc_ll_pi <- function(fixed_pi, fixed_r_t = NA, m_step = FALSE){

    lgamma1p <- function(x) lgamma(1+x)

    min_D <- 0.0

    v_list <- base::split(dt$c_cases/fixed_pi, dt$region)

    A_start0 <- lag_prob_mat * do.call(c, lapply(v_list, function(x){ simple_deconv(x, probs = colMeans(lag_prob_mat)) }))
    A_start0[A_start0 <= min_D] <- min_D

    A_vec0 <- split_vec(A_start0)
    A_vec0[A_vec0 <= min_D] <- min_D


    w_rt <- (1:ncol(X_rt))
    w_pi <- ncol(X_rt) + (1:ncol(X_pi))
    w_lag <- ncol(X_rt) + ncol(X_pi) + (1:(ncol(X_lag)-1))

    pi_hat <- expit( logit(fixed_pi) + dt$pi_offset-mean(dt$pi_offset) )

    rt_hat <- rep(fixed_r_t, nrow(X_rt))

    if( !is.na(min_pi_t) ){
      pi_hat[pi_hat < min_pi_t] <- min_pi_t
    }

    LPM <- lag_prob_mat

    yy <- (J %*% A_vec0)[,1]
    yy <- pmax(yy, min_D)

    Lam <- ( W %*% yy )[,1] + Lambda_pre + adj_Lambda

    if( is.na(fixed_r_t) ){
      rt_hat <- yy/Lam
    }

    cc <- dt$c_cases


    if( recovered_offset ){

      p_past_infected <- data.table(yy = yy, region = dt$region, pop_size = dt$pop_size)[,cumsum(data.table::shift(yy,fill=0))/pop_size,by=region]$V1

      p_past_infected <- pmin(pmax(p_past_infected, 10/dt$pop_size), 1 - 10/dt$pop_size)

      rt_hat <- rt_hat * (1 - p_past_infected)
    }


    M <- (S %*% A_vec0)[,1]

    M[M <= cc] <- cc[M <= cc] + min_D

    uu <- M - cc

    uu[uu <= min_D] <- min_D

    if( !m_step ){

      ll <- sum( yy[ss] * log( rt_hat[ss])  + yy[ss] * log(Lam[ss] ) - Lam[ss] * rt_hat[ss]  - lgamma1p(yy[ss]) ) +
        sum( cc[ss]*log(pi_hat[ss]) + (uu[ss])*log(1-pi_hat[ss])  + lchoose_c(cc[ss]+uu[ss],cc[ss]) ) +
        sum(A_start0[ss,] * log(LPM[ss,])) + sum(lgamma1p(rowSums(A_start0[ss,]))) - sum(lgamma1p(A_start0[ss,]))

    }else{

      ll <- sum( yy[ss] * log( rt_hat[ss])  - Lam[ss] * rt_hat[ss]  ) +
        sum( cc[ss]*log(pi_hat[ss]) + (uu[ss])*log(1-pi_hat[ss])   ) +
        sum(A_start0[ss,] * log(LPM[ss,]))

    }

    if( adjust_prevalence ){

      sero_pi <- crossprod(B_prev, yy)
      sero_pi <-  pmin(pmax(sero_pi, 10/dt$pop_size), 1 - 10/dt$pop_size)

      ll <- ll + prev_weight*(sum( S_prev * log(sero_pi) ) + sum( (K_prev - S_prev) * log(1 - sero_pi) ) + sum(lchoose_c( K_prev, S_prev )))
    }


    ll
  }


  calc_ll_E_step <- function(A_){

    lgamma1p <- function(x) lgamma(1+x)

    A_[A_ <= min_D] <- min_D

    A_m_ <- unsplit_vec(A_)

    C_ <- dt$c_cases

    M_ <- (S %*% A_)[,1]
    if(any(is.na(M_))){
      stop(print(summary(A_)))
    }

    M_[M_ <= min_D] <- min_D
    M_[M_ <= C_] <- C[M_ <= C_] + min_D

    U_ <- M_ - C_

    U_[U_ <= min_D] <- min_D

    N_ <- (J %*% A_)[,1]
    N_[N_ <= min_D] <- min_D

    Lam_ <- (W %*% N_)[,1] + lambda_0

    pi_hat <- truncate_pi(dt$pi_t)

    rt_hat <- dt$R_t

    LPM <- lag_prob_mat

    ll <- sum( N_ * log( rt_hat )  + N_ * log(Lam_ ) - Lam_ * rt_hat - lgamma1p(N_) ) +
      sum( C_*log(pi_hat) + (U_)*log(1-pi_hat)  + lchoose_c(C_+U_,C_) ) +
      sum(A_m_ * log(LPM)) + sum(lgamma1p(rowSums(A_m_))) - sum(lgamma1p(A_m_))

    if( adjust_prevalence ){

      sero_pi <- crossprod(B_prev, N_)

      eps <- 1e-6
      sero_pi <- pmax(sero_pi, eps)
      sero_pi <- pmin(sero_pi, 1 - eps)

      ll <- ll + prev_weight*(sum( S_prev * log(sero_pi) ) + sum( (K_prev - S_prev) * log(1 - sero_pi) ) + sum(lchoose_c( K_prev, S_prev )))
    }


    ll
  }

  #   E-M algorithm ----

  Y_moments <- NULL
  sigma_delta <- Inf
  ll_old <- (-Inf)
  rt_ll_old <- (-Inf)
  pi_ll_old <- (-Inf)

  final_pass <- FALSE
  it <- 0

  theta <- NULL
  theta_last <- NULL


  repeat{

    ##  E step ----

    min_D <- 1e-8
    tol_delta <- 1e-3
    nudge <- 0.5

    min_grad_norm <- Inf
    best_A <- NULL

    C <- fill_na(dt$c_cases, dt$region, init_val = 0)
    ascertain_prob <- truncate_pi(fill_na(dt$pi_t, dt$region))
    R_t <- fill_na(dt$R_t, dt$region)

    if(constrain_Rt_E_step){
      R_t[] <- 1
    }

    eps <- 1e-8
    R_t <- pmax(R_t, eps)
    ascertain_prob <- pmax(ascertain_prob, eps)
    ascertain_prob <- pmin(ascertain_prob, 1-eps)

    lambda_0 <- Lambda_pre + adj_Lambda

    if( is.null(A_start) ){

      v_list <- base::split(C/ascertain_prob, dt$region)

      A_start <- lag_prob_mat * do.call(c, lapply(v_list, function(x){ simple_deconv(x, probs = colMeans(lag_prob_mat)) }))

    }

    nt <- nrow(A_start)
    nk <- ncol(A_start)

    A_vec <- split_vec(A_start)
    A_vec[A_vec <= min_D] <- min_D
    eta_lag_vec <- log(split_vec(lag_prob_mat))

    eta_pi_vec <- log(1 - ascertain_prob)

    A_last_1 <- NULL
    delta_D <- Inf

    E_step_ll_last <- (-Inf)

    for( iter in 1:max_it_E_step ){

      M <- (S %*% A_vec)[,1]


      M[M <= C] <- C[M <= C] + min_D

      U <- M - C

      N <- (J %*% A_vec)[,1]

      M[M <= min_D] <- min_D
      U[U <= min_D] <- min_D

      M <- C + U

      N[N <= min_D] <- min_D

      Lambda <- (W %*% N)[,1] + lambda_0

      G_N <- log(R_t) + log(Lambda) + crossprod(W, N/Lambda)[,1] - crossprod(W, R_t)[,1]
      G_M <- log( 1 - ascertain_prob ) + digamma( M + 1 ) - digamma( U  + 1 )

      if( adjust_prevalence ){

        sero_pi <- crossprod(B_prev, N)[,1]
        ser_diff <- max(abs(sero_pi - S_prev/K_prev))

        if( prev_method == "hyper" ){

          CS_Y <- crossprod(B_cs, N)[,1]
          CS_Y <- pmax(CS_Y, S_prev - 0.99)
          gd <- digamma(CS_Y+1) - digamma(CS_Y-S_prev+1) - digamma(N_pop - CS_Y+1) + digamma(N_pop - CS_Y-K_prev+S_prev+1)

          sero_grad <- prev_weight * (B_cs %*% gd )[,1]

        }else if( prev_method == "normal" ){

          CS_Y <- crossprod(B_cs, N)[,1]
          CS_Y <- pmax(CS_Y, S_prev - 0.99)

          sero_grad <- prev_weight * (B_cs %*% W_prev %*% ( S_prev * (N_pop/K_prev)  - CS_Y ) )[,1]


        }else{

          eps <- 1e-6
          sero_pi <- pmax(sero_pi, eps)
          sero_pi <- pmin(sero_pi, 1 - eps)

          sero_grad <- prev_weight * (B_prev %*% c(
            (S_prev - K_prev * sero_pi)/( sero_pi*(1 - sero_pi) )
          ))[,1]

        }

        G_N <- G_N + sero_grad
      }

      grad <- crossprod(J, G_N)[,1] + crossprod(S, G_M)[,1] + eta_lag_vec - digamma(A_vec + 1)

      #   Solve Hess * x = grad ----

      dL <- Matrix::Diagonal(x = 1/Lambda)
      AA <- crossprod(W, dL)

      A_mat <- AA + t(AA) - (t(W) %*% Matrix::Diagonal( x = N/(Lambda^2) ) %*% W)

      if( adjust_prevalence ){
        if( prev_method == "hyper" ){

          CS_Y <- crossprod(B_cs, N)[,1]
          CS_Y <- pmax(CS_Y, S_prev - 0.99)

          hd <- trigamma(CS_Y+1) - trigamma(CS_Y-S_prev+1) + trigamma(N_pop - CS_Y+1) - trigamma(N_pop - CS_Y-K_prev+S_prev+1)

          Sero_Hess <- prev_weight * B_cs %*% Matrix::Diagonal(x = hd) %*% t(B_cs)
        }else if( prev_method == "normal" ){

          Sero_Hess <- (-prev_weight) * B_cs %*% W_prev %*% t(B_cs)

        }else{
          hd <- S_prev/(sero_pi^2) + (K_prev - S_prev)/( (1 - sero_pi)^2 )
          Sero_Hess <- prev_weight * (-1) * B_prev %*% Matrix::Diagonal(x = hd) %*% t(B_prev)
        }

        A_mat <- A_mat + Sero_Hess
      }

      A_i <- solve(A_mat,tol = 0)

      R_i_diag <- (-1)/trigamma(A_vec + 1)
      R_i <- Matrix::Diagonal(x = R_i_diag )
      D_i <- Matrix::Diagonal(x = 1/(trigamma(M + 1)  - trigamma(M - C + 1)) )

      JRiSt <- tcrossprod(tcrossprod(J,R_i), S)

      # inner_1 <- solve( D_i + tcrossprod(tcrossprod(S,R_i), S))
      inner_1 <- solve( D_i + Matrix::Diagonal( x = (S %*% R_i_diag)[,1] ))

      # inner_2_i <- ( A_i + tcrossprod(J %*% R_i, J) - tcrossprod(JRiSt %*% inner_1, JRiSt) )
      inner_2_i <- ( A_i + Matrix::Diagonal( x = (J %*% R_i_diag)[,1] ) - tcrossprod(JRiSt %*% inner_1, JRiSt) )

      Ri_y <- (R_i %*% grad)[,1]
      Bi_y <- Ri_y - (R_i %*% crossprod(S, (inner_1 %*% (S %*% Ri_y))[,1]) )[,1]

      zz <- (J %*% Bi_y)[,1]
      Ri_z <- (R_i %*% crossprod(J, solve(inner_2_i, zz))[,1])[,1]

      Bi_z <- Ri_z - (R_i %*% crossprod(S,  (inner_1 %*% ((S %*% Ri_z)[,1]))[,1]) )[,1]

      adj <- Bi_y - Bi_z

      # ----

      A_last_1 <- A_vec

      adj <- 0.5 * adj

      # ----

      A_vec <- A_vec - adj

      A_vec[A_vec <= min_D] <- min_D

      delta_D <- max(abs(A_vec - A_last_1))

      if( delta_D < tol_delta ) break;

    }

    A_start <- unsplit_vec(A_vec)

    M <- (S %*% A_vec)[,1]

    M[M <= C] <- C[M <= C] + min_D

    adj_factor <- 1 # sum(C/ascertain_prob)/sum(M)

    M <- M*adj_factor

    U <- M - C

    N <- (J %*% A_vec)[,1]*adj_factor

    M[M <= min_D] <- min_D
    U[U <= min_D] <- min_D

    M <- C + U

    N[N <= min_D] <- min_D

    New_I <- N
    New_A <- M
    New_U <- U

    # ----

    Lambda_0 <- (W %*% New_I)[,1]

    Lambda_new <- as.numeric( Lambda_0 + Lambda_pre + adj_Lambda)

    wneg_u <- which(New_A <= dt$c_cases)
    New_A[wneg_u] <- dt$c_cases[wneg_u]

    dt[,`:=`(
      y_cases = New_I,
      u_cases = New_U,
      Lambda = Lambda_new
    ),]

    ## M step ----

    ### Fit ascertainment model ----

    if( !recycle_start | it <= 2 ){
      pi_start <- NULL
    }

    if( !accelerated_em || it <= 1 ){
      pi_fit <- stats::glm.fit( y = cbind(dt$c_cases, dt$u_cases)[ss,], x = X_pi[ss,], offset = dt$pi_offset[ss], family = binomial_c())
    }

    ### Fit Rt model ----
    if( recovered_offset ){
      p_past_infected <- dt[,cumsum(data.table::shift(y_cases,fill=0))/pop_size,by=region]$V1
      p_past_infected[p_past_infected >= 1] <- 1
      rt_offset <- log(dt$Lambda * (1 - p_past_infected))
    }else{
      rt_offset <- log(dt$Lambda)
    }

    if( !accelerated_em || it <= 1 ){

      rt_fit <- stats::glm.fit(  y = dt$y_cases[ss], x = as.matrix(X_rt[ss,]), offset = rt_offset[ss], family = poisson_c() )

    }

    ### Fit lag models ----

    if( !fix_lag_probs ){
      opt_lag_probs <- optim_lag_probs(A_start, mean_lag)
      lag_probs <- opt_lag_probs$probs
      lag_par <- opt_lag_probs$opt_par
    }else{
      lag_par <- NA
    }

    lag_eta_vec <- split_vec(matrix(log(lag_probs), nrow = nrow(dt), ncol=nk, byrow=TRUE))

    if( estimate_weekday_lag ){

      A_n <- rep_vec(rowSums(A_start))

      lag_fit = fastglm::fastglm(x=X_lag, y=cbind(A_vec,A_n-A_vec), family = binomial_c, method = 2, offset = lag_eta_vec)

    }else{
      lag_fit = NULL
    }

    ## Accelerated EM algorithm ----

    theta <- c(
      coef(rt_fit), coef(pi_fit)
    )
    theta[is.na(theta)] <- 0.00
    if( estimate_weekday_lag){
      theta <- c(theta, coef(lag_fit)[-1])
    }

    xinfo <- calc_information()

    if( !is.null(theta_last) & accelerated_em & it > 1 ){

      diff_check <- theta_diff
      if( is.null(diff_check) ){
        diff_check <- Inf
      }

      acc_nudge <- 0.5

      theta_adj <- solve(
        xinfo$info_naive + acc_nudge*xinfo$adj_mat,
        xinfo$grad_vec
      )[,1]

      theta_acc <- theta_last + theta_adj

      theta_diff <- sum(abs(theta - theta_acc))

      theta <- theta_acc

      w_rt <- (1:ncol(X_rt))
      w_pi <- ncol(X_rt) + (1:ncol(X_pi))

      rt_coef <- theta[w_rt]
      pi_coef <- theta[w_pi]

      if( estimate_weekday_lag){
        lag_coef <- theta[ ncol(X_rt) + ncol(X_pi) + (1:(ncol(X_lag) - 1))]
        lag_coef <- c(0, lag_coef)
      }

    }else{
      theta_diff <- NULL

      rt_coef <- coef(rt_fit)
      rt_coef[is.na(rt_coef)] <- 0.00
      pi_coef <- coef(pi_fit)
      if( estimate_weekday_lag) lag_coef <- coef(lag_fit)
    }
    theta_last <- theta

    # Update parameters ----

    if( recycle_start ) pi_start <- pi_coef
    pi_hat <- (expit(X_pi %*% pi_coef + dt$pi_offset))

    if( recycle_start ) rt_start <- rt_coef
    rt_hat <- exp( X_rt %*% rt_coef )

    if( recovered_offset ){
      rt_hat <- rt_hat * (1 - p_past_infected)
    }

    if( !is.na(min_pi_t) ){
      pi_hat[pi_hat < min_pi_t] <- min_pi_t
    }

    if( estimate_weekday_lag ){
      lag_prob_mat <- exp((X_lag %*% lag_coef)[,1] + lag_eta_vec)
    }else{
      lag_prob_mat <- exp(lag_eta_vec)
    }
    lag_prob_mat <- unsplit_vec(lag_prob_mat)
    lag_prob_mat <- lag_prob_mat/rowSums(lag_prob_mat)

    R_t_last <- dt$R_t
    pi_t_last <- dt$pi_t
    dt[,R_t := rt_hat,]
    dt[,pi_t := pi_hat,]

    #---

    sigma_delta <- max(abs(rt_sigma[ss] - dt$R_t[ss])/rt_sigma[ss], na.rm=TRUE)
    coef_delta <- max(max(abs(R_t_last[ss] - dt$R_t[ss])/R_t_last[ss], na.rm=TRUE), max(abs(pi_t_last[ss] - dt$pi_t[ss])/pi_t_last[ss], na.rm=TRUE))

    # Calculate Q = E(logLik|C) ----

    ll_new <- calc_ll(theta, m_step = FALSE)

    ll_delta <- ll_new - ll_old
    ll_old <- ll_new

    iter_string <- paste0("E-M iter ", it, ": plogLik = ",format(ll_new, digits = 3),", delta = ",format(ll_delta, digits = 3))

    if(  !fix_lag_probs ){
      iter_string <- paste0(iter_string, ", lag_par = ", format(lag_par, digits = 3))
    }

    if( adjust_prevalence ){
      iter_string <- paste0(iter_string, ", prev dif = ", format(ser_diff, digits = 3))
    }

    iter_string <- paste0(iter_string, ", max|grad| = ", format(max(abs(xinfo$grad_vec)), digits = 3))

    cat(paste0(iter_string, "              \r"))

    # Run checks at end of iteration ----

    it <- it + 1
    if( !is.na(plot_every) & it %% plot_every == 0 ) plot_result_mod(dt[ss,], plot.title = state_name)

    if( it > max_it ){
      break;
    }

    if( abs(ll_delta) + max(abs(xinfo$grad_vec)/sqrt(abs(diag(xinfo$info_naive))+0.5)) < tol ){  #& ll_delta > 0 ){
      if( final_pass ){
        break;
      }else{
        final_pass <- TRUE
      }
    }else if( final_pass ){
      final_pass <- FALSE
    }

  }
  cat("\n")

  # Calculate variance-covariance matrix ----

  xinfo <- calc_information(prev_w = info_prev_weight )
  xinfo_w <- calc_information(prev_w = prev_weight)
  xinfo_0 <- calc_information(prev_w = 1e-8 )
  xinfo_1 <- calc_information(prev_w = 1.00 )

  J_mat <- xinfo$info_adj
  J_naive <- xinfo$info_naive

  J_w <- xinfo_w$info_adj
  J_1 <- xinfo_1$info_adj


  if( any(diag( J_mat ) < 0 ) ){
    wneg <- which(diag( J_mat ) < 0)
    warning(paste0(length(wneg), " negative variances in information matrix."))
    J_mat[wneg,] <- J_naive[wneg,]
    J_mat[,wneg] <- J_naive[,wneg]
  }

  w_rt <- (1:ncol(X_rt))
  w_pi <- ncol(X_rt) + (1:ncol(X_pi))
  w_lag <- ncol(X_rt) + ncol(X_pi) + (1:(ncol(X_lag)-1))

  if( estimate_weekday_lag ){
    if(  min(diag(J_mat)[w_lag]) <= 0 ){

      warning("Lag vcov matrix is not invertible.")

      theta_vcov <- MASS::ginv(as.matrix(J_mat[-w_lag, -w_lag]))

      vcov_names <- c(colnames(X_rt), colnames(X_pi))
      colnames(theta_vcov) <- rownames(theta_vcov) <- vcov_names

    }else{

      theta_vcov <- MASS::ginv(as.matrix(J_mat))

      vcov_names <- c(colnames(X_rt), colnames(X_pi), colnames(X_lag)[-1])
      colnames(theta_vcov) <- rownames(theta_vcov) <- vcov_names

    }
  }else{

    theta_vcov <- MASS::ginv(as.matrix(J_mat))

    vcov_names <- c(colnames(X_rt), colnames(X_pi))
    colnames(theta_vcov) <- rownames(theta_vcov) <- vcov_names
  }


  logRt_vcov <- X_rt %*% theta_vcov[w_rt,w_rt] %*% t(X_rt)
  pi_eta_vcov <- X_pi %*% theta_vcov[w_pi,w_pi] %*% t(X_pi)

  logRt_var_diag <- diag(logRt_vcov)
  logRt_var_diag[logRt_var_diag < 0] <- NA

  logitPi_var_diag <- diag(pi_eta_vcov)
  logitPi_var_diag[logitPi_var_diag < 0] <- NA

  dt[, R_t_SE := R_t * sqrt(logRt_var_diag) ,]
  dt[, pi_t_SE := pi_t * (1 - pi_t) * sqrt(logitPi_var_diag) ,]

  #---

  var_i <- rep(NA, nrow(dt))
  var_i[ss] <- diag(xinfo$VarY_C)

  CS <- Matrix::bdiag(
    lapply(base::split(dt$region[ss],dt$region[ss]),
           function(x){
             n <- length(x)
             CS <- matrix(1, n, n)
             CS[upper.tri(CS)] <- 0
             CS
           }
    )
  )

  prev_y <- dt[,list(prev = cumsum(y_cases)/pop_size),by=region]$prev

  var_p <- rep(NA, nrow(dt))
  var_p[ss] <- diag(CS %*% xinfo$VarY_C %*% t(CS))

  psz <- dt$pop_size
  if( is.null(psz) ){
    psz <- as.numeric(NA)
  }

  dt[,`:=`(
    Infections = y_cases,
    Infections_SE = sqrt( var_i ),
    Prevalence = prev_y,
    Prevalence_SE = sqrt(var_p)/psz
  ),]

  SE_rt_par <- sqrt(diag(theta_vcov[w_rt,w_rt] ))
  SE_pi_par <- sqrt(diag(theta_vcov[w_pi,w_pi] ))

  #---

  resid_df <- nrow(X_rt) - ncol(X_rt) - ncol(X_pi)

  par_rt <- data.table("Variable" = colnames(X_rt), "Estimate" = coef(rt_fit), "SE" = SE_rt_par, "P-value" =  pf((coef(rt_fit)/SE_rt_par)^2, 1, resid_df, low=F)  )
  par_pi <- data.table("Variable" = colnames(X_pi), "Estimate" = coef(pi_fit), "SE" = SE_pi_par, "P-value" =  pf((coef(pi_fit)/SE_pi_par)^2, 1, resid_df, low=F)  )

  #---

  if( estimate_weekday_lag ){
    fit_list <- list(rt_fit, pi_fit, lag_fit)
  }else{
    fit_list <- list(rt_fit, pi_fit)
  }

  dt[,obs_weight := rowSums(is_obs_mat * lag_prob_mat),]

  out_list <- list(
    Rt_coef = par_rt, pi_coef = par_pi, plogLik = ll_new, lag_probs = lag_probs,
    data = dt, gradient = xinfo$grad_vec,
    X_rt = X_rt,
    fit_list = fit_list,
    A_matrix = A_start
  )

  if(return_vcov){
    out_list$vcov <- theta_vcov
    out_list$xinfo <- xinfo
    out_list$adj_info_mat <- xinfo$info_adj
    out_list$naive_info_mat <- xinfo$info_naive
    out_list$info_adj_mat <- xinfo$THiTt

    out_list$J_w <- J_w
    out_list$J_1 <- J_1
  }

  options(na.action=na.action.prev)
  invisible(out_list)
}

# plot ----
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
