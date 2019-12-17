

f_BS_price = function(S, K,  TT, vol, r, isCall) {
  # Function to price options using Black Scholes formula
  
  # input:
  #      double S: Underlying price
  #      double K: Strike price
  #      double TT: Time to maturity(in years)
  #      double vol: Volatility (annualized)
  #      double r:  constant risk-free rate (annualized)
  #      boolean isCall: True for Call, False for Put
  
  # output:
  #      double C or P: The price of the option
  d1 = (log(S / K) + (r + (vol ** 2) / 2) * TT) / (vol * sqrt(TT))
  d2 = (log(S / K) + (r - (vol ** 2) / 2) * TT) / (vol * sqrt(TT))
  C = S  * pnorm(d1) - exp(-r * TT) * K * pnorm(d2)
  if (isCall) {
    return(C)
  } else{
    P = exp(-r * TT) * K + C - S
    return(P) # Put call parity
  }
}


# Create a vectorized version of the Black-Scholes pricer function
f_vBS_price = Vectorize(f_BS_price, c("S" , "K", "TT", "vol", "r", "isCall"))


f_rf_interpolate = function(t1, rf) {
  # Function to find rate at a given time by interpolation given term structure
  
  # input:
  #      double t1: time at which we want to determine the rate, t1 should be
  #                 inside the term structure maturity intervals
  #      double rf: Named vector of risk-free rate, with the names being the
  #                 maturities of the term structure.
  
  # output:
  #       double r: risk-free rate at t1
  
  rf_mat = as.numeric(names(rf)) # risk free term structure maturities
  rate_vec = as.numeric(rf)
  low_idx = findInterval(t1, rf_mat)
  r = (t1 - rf_mat[low_idx]) / (rf_mat[low_idx + 1] - rf_mat[low_idx]) *
    (rate_vec[low_idx + 1] - rate_vec[low_idx]) + rate_vec[low_idx]
  
  return(r)
}


f_fwd_rate = function(t1, t2, rf) {
  # Function to find forward rate between two dates t1 and t2 given the
  # term structure rf. The function uses f_rf_interpolate
  
  #input:
  #      double t1: start date of the forward rate
  #      double t2: end date of the forward rate
  
  #output:
  #      double fwd: Forward rate between t1 and t2
  
  r1 = f_rf_interpolate(t1, rf)
  r2 = f_rf_interpolate(t2, rf)
  fwd = (t2 * r2 - t1 * r1) / (t2 - t1)
  return(fwd)
}


f_hist = function(data, threshold, xlab, title = NULL) {
  # Function to draw the histogram of data and the value of the threshold
  
  #input
  #  double data: A vector for which we want the histogram
  #  double threshold: A given threshold to be put on the graph (e.g VaR)
  #  string xlab : Label of the x-axis
  #  string title: Title of the plot
  
  #output:
  #  Plotted graph
  
  hist(
    data,
    breaks = 10 * round(log(length(data))) ,
    xlab = xlab,
    main = title,
  )
  abline(v = threshold, col = "red", lwd = 2)
  
}


f_parametric_IV = function(params, m, tau) {
  alpha1 = params[1]
  alpha2 = params[2]
  alpha3 = params[3]
  alpha4 = params[4]
  
  stopifnot(length(m) == length(tau)) #Check moneyness and tau have same length
  sigma = alpha1 + alpha2 * (m - 1) ** 2 + alpha3 * (m - 1) ** 3 +
    alpha4 * sqrt(tau)
  return(sigma)
}


f_obj_fn = function(params, options_data) {
  m_vec = options_data[, 1]
  tau_vec = options_data[, 3]
  IV_vec = options_data[, 4]
  sigma_model = f_parametric_IV(params, m_vec, tau_vec)
  return(sum(abs(sigma_model - IV_vec)))
}
