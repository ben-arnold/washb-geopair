
#----------------------------
# washb-geopair-Functions.R
#
# Relative efficiency of pair
# matching in the WASH Benefits
# trials
#
# Base functions file
#----------------------------



#-------------------------------
# estimate_Imai_ATE
# convenience wrapper to run the 
# ATEcluster() function from the
# experiment package 
#
# and return a matched-pair level 
# data frame with key attributes. 
#
# It also calculates ATE 95% CIs
# based on the arithmetic mean 
# estimator from Imai et al. 2009
#
# It also estimates weighted
# and unweighted pair-wise
# outcome correlation, and 
# corresponding relative efficiency 
# of the matched pair design compared
# with an unmatched design
#
# Arguments, from environment::ATEcluster()
# @Y : outcome (vector)
# @Z : binary indicator of treatmetn (0/1)
# @grp : ID variable for clusters
# @match : ID variable for matched pairs
# @data : dataset that includes all these vars
#
# returns a list that includes:
# est       : ATE estimate, Imai 2009 estimator
# var       : Variance of ATE, Imai 2009 estimator
# est_min95 : lower bound of the 95% CI for ATE
# est_max95 : upper bound of the 95% CI for ATE
# corr_w : pair-wise outcome correlation, weighted
# corr_u : pair-wise outcome correlation, unweighted
# eff_w  : relative efficiencey of pair-matched design to unmatched design, weighted
# eff_u  : relative efficiencey of pair-matched design to unmatched design, unweighted
# dcl    : a data-frame of pair-level estimates, including:
#           Y1bar: mean in treatment clusters
#           Y0bar: mean in control clusters
#           diff: Y1bar-Y0bar
#           n1:  number of observations in treatment clusters (also used as weights)
#           n0:  number of observations in control clusters (also used as weights)
#           w:  pair-level weight (arithmetic mean)
#           w_dk:  pair-level weight (geometric mean, per Donner & Klar 1993)
#-------------------------------

estimate_Imai_ATE <- function(Y,Z,grp,match,data) {
  # experiment package
  require(experiment)
  
  # estimate ATE and cluster-level means
  estATE <- ATEcluster(Y=Y, Z=Z, grp=grp, match=match, data=data)
  
  # estimate 95% CIs based on the artimetic mean weighted variance
  # from Imai et al. 2009
  est_min95 <- estATE$est - 1.96*sqrt(estATE$var)
  est_max95 <- estATE$est + 1.96*sqrt(estATE$var)
  
  # estimate weighted and unweighted correlation between outcomes
  # see section 5.2 of Imai et al. 2009
  wY1bar <- estATE$Y1bar*estATE$w
  wY0bar <- estATE$Y0bar*estATE$w
  corr_w <- 2*cov(wY1bar,wY0bar) / ( var(wY1bar) + var(wY0bar) )
  corr_u <- cor(estATE$Y1bar,estATE$Y0bar)
  
  # estimate relative efficiency compared to an unmatched design
  # with weighted and unweighted correlation
  eff_w <- 1/(1-corr_w)
  eff_u <- 1/(1-corr_u)
  
  # combine cluster level measures in a data.frame()
  dcl <- data.frame(Y1bar=estATE$Y1bar,Y0bar=estATE$Y0bar,diff=estATE$diff,
                    n1=estATE$n1,n0=estATE$n0,w=estATE$w, w_dk=estATE$w.dk)
  
  return(
    list(est = estATE$est, var = estATE$var, est_min95=est_min95, est_max95=est_max95, corr_w=corr_w, corr_u=corr_u, eff_w= eff_w, eff_u=eff_u,  dcl=dcl)
  )
}




#----------------------------
# ATEcluster()
#
# This function is taken from
# the -environment- R package
# The package accompanies 
# the article:
# Imai, K., King, G. & Nall, C. 
# The Essential Role of Pair Matching in Cluster-Randomized Experiments, 
# with Application to the Mexican Universal Health Insurance Evaluation. 
# Stat. Sci. 24, 29–72 (2009).
# library(experiment)
#
# The reason for including it 
# separately from the package
# is to ensure that it is
# available for posterity,
# in case the environment package
# disappears (it doesn't seem to be under active devel.)
#----------------------------

# ATEcluster <- function (Y, Z, grp, data = parent.frame(), match = NULL, weights = NULL, 
#                         fpc = TRUE) 
# {
#   call <- match.call()
#   Y <- eval(call$Y, envir = data)
#   Z <- eval(call$Z, envir = data)
#   grp <- eval(call$grp, envir = data)
#   match <- eval(call$match, envir = data)
#   weights <- eval(call$weights, envir = data)
#   n <- length(Y)
#   res <- list(call = call, n = n, Y = Y, Z = Z, grp = grp, 
#               match = match, weights = weights)
#   if (is.null(match)) 
#     stop("This option is not yet available.")
#   else {
#     res$m <- m <- length(unique(match))
#     res$Y1bar <- Y1bar <- tapply(Y[Z == 1], match[Z == 1], 
#                                  mean)
#     res$Y0bar <- Y0bar <- tapply(Y[Z == 0], match[Z == 0], 
#                                  mean)
#     res$diff <- diff <- Y1bar - Y0bar
#     res$n1 <- n1 <- tapply(rep(1, sum(Z == 1)), match[Z == 
#                                                         1], sum)
#     res$n0 <- n0 <- tapply(rep(1, sum(Z == 0)), match[Z == 
#                                                         0], sum)
#   }
#   if (is.null(weights)) {
#     N1 <- w1 <- n1
#     N0 <- w0 <- n0
#   }
#   else {
#     w1 <- N1 <- tapply(weights[Z == 1], match[Z == 1], mean)
#     w0 <- N0 <- tapply(weights[Z == 0], match[Z == 0], mean)
#   }
#   w <- w1 + w0
#   w <- n * w/sum(w)
#   ATE.est <- weighted.mean(diff, w)
#   ATE.var <- m * sum((w * diff - n * ATE.est/m)^2)/((m - 1) * 
#                                                       (n^2))
#   w.dk <- w1 * w0/(w1 + w0)
#   w.dk <- n * w.dk/sum(w.dk)
#   ATEdk.est <- weighted.mean(diff, w.dk)
#   ATEdk.var <- sum(w.dk^2) * sum(w.dk * (diff - ATEdk.est)^2)/(n^3)
#   ATE.dkvar <- sum(w^2) * sum(w * (diff - ATE.est)^2)/(n^3)
#   if (!is.null(weights)) {
#     Y1var <- tapply(Y[Z == 1], match[Z == 1], var)/n1
#     Y0var <- tapply(Y[Z == 0], match[Z == 0], var)/n0
#     if (fpc) {
#       Y1var <- (1 - n1/N1) * Y1var
#       Y0var <- (1 - n0/N0) * Y0var
#       if ((sum(n0 > N0) + sum(n1 > N1)) > 0) 
#         stop("population size is smaller than sample size")
#     }
#     res$Y1var <- Y1var
#     res$Y0var <- Y0var
#     res$var.lb <- sum((w/n)^2 * (Y1var + Y0var))
#   }
#   res$est <- ATE.est
#   res$est.dk <- ATEdk.est
#   res$var <- ATE.var
#   res$dkvar <- ATE.dkvar
#   res$var.dk <- ATEdk.var
#   res$eff <- 1/(1 - 2 * cov(w * Y1bar, w * Y0bar)/(var(w * 
#                                                          Y1bar) + var(w * Y0bar)))
#   res$w <- w
#   res$w.dk <- w.dk
#   if (!is.null(match)) {
#     res$w1 <- w1
#     res$w0 <- w0
#     res$N0 <- N0
#     res$N1 <- N1
#   }
#   class(res) <- "ATEcluster"
#   return(res)
# }



