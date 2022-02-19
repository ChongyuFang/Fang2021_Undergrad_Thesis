# Prepared by Chongyu Fang, Jan 2021
# An investigation of output, price and monetary policy in China
# We use structural VAR model by Sims (1980)
# local projection methods by Jordà (2005)
# and threshold regression by Hansen (2000)

setwd("~/Desktop/Fang2021_Undergrad_Thesis/data")


##### Data #####

# import data
GDPgrowth = read.table("china_gdpgrowth.csv", header = T, sep = ",")
M2growth = read.table("china_M2growth.csv", header = T, sep = ",")
CPIinf = read.table("china_cpiinflation.csv", header = T, sep = ",")

# identify time series
GDPgrowth = ts(GDPgrowth[,2], start = c(1992,1), frequency = 4)
M2growth = ts(M2growth[,2], start = c(1993,4), frequency = 4)
CPIinf = ts(CPIinf[,2], start = c(1987,1), frequency = 4)

# match time window
GDPgrowth = ts.intersect(GDPgrowth, M2growth, CPIinf)[, "GDPgrowth"]
M2growth = ts.intersect(GDPgrowth, M2growth, CPIinf)[, "M2growth"]
CPIinf = ts.intersect(GDPgrowth, M2growth, CPIinf)[, "CPIinf"]

# plot the series
# and label threshold values beforehand
plot(GDPgrowth, main = "China GDP Growth", lwd = 2)
plot(CPIinf, main = "China CPI Inflation", lwd = 2)
plot(M2growth, main = "China M2 Growth", lwd = 2)

# check stationarity
library("tseries")
adf.test(GDPgrowth)
pp.test(GDPgrowth)
kpss.test(GDPgrowth)
adf.test(CPIinf)
pp.test(CPIinf)
kpss.test(CPIinf)
adf.test(M2growth)
pp.test(M2growth)
kpss.test(M2growth)

##### VAR #####

# create panel data
econsystem = cbind(GDPgrowth,CPIinf,M2growth)
plot(econsystem, yax.flip = TRUE, main = "China Economic System")

# select VAR order
library("vars")
VARselect(econsystem, lag.max = 4, type = "const")
VARselect(econsystem, lag.max = 8, type = "const")
VARselect(econsystem, lag.max = 12, type = "const")
VARselect(econsystem, lag.max = 16, type = "const")
VARselect(econsystem, lag.max = 20, type = "const")

# p=3 lags
# construct VAR
VAR_econsystem = VAR(econsystem, p = 3)
summary(VAR_econsystem)
roots(VAR_econsystem)
plot(VAR_econsystem)

# acquire impulse responses
irf_econsystem = irf(VAR_econsystem, ortho = FALSE, ci = 0.95, runs = 100, n.ahead = 16)
plot(irf_econsystem, lwd = 2)

# acquire structural impulse responses
sirf_econsystem = irf(VAR_econsystem, ortho = TRUE, ci = 0.95, runs = 100, n.ahead = 16)
plot(sirf_econsystem, lwd = 2)

##### Hansen Threshold Estimation #####

# Hansen (2000) threshold regression
library("tcltk") # already a base package in R, please download XQuartz for MacOS
library("pdR")

# generate lags as threshold variable as in Hansen (2000)
GDPgrowth_l1 = lag(GDPgrowth,-1)
GDPgrowth_l2 = lag(GDPgrowth,-2)
GDPgrowth_l3 = lag(GDPgrowth,-3)
GDPgrowth_l4 = lag(GDPgrowth,-4)
GDPgrowth_l5 = lag(GDPgrowth,-5)
GDPgrowth_l6 = lag(GDPgrowth,-6)
GDPgrowth_l7 = lag(GDPgrowth,-7)
GDPgrowth_l8 = lag(GDPgrowth,-8)
GDPgrowth_l9 = lag(GDPgrowth,-9)
GDPgrowth_l10 = lag(GDPgrowth,-10)
GDPgrowth_l11 = lag(GDPgrowth,-11)
GDPgrowth_l12 = lag(GDPgrowth,-12)

M2growth_l1 = lag(M2growth,-1)
M2growth_l2 = lag(M2growth,-2)
M2growth_l3 = lag(M2growth,-3)
M2growth_l4 = lag(M2growth,-4)
M2growth_l5 = lag(M2growth,-5)
M2growth_l6 = lag(M2growth,-6)
M2growth_l7 = lag(M2growth,-7)
M2growth_l8 = lag(M2growth,-8)
M2growth_l9 = lag(M2growth,-9)
M2growth_l10 = lag(M2growth,-10)
M2growth_l11 = lag(M2growth,-11)
M2growth_l12 = lag(M2growth,-12)

CPIinf_l1 = lag(CPIinf,-1)
CPIinf_l2 = lag(CPIinf,-2)
CPIinf_l3 = lag(CPIinf,-3)
CPIinf_l4 = lag(CPIinf,-4)
CPIinf_l5 = lag(CPIinf,-5)
CPIinf_l6 = lag(CPIinf,-6)
CPIinf_l7 = lag(CPIinf,-7)
CPIinf_l8 = lag(CPIinf,-8)
CPIinf_l9 = lag(CPIinf,-9)
CPIinf_l10 = lag(CPIinf,-10)
CPIinf_l11 = lag(CPIinf,-11)
CPIinf_l12 = lag(CPIinf,-12)

# threshold regression
econsystem = cbind(GDPgrowth, CPIinf, M2growth,
                   GDPgrowth_l1,GDPgrowth_l2,GDPgrowth_l3,GDPgrowth_l4,
                   GDPgrowth_l5,GDPgrowth_l6,GDPgrowth_l7,GDPgrowth_l8,
                   GDPgrowth_l9,GDPgrowth_l10,GDPgrowth_l11,GDPgrowth_l12,
                   CPIinf_l1,CPIinf_l2,CPIinf_l3,CPIinf_l4,
                   CPIinf_l5,CPIinf_l6,CPIinf_l7,CPIinf_l8,
                   CPIinf_l9,CPIinf_l10,CPIinf_l11,CPIinf_l12,
                   M2growth_l1,M2growth_l2,M2growth_l3,M2growth_l4,
                   M2growth_l5,M2growth_l6,M2growth_l7,M2growth_l8,
                   M2growth_l9,M2growth_l10,M2growth_l11,M2growth_l12
                   )

# null values cannot be regressed on
econsystem = econsystem[13:108,]

# dependent is GDPgrowth, threshold is GDPgrowth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_GDPgrowth=SMPLSplit_het(dat=econsystem,
                                           dep=1,
                                           indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                   16,17,18,19,20,21,22,23,24,25,26,27,
                                                   28,29,30,31,32,33,34,35,36,37,38,39),
                                           th=(3+i),
                                           trim_per=0.1,
                                           rep=1000,
                                           plot=0)
}

# dependent is CPIinf, threshold is GDPgrowth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_GDPgrowth=SMPLSplit_het(dat=econsystem,
                                           dep=2,
                                           indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                   16,17,18,19,20,21,22,23,24,25,26,27,
                                                   28,29,30,31,32,33,34,35,36,37,38,39),
                                           th=(3+i),
                                           trim_per=0.1,
                                           rep=1000,
                                           plot=0)
}

# dependent is M2growth, threshold is GDPgrowth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_GDPgrowth=SMPLSplit_het(dat=econsystem,
                                           dep=3,
                                           indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                   16,17,18,19,20,21,22,23,24,25,26,27,
                                                   28,29,30,31,32,33,34,35,36,37,38,39),
                                           th=(3+i),
                                           trim_per=0.1,
                                           rep=1000,
                                           plot=0)
}

# dependent is GDPgrowth, threshold is CPIinf lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_CPIinf=SMPLSplit_het(dat=econsystem,
                                        dep=1,
                                        indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                16,17,18,19,20,21,22,23,24,25,26,27,
                                                28,29,30,31,32,33,34,35,36,37,38,39),
                                        th=(15+i),
                                        trim_per=0.1,
                                        rep=1000,
                                        plot=0)
}

# dependent is CPIinf, threshold is CPIinf lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_CPIinf=SMPLSplit_het(dat=econsystem,
                                        dep=2,
                                        indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                16,17,18,19,20,21,22,23,24,25,26,27,
                                                28,29,30,31,32,33,34,35,36,37,38,39),
                                        th=(15+i),
                                        trim_per=0.1,
                                        rep=1000,
                                        plot=0)
}

# dependent is M2growth, threshold is CPIinf lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_CPIinf=SMPLSplit_het(dat=econsystem,
                                        dep=3,
                                        indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                16,17,18,19,20,21,22,23,24,25,26,27,
                                                28,29,30,31,32,33,34,35,36,37,38,39),
                                        th=(15+i),
                                        trim_per=0.1,
                                        rep=1000,
                                        plot=0)
}

# dependent is GDPgrowth, threshold is M2growth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_M2growth=SMPLSplit_het(dat=econsystem,
                                          dep=1,
                                          indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                  16,17,18,19,20,21,22,23,24,25,26,27,
                                                  28,29,30,31,32,33,34,35,36,37,38,39),
                                          th=(27+i),
                                          trim_per=0.1,
                                          rep=1000,
                                          plot=0)
}

# dependent is CPIinf, threshold is M2growth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_M2growth=SMPLSplit_het(dat=econsystem,
                                          dep=2,
                                          indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                  16,17,18,19,20,21,22,23,24,25,26,27,
                                                  28,29,30,31,32,33,34,35,36,37,38,39),
                                          th=(27+i),
                                          trim_per=0.1,
                                          rep=1000,
                                          plot=0)
}

# dependent is M2growth, threshold is M2growth lag 1-12, respectively
for (i in 1:12){
  ThresholdResults_M2growth=SMPLSplit_het(dat=econsystem,
                                          dep=3,
                                          indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                  16,17,18,19,20,21,22,23,24,25,26,27,
                                                  28,29,30,31,32,33,34,35,36,37,38,39),
                                          th=(27+i),
                                          trim_per=0.1,
                                          rep=1000,
                                          plot=0)
}

# based on results above and according to Jorda's selection methods, we acquire thresholds
# to illustrate, we generate likelihood ratio plots accordingly

# threshold for GDPgrowth is (9.8%+9.8%)/2 = 9.8%
ThresholdResults_GDPgrowth=SMPLSplit_het(dat=econsystem,
                                         dep=2,
                                         indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                 16,17,18,19,20,21,22,23,24,25,26,27,
                                                 28,29,30,31,32,33,34,35,36,37,38,39),
                                         th=15, # 12th lag of GDPgrowth
                                         trim_per=0.1,
                                         rep=1000,
                                         plot=1)

ThresholdResults_GDPgrowth=SMPLSplit_het(dat=econsystem,
                                         dep=3,
                                         indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                 16,17,18,19,20,21,22,23,24,25,26,27,
                                                 28,29,30,31,32,33,34,35,36,37,38,39),
                                         th=13, # 10th lag of GDPgrowth
                                         trim_per=0.1,
                                         rep=1000,
                                         plot=1)

# threshold for CPIinf is (2.066667%+2.033333%)/2 = 2.05%
ThresholdResults_CPIinf=SMPLSplit_het(dat=econsystem,
                                      dep=2,
                                      indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                              16,17,18,19,20,21,22,23,24,25,26,27,
                                              28,29,30,31,32,33,34,35,36,37,38,39),
                                      th=16, # 1st lag of CPIinf
                                      trim_per=0.1,
                                      rep=1000,
                                      plot=1)

ThresholdResults_CPIinf=SMPLSplit_het(dat=econsystem,
                                      dep=3,
                                      indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                              16,17,18,19,20,21,22,23,24,25,26,27,
                                              28,29,30,31,32,33,34,35,36,37,38,39),
                                      th=25, # 10th lag of CPIinf
                                      trim_per=0.1,
                                      rep=1000,
                                      plot=1)

# threshold for M2growth is (16.5%+15.85%)/2 = 16.175%
ThresholdResults_M2growth=SMPLSplit_het(dat=econsystem,
                                        dep=2,
                                        indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                16,17,18,19,20,21,22,23,24,25,26,27,
                                                28,29,30,31,32,33,34,35,36,37,38,39),
                                        th=37, # 10th lag of M2growth
                                        trim_per=0.1,
                                        rep=1000,
                                        plot=1)

ThresholdResults_M2growth=SMPLSplit_het(dat=econsystem,
                                        dep=3,
                                        indep=c(4,5,6,7,8,9,10,11,12,13,14,15,
                                                16,17,18,19,20,21,22,23,24,25,26,27,
                                                28,29,30,31,32,33,34,35,36,37,38,39),
                                        th=28, # 1st lag of M2growth
                                        trim_per=0.1,
                                        rep=1000,
                                        plot=1)

##### Local Projection #####

# load packages for local projections
library("lpirfs")
library("ggpubr")
library("gridExtra")

# after acquiring thresholds, we restore original econsystem
econsystem = cbind(GDPgrowth,CPIinf,M2growth)

# local projections must be computed under data frame
econsystem = data.frame(econsystem)

# estimate linear local projection model
# set lags to be 3 (as in VAR before), unit shock
# compare impulse responses from linear LP and VAR
econsystem_lin_lp = lp_lin(endog_data = econsystem, lags_endog_lin = 3,
                           trend = 0, shock_type = 1,
                           confint = 1.96, hor = 16)

# show the impulse responses
econsystem_lin_lp_plot = plot_lin(econsystem_lin_lp)
econsystem_lin_lp_plot_all = sapply(econsystem_lin_lp_plot, ggplotGrob)
marrangeGrob(econsystem_lin_lp_plot_all, nrow = 3, ncol = 3, top = NULL)

# estimate NON-linear and regime-dependent models, set lags to be 3 (as in VAR before)
# display asymmetric effects of monetary policy shocks

# situation (1): inflation regime-dependent
# for high inflation regime and low inflation regime
# inflation regime threshold is 2.05%
plot(CPIinf, main = "China CPI Inflation", lwd = 2)
abline(a = 2.05, b = 0, col = "blue", lwd = 2)

# create dummy: apply threshold of 2.05% to the first lag of inflation rate
switching_data = ifelse(dplyr::lag(econsystem$CPIinf, 1) > 2.05, 1, 0)

econsystem_nonlin_lp = lp_nl(endog_data = econsystem,
                             lags_endog_lin = 3,           lags_endog_nl = 3,
                             trend = 1,                    shock_type = 0,
                             confint = 1.96,               hor = 16,
                             switching = switching_data,   lag_switching = FALSE,
                             use_logistic = FALSE)

# transition function approach instead of dummy approach
econsystem_nonlin_lp = lp_nl(endog_data = econsystem,
                             lags_endog_lin = 3,           lags_endog_nl = 3,
                             trend = 1,                    shock_type = 0,
                             confint = 1.96,               hor = 16,
                             switching = CPIinf,           lag_switching = TRUE,
                             use_hp = TRUE,                lambda = 1600,
                             use_logistic = TRUE)

# create nonlinear impulse responses
econsystem_nonlin_lp_plot = plot_nl(econsystem_nonlin_lp)

# combine and show the impulse responses plots
single_plots = econsystem_nonlin_lp_plot$gg_s1[c(3,6,9)]
single_plots[4:6] = econsystem_nonlin_lp_plot$gg_s2[c(3,6,9)]
all_plots = sapply(single_plots, ggplotGrob)
marrangeGrob(all_plots, nrow = 3, ncol = 2, top = NULL)

# situation (2): GDP growth regime-dependent
# for high growth regime and low growth regime
# inflation regime threshold is 9.8%
plot(GDPgrowth, main = "China GDP Growth", lwd = 2)
abline(a = 9.8, b = 0, col = "blue", lwd = 2)

# create dummy: apply threshold of 9.8% to the tenth lag of inflation rate
switching_data = ifelse(dplyr::lag(econsystem$GDPgrowth, 10) > 9.8, 1, 0)

econsystem_nonlin_lp = lp_nl(endog_data = econsystem,
                             lags_endog_lin = 3,           lags_endog_nl = 3,
                             trend = 1,                    shock_type = 0,
                             confint = 1.96,               hor = 16,
                             switching = switching_data,   lag_switching = FALSE,
                             use_logistic = FALSE)

# transition function approach instead of dummy approach
econsystem_nonlin_lp = lp_nl(endog_data = econsystem,
                             lags_endog_lin = 3,           lags_endog_nl = 3,
                             trend = 1,                    shock_type = 0,
                             confint = 1.96,               hor = 16,
                             switching = GDPgrowth,        lag_switching = TRUE,
                             use_hp = TRUE,                lambda = 1600,
                             use_logistic = TRUE)

# create nonlinear impulse responses
econsystem_nonlin_lp_plot = plot_nl(econsystem_nonlin_lp)

# combine and show the impulse responses plots
single_plots = econsystem_nonlin_lp_plot$gg_s1[c(3,6,9)]
single_plots[4:6] = econsystem_nonlin_lp_plot$gg_s2[c(3,6,9)]
all_plots = sapply(single_plots, ggplotGrob)
marrangeGrob(all_plots, nrow = 3, ncol = 2, top = NULL)




