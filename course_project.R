# Author: Xinghan Guo
# Date: Aug 18, 2022
options(digits=3, width=70)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(boot)
library(corrplot)

asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")
start.date = "2014-10-01"
end.date = "2019-10-30"

vfinx.prices = get.hist.quote(instrument="vfinx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")    
veurxs
veiex.prices = get.hist.quote(instrument="veiex", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbltx.prices = get.hist.quote(instrument="vbltx", start=start.date,
                             end=end.date, quote="AdjClose",
                             # provider="yahoo", 
                             # origin="1970-01-01",
                             # compression="m", 
                             retclass="zoo")
vbltx.prices = to.monthly(vbltx.prices)$vbltx.prices.Close
vbisx.prices = get.hist.quote(instrument="vbisx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
                             
index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbltx.prices) = as.yearmon(index(vbltx.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names

projectPrices.df = coredata(projectPrices.z)
rownames(projectPrices.df) = as.character(index(projectPrices.z))


projectReturns.z = diff(log(projectPrices.z))   
projectReturnsSimple.z = exp(projectReturns.z) - 1

projectReturns.df = as.data.frame(coredata(projectReturns.z))
rownames(projectReturns.df) = as.character(index(projectReturns.z))
projectReturnsSimple.df = as.data.frame(coredata(projectReturnsSimple.z))
rownames(projectReturnsSimple.df) = as.character(index(projectReturnsSimple.z))

```{r}
# 1.1 
# Compute time plots of monthly prices and continuously compounded returns and comments. Are there any unusually large or small returns? Can you identify any news events that may explain these unusual values? Give a plot showing the growth of $1 in each of the funds over the five year period (recall, this is called an "equity curve"). Which fund gives the highest future value? Are you surprised?

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(projectPrices.z, col="blue", lwd=2)
plot(projectReturns.z, panel=my.panel, col="blue", lwd=2)

# plot growth of $1 over the five years using PerformanceAnalytics function
# chart.CumReturns
chart.CumReturns(projectReturnsSimple.z, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="growth of $1") 
```
                             
```{r}
# 1.2
# Create four-panel diagnostic plots containing histograms, boxplots, Q-Q-plots, and SACFs for each return series and comment. Do the returns look normally distributed? Are there any outliers in the data? Is there any evidence of linear time dependence? Also, create a boxplot showing the distributions of all of the assets in one graph.

fourPanelPlot(projectReturns.z[, "vfinx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veurx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veiex", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbltx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbisx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vpacx", drop=FALSE])
ret.mat = coredata(projectReturns.z)
boxplot(ret.mat, main="Vanguard Returns", col="cornflowerblue")
```

```{r}
# 1.3
# Compute univariate descriptive statistics (mean, variance, standard deviation, skewness, kurtosis, quantiles) for each return series and comment. Which funds have the highest and lowest average return? Which funds have the highest and lowest standard deviation? Which funds look most and least normally distributed?

muhat.vals = colMeans(projectReturns.z)
muhat.mat = as.matrix(muhat.vals)
sd.vals = apply(projectReturns.z, 2, sd)
skew.vals = apply(projectReturns.z, 2, skewness)
ekurt.vals = apply(projectReturns.z, 2, kurtosis)
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("vfinx,veurx","vfinx,veiex","vfinx,vbltx", "vfinx,vbisx", "vfinx,vpacx",
    "veurx,veiex", "veurx,vbltx", "veurx,vbisx", "veurx,vpacx",
    "veiex,vbltx", "veiex,vbisx", "veiex,vpacx",
    "vbltx,vbisx", "vbltx,vpacx",
    "vbisx,vpacx")
q.vals = apply(projectReturns.z, 2, quantile, prob=c(0.01,0.05))
# display results in a table
stats.mat = rbind(muhat.vals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) = c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")
stats.mat
```

```{r}
# 1.4
# Using a monthly risk-free rate equal to 0.0004167 per month (which corresponds to a continuously compounded annual rate of 0.5%), compute Sharpe's slope/ratio for each asset. Use the bootstrap to calculate estimated standard errors for the Sharpe ratios. Arrange these values nicely in a table. Which asset has the highest slope? Are the Sharpe slopes estimated precisely?

ret.mat = coredata(projectReturns.z)

rf = 0.005/12
SharpeRatios = (muhat.vals - rf)/sd.vals
SharpeRatios

sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}

sharpe.vfinx.boot = boot(ret.mat[, "vfinx"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veurx.boot = boot(ret.mat[, "veurx"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veiex.boot = boot(ret.mat[, "veiex"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbltx.boot = boot(ret.mat[, "vbltx"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbisx.boot = boot(ret.mat[, "vbisx"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vpacx.boot = boot(ret.mat[, "vpacx"], statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vfinx.boot
sharpe.veurx.boot
sharpe.veiex.boot
sharpe.vbltx.boot
sharpe.vbisx.boot
sharpe.vpacx.boot
df_1.4 <- data.frame(SharpeRatios,SE=c(0.141, 0.135, 0.13, 0.124, 0.131, 0.134))
df_1.4
```

```{r}
# 1.5
# Compute estimated standard errors and form 95% confidence intervals for the estimates of the mean and standard deviation. Arrange these values nicely in a table. Are these means and standard deviations estimated very precisely? Which estimates are more precise: the estimated means or standard deviations?

n.obs = nrow(projectReturns.z)
se.muhat = sd.vals/sqrt(n.obs) 
se.sigmahat = sd.vals/sqrt(2*n.obs)

mu.lower.95 = muhat.vals - qnorm(0.975)*se.muhat
mu.upper.95 = muhat.vals + qnorm(0.975)*se.muhat
sigma.lower.95 = sd.vals - qnorm(0.975)*se.sigmahat
sigma.upper.95 = sd.vals + qnorm(0.975)*se.sigmahat

df_1.5.1 <- data.frame(muhat.vals, se.muhat, mu.lower.95, mu.upper.95)
df_1.5.2 <- data.frame(sd.vals, se.sigmahat, sigma.lower.95, sigma.upper.95)


cbind(muhat.vals, se.muhat, mu.lower.95, mu.upper.95)
cbind(sd.vals, se.sigmahat, sigma.lower.95, sigma.upper.95)

df_1.5.1
df_1.5.2
```

```{r}
# 1.6
# Convert the monthly sample means into annual estimates by multiplying by 12 and convert the monthly sample SDs into annual estimates by multiplying by the square root of 12. Comment on the values of these annual numbers. Using these values, compute annualized Sharpe ratios. Are the asset rankings the same as with the monthly Sharpe ratios? Assuming you get the average annual return every year for 5 years, how much would $1 grow to after 5 years? (Remember, the annual return you compute is a cc annual return).

rf_annual = 0.005
annual_SharpeRatios = (muhat.vals*12 - rf_annual)/(sd.vals*sqrt(12))
annual_SharpeRatios
df.1.6 = data.frame(annual_SharpeRatios)
df.1.6

r_fiveYears = (1+muhat.vals*12)^5
df.1.6.2 = data.frame(r_fiveYears)
df.1.6.2
```

```{r}
# 1.7
# Compute and plot all pair-wise scatterplots between your 6 assets. Briefly comment on any relationships you see.
# Compute the sample covariance matrix of the returns on your six assets and comment on the direction of linear association between the asset returns. 

ret.mat = coredata(projectReturns.z)
pairs(ret.mat, col="blue")
df.1.7 = cov.mat
df.1.7
```

```{r}
# 1.8
# Compute the sample correlation matrix of the returns on your six assets and plot this correlation matrix using the R corrplot package function corrplot(). Which assets are most highly correlated?  Which are least correlated? Based on the estimated correlation values do you think diversification will reduce risk with these assets?
cor.mat
corrplot(cor.mat, method="ellipse")
```


```{r}
# 2.1
# Assume that you have $100,000 to invest starting at  October 30, 2019.  For each asset, determine the 1% and 5% value-at-risk of the $100,000 investment over a one-month investment horizon based on the normal distribution using the estimated means and variances of your assets.  
# Use the bootstrap to compute estimated standard errors and 95% confidence intervals for your 1% and 5% VaR estimates. Create a table showing the 1% and 5% VaR estimates along with the bootstrap standard errors and 95% confidence intervals.
# Using these results, comment on the precision of your VaR estimates. Which assets have the highest and lowest VaR at each horizon?

Value.at.Risk = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05 = Value.at.Risk(ret.mat, p=0.05, method="normal")
VaR.normal.05
VaR.normal.01 = Value.at.Risk(ret.mat, p=0.01)
VaR.normal.01
df.2.1 = data.frame(VaR.normal.01, VaR.normal.05)



```

```{r}
ValueAtRisk.boot.0.05 = function(x, idx, p=0.05, w=100000) {
    q = mean(x[idx]) + sd(x[idx])*qnorm(p)
    VaR = (exp(q) - 1)*w
    VaR
}
vfinx.VaR.boot.0.05 = boot(ret.mat[,"vfinx"], statistic = ValueAtRisk.boot.0.05, R=999)
vfinx.0.95.ci.boot.0.05 = boot.ci(vfinx.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))
veurx.VaR.boot.0.05 = boot(ret.mat[,"veurx"], statistic = ValueAtRisk.boot.0.05, R=999)
veurx.0.95.ci.boot.0.05 = boot.ci(veurx.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))
veiex.VaR.boot.0.05 = boot(ret.mat[,"veiex"], statistic = ValueAtRisk.boot.0.05, R=999)
veiex.0.95.ci.boot.0.05 = boot.ci(veiex.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))
vbltx.VaR.boot.0.05 = boot(ret.mat[,"vbltx"], statistic = ValueAtRisk.boot.0.05, R=999)
vbltx.0.95.ci.boot.0.05 = boot.ci(vbltx.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))
vbisx.VaR.boot.0.05 = boot(ret.mat[,"vbisx"], statistic = ValueAtRisk.boot.0.05, R=999)
vbisx.0.95.ci.boot.0.05 = boot.ci(vbisx.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))
vpacx.VaR.boot.0.05 = boot(ret.mat[,"vpacx"], statistic = ValueAtRisk.boot.0.05, R=999)
vpacx.0.95.ci.boot.0.05 = boot.ci(vpacx.VaR.boot.0.05, conf=0.95, type=c("norm", "perc"))

vfinx.VaR.boot.0.05
vfinx.0.95.ci.boot.0.05
veurx.VaR.boot.0.05
veurx.0.95.ci.boot.0.05
veiex.VaR.boot.0.05
veiex.0.95.ci.boot.0.05
vbltx.VaR.boot.0.05
vbltx.0.95.ci.boot.0.05
vbisx.VaR.boot.0.05
vbisx.0.95.ci.boot.0.05
vpacx.VaR.boot.0.05
vpacx.0.95.ci.boot.0.05

df_2.1.1 <- data.frame(VaR.normal.05, SE=c(846, 681, 706, 503, 73.4, 770), 
                       lower_bound=c(-6587, -7408, -8355, -4672, -710, -7151),
                       upper_bound=c(-3269, -4739, -5589, -2701, -423, -4131))

df_2.1.1
```

```{r}
ValueAtRisk.boot.0.01 = function(x, idx, p=0.01, w=100000) {
    q = mean(x[idx]) + sd(x[idx])*qnorm(p)
    VaR = (exp(q) - 1)*w
    VaR
}
vfinx.VaR.boot.0.01 = boot(ret.mat[,"vfinx"], statistic = ValueAtRisk.boot.0.01, R=999)
vfinx.0.95.ci.boot.0.01 = boot.ci(vfinx.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))
veurx.VaR.boot.0.01 = boot(ret.mat[,"veurx"], statistic = ValueAtRisk.boot.0.01, R=999)
veurx.0.95.ci.boot.0.01 = boot.ci(veurx.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))
veiex.VaR.boot.0.01 = boot(ret.mat[,"veiex"], statistic = ValueAtRisk.boot.0.01, R=999)
veiex.0.95.ci.boot.0.01 = boot.ci(veiex.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))
vbltx.VaR.boot.0.01 = boot(ret.mat[,"vbltx"], statistic = ValueAtRisk.boot.0.01, R=999)
vbltx.0.95.ci.boot.0.01 = boot.ci(vbltx.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))
vbisx.VaR.boot.0.01 = boot(ret.mat[,"vbisx"], statistic = ValueAtRisk.boot.0.01, R=999)
vbisx.0.95.ci.boot.0.01 = boot.ci(vbisx.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))
vpacx.VaR.boot.0.01 = boot(ret.mat[,"vpacx"], statistic = ValueAtRisk.boot.0.01, R=999)
vpacx.0.95.ci.boot.0.01 = boot.ci(vpacx.VaR.boot.0.01, conf=0.95, type=c("norm", "perc"))

vfinx.VaR.boot.0.01
vfinx.0.95.ci.boot.0.01
veurx.VaR.boot.0.01
veurx.0.95.ci.boot.0.01
veiex.VaR.boot.0.01
veiex.0.95.ci.boot.0.01
vbltx.VaR.boot.0.01
vbltx.0.95.ci.boot.0.01
vbisx.VaR.boot.0.01
vbisx.0.95.ci.boot.0.01
vpacx.VaR.boot.0.01
vpacx.0.95.ci.boot.0.01

df_2.1.2 <- data.frame(VaR.normal.01, SE=c(1029, 819, 856, 625, 97.6, 965), 
                       lower_bound=c(-9202, -10183, -11441, -6567, -1056, -9998),
                       upper_bound=c(-5168, -6974, -8084, -4116, -673, -6217))
df_2.1.2

```

```{r}
# 2.2
# Using the monthly mean and standard deviation estimates, compute the annualized mean (12 time monthly mean) and standard deviation (square root of 12 time monthly std dev) and determine the 1% and 5% value-at-risk of the $100,000 investment over a one-year investment horizon. Arrange these results nicely in a table.

Value.at.Risk.annual = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = apply(x, 2, mean)*12 + apply(x, 2, sd)*sqrt(12)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05.annual = Value.at.Risk.annual(ret.mat, p=0.05, method="normal")
VaR.normal.05.annual
VaR.normal.01.annual = Value.at.Risk.annual(ret.mat, p=0.01)
VaR.normal.01.annual

df.2.2 = data.frame(VaR.normal.01.annual, VaR.normal.05.annual)
df.2.2




```


```{r}
# 2.3
# Repeat the VaR analysis (but skip the bootstrapping and the annualized VaR calculation), but this time use the empirical 1% and 5% quantiles of the return distributions (which do not assume a normal distribution - this method is often called historical simulation). How different are the results from those based on the normal distribution?
VaR.empirical.05 = Value.at.Risk(ret.mat, p=0.05, method="empirical")
VaR.empirical.01 = Value.at.Risk(ret.mat, p=0.01, method="empirical")

cbind(VaR.normal.05, VaR.empirical.05)
cbind(VaR.normal.01, VaR.empirical.01)

df.2.3 = data.frame(VaR.empirical.01, VaR.empirical.05, VaR.normal.01, VaR.normal.05)
df.2.3
```


```{r}
# 3.1
# Compute the global minimum variance portfolio and calculate the expected return and SD of this portfolio. Are there any negative weights in the global minimum variance portfolio? 
# Graph the weights of the 6 assets in this portfolio using a bar chart.

gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
summary(gmin.port, risk.free=rf)

gmin.weights.mat = as.matrix(gmin.port$weights)

df.3.1 = data.frame(gmin.weights.mat)
df.3.1
plot(gmin.port)

```

```{r}
# 3.2
# Annualize the monthly mean and SD by multiplying the mean by 12 and the SD by the square root of 12. Compute the annual Sharpe ratio from these values. Briefly comment on these values relative to those for each asset.
options(scipen = 999)

Value.at.Risk.gm.m = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = gmin.port$er + gmin.port$sd*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}

Value.at.Risk.gm.a = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = gmin.port$er*12 + gmin.port$sd*sqrt(12)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}

GM_VaR_0.01_monthly = Value.at.Risk.gm.m(ret.mat, p=0.01,method="normal")
GM_VaR_0.01_annual = Value.at.Risk.gm.a(ret.mat, p=0.01,method="normal")
GM_VaR_0.05_monthly = Value.at.Risk.gm.m(ret.mat, p=0.05,method="normal")
GM_VaR_0.05_annual = Value.at.Risk.gm.a(ret.mat, p=0.05,method="normal")



GM_mu_monthly = gmin.port$er
GM_mu_annual = gmin.port$er*12
GM_var_monthly = (gmin.port$sd)^2
GM_var_annual = (gmin.port$sd*sqrt(12))^2
GM_sd_monthly = gmin.port$sd
GM_sd_annual = gmin.port$sd*sqrt(12)
GM_SharpRatio_monthly = (gmin.port$er - rf)/gmin.port$sd
GM_SharpRatio_annual = (gmin.port$er*12 - rf_annual) /(gmin.port$sd * sqrt(12))
GM_VaR_0.01_monthly = Value.at.Risk.gm.m(ret.mat, p=0.01,method="normal")
GM_VaR_0.01_annual = Value.at.Risk.gm.a(ret.mat, p=0.01,method="normal")
GM_VaR_0.05_monthly = Value.at.Risk.gm.m(ret.mat, p=0.05,method="normal")
GM_VaR_0.05_annual = Value.at.Risk.gm.a(ret.mat, p=0.05,method="normal")

  
df.4.2 <- data.frame(
   Estimates = c('Expected return','Variance','Standard Deviation','Sharp Ratio','VaR 1%', 'VaR 5%'),
   Monthly = c(GM_mu_monthly,GM_var_monthly,GM_sd_monthly,GM_SharpRatio_monthly,GM_VaR_0.01_monthly,GM_VaR_0.05_monthly),
   Annual = c(GM_mu_annual,GM_var_annual,GM_sd_annual,GM_SharpRatio_annual,GM_VaR_0.01_annual,GM_VaR_0.05_annual),
   stringsAsFactors = FALSE
)
df.4.2
```



```{r}
# 3.3
# Assume that you have $100,000 to invest starting on October 30, 2019. For the global minimum variance portfolio, determine the 1% and 5% value-at-risk of the $100,000 investment over a one-month investment horizon. Remember that returns are continuously compounded, so you have to convert the 1% and 5% quantiles to simple returns (see the example in the lecture notes on Introduction to Portfolio Theory). Compare this value to the VaR values for the individual assets.

###
w0 = 100000
mu.p.m = as.numeric(t(gmin.port$weights)*muhat.vals)
sig.p.m = as.numeric(t(gmin.port$weights)%*%cov.mat%*%gmin.port$weights)
VaR.p.m.01 = (exp((mu.p.m+sig.p.m*qnorm(0.01)))-1)*w0
VaR.p.m.05 = (exp((mu.p.m+sig.p.m*qnorm(0.05)))-1)*w0
VaR.p.m.01
VaR.p.m.05

rbind(VaR.normal.05, VaR.p.m.05)
rbind(VaR.normal.01, VaR.p.m.01)


```

```{r}
# 3.4
# Compute the global minimum variance portfolio with the added restriction that short-sales are not allowed, and calculate the expected return and SD of this portfolio.  This is the relevant portfolio for you because you cannot short mutual funds in your 401K account.
# Graph the weights of the 6 assets in this portfolio. 
# Annualize the monthly estimates by multiplying the ER by 12 and the SD by the square root of 12. Compute the annual Sharpe ratio from these values. Compare this portfolio with the global minimum variance portfolio that allows short-sales. 

gmin.port.ns <- globalMin.portfolio(muhat.vals, cov.mat, shorts=FALSE)
summary(gmin.port.ns, risk.free=rf)
plot(gmin.port.ns)
gmin.ns.weights.mat = as.matrix(gmin.port.ns$weights)
gmin.ns.weights.mat

GM_ns_SharpRatio_annual = (gmin.port.ns$er*12 - rf_annual) /(gmin.port.ns$sd * sqrt(12))
rbind(GM_ns_SharpRatio_annual, GM_SharpRatio_annual)

```

```{r}
options(scipen = 999)

Value.at.Risk.gmn.m = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = gmin.port.ns$er + gmin.port.ns$sd*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}

Value.at.Risk.gmn.a = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = gmin.port.ns$er*12 + gmin.port.ns$sd*sqrt(12)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}

GMn_VaR_0.01_monthly = Value.at.Risk.gmn.m(ret.mat, p=0.01,method="normal")
GMn_VaR_0.01_annual = Value.at.Risk.gmn.a(ret.mat, p=0.01,method="normal")
GMn_VaR_0.05_monthly = Value.at.Risk.gmn.m(ret.mat, p=0.05,method="normal")
GMn_VaR_0.05_annual = Value.at.Risk.gmn.a(ret.mat, p=0.05,method="normal")



GMn_mu_monthly = gmin.port.ns$er
GMn_mu_annual = gmin.port.ns$er*12
GMn_var_monthly = (gmin.port.ns$sd)^2
GMn_var_annual = (gmin.port.ns$sd*sqrt(12))^2
GMn_sd_monthly = gmin.port.ns$sd
GMn_sd_annual = gmin.port.ns$sd*sqrt(12)
GMn_SharpRatio_monthly = (gmin.port.ns$er - rf)/gmin.port.ns$sd
GMn_SharpRatio_annual = (gmin.port.ns$er*12 - rf_annual) /(gmin.port.ns$sd * sqrt(12))
GMn_VaR_0.01_monthly = Value.at.Risk.gmn.m(ret.mat, p=0.01,method="normal")
GMn_VaR_0.01_annual = Value.at.Risk.gmn.a(ret.mat, p=0.01,method="normal")
GMn_VaR_0.05_monthly = Value.at.Risk.gmn.m(ret.mat, p=0.05,method="normal")
GMn_VaR_0.05_annual = Value.at.Risk.gmn.a(ret.mat, p=0.05,method="normal")

  
df.3.4 <- data.frame(
   Estimates = c('Expected return','Variance','Standard Deviation','Sharp Ratio','VaR 1%', 'VaR 5%'),
   Monthly = c(GMn_mu_monthly,GMn_var_monthly,GMn_sd_monthly,GMn_SharpRatio_monthly,GMn_VaR_0.01_monthly,GMn_VaR_0.05_monthly),
   Annual = c(GMn_mu_annual,GMn_var_annual,GMn_sd_annual,GMn_SharpRatio_annual,GMn_VaR_0.01_annual,GMn_VaR_0.05_annual),
   stringsAsFactors = FALSE
)
df.3.4
```



```{r}
# 3.5
# Assume that you have $100,000 to invest for a year starting on October 30, 2019. For the global minimum variance portfolio with short-sales not allowed, determine the 1% and 5% value-at-risk of the $100,000 investment over a one-month investment horizon. Compare your results with those for the global minimum variance that allows short sales.

Value.at.Risk.GM.ns = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = gmin.port.ns$er + gmin.port.ns$sd*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05.GM.ns = Value.at.Risk.GM.ns(ret.mat, p=0.05, method="normal")
VaR.normal.05.GM.ns
VaR.normal.01.GM.ns = Value.at.Risk.GM.ns(ret.mat, p=0.01)
VaR.normal.01.GM.ns

rbind(VaR.normal.05.GM.ns, VaR.normal.05.GM)
rbind(VaR.normal.01.GM.ns, VaR.normal.01.GM)

# answer
w0 = 100000
mu.p.m.noShort = as.numeric(t(gmin.port.ns$weights)*muhat.vals)
sig.p.m.noShort = as.numeric(t(gmin.port.ns$weights)%*%cov.mat%*%gmin.port.ns$weights)
VaR.p.m.noShort.01 = (mu.p.m.noShort+sig.p.m.noShort*qnorm(0.01))*w0
VaR.p.m.noShort.05 = (mu.p.m.noShort+sig.p.m.noShort*qnorm(0.05))*w0
VaR.p.m.noShort.01
VaR.p.m.noShort.05
```

```{r}
# 3.6
# Using the estimated means, variances and covariances computed earlier, compute and plot the efficient portfolio frontier, allowing for short sales, for the 6 risky assets using the Markowitz algorithm. That is, compute the Markowitz bullet. Recall, to do this you only need to find two efficient portfolios and then every other efficient portfolio is a convex combination of the two efficient portfolios. Use the global minimum variance portfolio as one efficient portfolio. For the second efficient portfolio, compute the efficient minimum variance portfolio with a target return equal to the maximum of the average returns for the six assets (see examples from lecture notes). 

# Create a plot (based on monthly frequency) with portfolio expected return on the vertical axis and portfolio standard deviation on the horizontal axis showing the efficient portfolios. Indicate the location of the global minimum variance portfolio (with short sales allowed) as well as the locations of your six assets.

ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, alpha.max=1.5, nport=20)
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
abline(h=0, v=0)
text(gmin.port$sd, gmin.port$er, labels="GMIN", pos=1.5, cex = cex.val)



## efficient portfolio with target return equal to max returns
target.return <- max(muhat.vals)
e.port.max<- efficient.portfolio(muhat.vals, cov.mat, target.return)
summary(e.port.max, risk.free=rf)
plot(e.port.max)
```
```{r}
efficientPortfolio = efficient.portfolio(muhat.vals,cov.mat,max(muhat.vals))
xm = seq(-1,2,0.1)
z = xm%*%t(gmin.port$weights)+(1-xm)%*%t(efficientPortfolio$weights)
muhat.bullets = z%*%muhat.vals
sig2.bullets = diag(z%*%cov.mat%*%t(z))
sig.bullets = sqrt(sig2.bullets)
plot(sig.bullets,muhat.bullets,type = 'b')


points(gmin.port$sd, gmin.port$er, col="orange", lwd=3)
text(gmin.port$sd, gmin.port$er, labels="GMIN", pos=4, cex = 1.25)

points(sd.vals,muhat.vals)
text(sd.vals,muhat.vals,labels = colnames(projectReturns.df),pos = 4)


```

```{r}
# 3.7
# Compute the tangency portfolio using a monthly risk-free rate equal to 0.0004167 per month (which corresponds to an annual rate of 0.5%). Recall, we need the risk free rate to be smaller than the average return on the global minimum variance portfolio in order to get a nice graph.
# Graph the weights of the 6 assets in this portfolio. In the tangency portfolio, are any of the weights on the 6 funds negative?
# Compute the expected return, variance and standard deviation of the tangency portfolio. 
# Compare the Sharpe ratio of the tangency portfolio with those of the individual assets.
# Show the tangency portfolio as well as combinations of T-bills and the tangency portfolio on a plot with the Markowitz bullet. That is, compute the efficient portfolios consisting of T-bills and risky assets.
# Annualize the monthly ER and SD of the tangency portfolio by multiplying the ER by 12 and the SD by the square root of 12. Compute the annual Sharpe ratio from these values. Briefly comment.

tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
summary(tan.port, risk.free=rf)
plot(tan.port)
tan.weights.mat = as.matrix(tan.port$weights)
tan_SharpeRatio = (tan.port$er - rf) / tan.port$sd

plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=3)
text(gmin.port$sd, gmin.port$er, labels="GMIN", pos=2.5, cex = 1.25)

points(tan.port$sd, tan.port$er, col="red", lwd=3)
text(tan.port$sd, tan.port$er, labels="tangency", pos=4)

sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)
abline(v=0, h=0)
points(0, rf, col="green", lwd=3)
text(0, rf, labels="rf", pos=4)
```

```{r}
tan_SharpeRatio_annual = (tan.port$er*12 - rf_annual) / (tan.port$sd*sqrt(12))
tan_SharpeRatio_annual
cbind(tan_SharpeRatio, tan_SharpeRatio_annual)
 

  
df.3.7 <- data.frame(
   Estimates = c('Expected return','Variance','Standard Deviation','Sharp Ratio'),
   Monthly = c(tan.port$er,(tan.port$sd)^2,tan.port$sd,tan_SharpeRatio),
   Annual = c(tan.port$er*12,(tan.port$sd*sqrt(12))^2,tan.port$sd*sqrt(12),tan_SharpeRatio_annual),
   stringsAsFactors = FALSE
)
df.3.7

```

```{r}
# 3.8
# Compute and plot the efficient portfolio frontier this time not allowing for short sales, for the 6 risky assets using the Markowitz algorithm. Recall, to do this you need to create a grid of target return values, between the mean of the no short sales global minimum variance portfolio and the mean of the asset with the highest average return, and solve the Markowitz algorithm with the no short sales restriction.   
# Compare the no short sale frontier with the frontier allowing short sales (try to plot them on the same graph)

# Consider a portfolio with a target volatility of 0.02 or 2% per month. What is the approximate cost in the expected return of investing in a no short sale efficient portfolio versus a short sale efficient portfolio?

ef.ns <- efficient.frontier(muhat.vals, cov.mat, alpha.min=0, 
                            alpha.max=1, nport=20, shorts=FALSE)
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(ef.ns$sd, ef.ns$er, type="b", col="red", lwd=2)
abline(h=0, v=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)


```

```{r}
# 3.9
# Using a monthly risk-free rate equal to 0.0004167 per month and the estimated means, variances and covariances compute the tangency portfolio imposing the additional restriction that short-sales are not allowed. 
# Compute the expected return, variance, and standard deviation of the tangency portfolio. 
# Give the value of Sharpe's slope for the no-short sales tangency portfolio. 
# Annualize the monthly ER and SD of the tangency portfolio by multiplying the ER by 12 and the SD by the square root of 12. Compute the annual Sharpe ratio from these values. Briefly comment.

# Compare this tangency portfolio with the tangency portfolio where short-sales are allowed.

tan.port.ns <- tangency.portfolio(muhat.vals, cov.mat, rf, shorts=FALSE)
summary(tan.port.ns, risk.free=rf)
plot(tan.port.ns)
tan.port.ns$er
tan.port.ns$sd
tan.port.ns$sd^2

tan.ns.SRatio = (tan.port.ns$er - rf) / tan.port.ns$sd
tan.ns.SRatio

tan.ns.SRatio.annual = (tan.port.ns$er*12 - rf_annual) / (tan.port.ns$sd*sqrt(12))
tan.ns.SRatio.annual

tan.port.ns$er*12
tan.port.ns$sd*sqrt(12)
(tan.port.ns$er*12 - rf_annual) / (tan.port.ns$sd*sqrt(12))

```
```{r}
df.3.9 <- data.frame(
   Estimates = c('Expected return','Variance','Standard Deviation','Sharp Ratio'),
   Monthly = c(tan.port.ns$er,(tan.port.ns$sd)^2,tan.port.ns$sd,tan.ns.SRatio),
   Annual = c(tan.port.ns$er*12,(tan.port.ns$sd*sqrt(12))^2,tan.port.ns$sd*sqrt(12),tan.ns.SRatio.annual),
   stringsAsFactors = FALSE
)
df.3.9
```


```{r}
# 4.1
e.port.new<- efficient.portfolio(muhat.vals, cov.mat, 0.005, shorts=FALSE)
summary(e.port.new)
barplot(e.port.new$weights)
```

```{r}
# 4.2
Value.at.Risk.new.m = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = e.port.new$er + e.port.new$sd*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05.new.m = Value.at.Risk.new.m(ret.mat, p=0.05, method="normal")
VaR.normal.01.new.m = Value.at.Risk.new.m(ret.mat, p=0.01)


Value.at.Risk.new.a = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
	method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
	  q = e.port.new$er*12 + e.port.new$sd*sqrt(12)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
	VaR = (exp(q) - 1)*w
	VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05.new.m.a = Value.at.Risk.new.a(ret.mat, p=0.05, method="normal")
VaR.normal.01.new.m.a = Value.at.Risk.new.a(ret.mat, p=0.01)







a = (e.port.new$er - rf)/e.port.new$sd
b = (e.port.new$er*12 - rf_annual) /(e.port.new$sd * sqrt(12))


  
df.finally <- data.frame(
   Estimates = c('Expected return','Variance','Standard Deviation','Sharp Ratio','VaR 1%', 'VaR 5%'),
   Monthly = c(e.port.new$er,(e.port.new$sd)^2,e.port.new$sd,a,VaR.normal.01.new.m,VaR.normal.05.new.m),
   Annual = c(e.port.new$er*12,(e.port.new$sd*sqrt(12))^2,e.port.new$sd*sqrt(12),b,VaR.normal.01.new.m.a,VaR.normal.05.new.m.a),
   stringsAsFactors = FALSE
)
df.finally



```
```{r}
e.port.new<- efficient.portfolio(muhat.vals, cov.mat, 0.005, shorts=FALSE)
plot(e.port.new, plot.assets=TRUE, col="blue", lwd=2)
points(e.port.new$sd, e.port.new$er, type="b", col="red", lwd=2)
abline(h=0, v=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)
# done
```

