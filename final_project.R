# Data for the project are downloaded automatically from Yahoo! and consist of
# closing price data on 6 Vanguard mutual funds:
#
# 1. S&P 500 index (vfinx)
# 2. European stock index (veurx)
# 3. Emerging markets fund (veiex)
# 4. Long term bond index (vbltx)
# 5. Short term bond index (vbisx)
# 6. Pacific stock index (vpacx)

options(digits=3, width=70)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(boot)
library(corrplot)

savePath="D:\\Dropbox\\UW Seattle Courses\\Econ 424\\summer2022\\Project\\"

asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")
start.date = "2014-10-01"
end.date = "2019-10-30"

vfinx.prices = get.hist.quote(instrument="vfinx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")    
veurx.prices = get.hist.quote(instrument="veurx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
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
# change time indices to class yearmon, which is most appropriate for monthly data
index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbltx.prices) = as.yearmon(index(vbltx.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names
# create data.frame for downloading
projectPrices.df = coredata(projectPrices.z)
rownames(projectPrices.df) = as.character(index(projectPrices.z))

#
# compute cc and simple returns
#

projectReturns.z = diff(log(projectPrices.z))   
projectReturnsSimple.z = exp(projectReturns.z) - 1
# create data.frame for downloading
projectReturns.df = as.data.frame(coredata(projectReturns.z))
rownames(projectReturns.df) = as.character(index(projectReturns.z))
projectReturnsSimple.df = as.data.frame(coredata(projectReturnsSimple.z))
rownames(projectReturnsSimple.df) = as.character(index(projectReturnsSimple.z))


#
# plot data
#
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

#
# Create matrix of return data and compute pairwise scatterplots
#

ret.mat = coredata(projectReturns.z)
pairs(ret.mat, col="blue")


# show a 4 panel plot for vfinx returns - notice the use of drop=FALSE.
# This preserves the column name. fourPanelPlot() is in IntroCompFinR
# Note: may need to expand graphics window in Rstudio to see legend
fourPanelPlot(projectReturns.z[, "vfinx", drop=FALSE])

#
# boxplots of returns
#

boxplot(ret.mat, main="Vanguard Returns", col="cornflowerblue")

#
# compute descriptive statistics
#

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
# empirical quantiles for VaR calculations
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
# print statistics
stats.mat

# compute standard errors and confidence intervals (do it yourself)


#
# annualize monthly statistics using square-root-of-time rule
#
12*muhat.vals
sqrt(12)*sd.vals

#
# plot return-risk tradeoff and compute Sharpe ratios
#
## risk free rate
rf = 0.005/12

plot(sd.vals, muhat.vals, xlim=c(0, 0.06), ylim=c(0, 0.013),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=2, pch=16, col="cornflowerblue")
text(sd.vals, muhat.vals, labels=colnames(projectReturns.z),
     pos=3)

SharpeRatios = (muhat.vals - rf)/sd.vals

# compute annualized Sharpe ratios (do it yourself)

# compute bootstrap standard error and 95% ci for sharpe ratio
# function to bootstrap VaR
sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}

sharpe.vfinx.boot = boot(ret.mat[, "vfinx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vfinx.boot
boot.ci(sharpe.vfinx.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.vfinx.boot)

#
# visualize correlations - see help on corrplot() function
#
pairs(ret.mat, col="blue")
corrplot(cor.mat, method="ellipse")
# note: if using Rstudio, press the Clear All button on the graph pane. The corrplot() function
# screws up the graphics device in Rstudio

#
# write covariance matrix, expected returns, sd values and quantiles to files
# for importing into Excel
#

write.csv(projectPrices.df, file=paste(savePath, "projectPrices.csv", sep=""))
write.csv(projectReturns.df, file=paste(savePath, "projectReturns.csv", sep=""))
write.csv(projectReturnsSimple.df, file=paste(savePath, "projectReturns.csv", sep=""))
write.csv(cov.mat, file=paste(savePath, "covmat.csv", sep=""))
write.csv(muhat.vals, file=paste(savePath, "muhat.csv", sep=""))
write.csv(sd.vals, file=paste(savePath, "sd.csv", sep=""))
write.csv(t(q.vals), file=paste(savePath, "q.csv", sep=""))


#
# VaR analysis
#

# function to compute normal and empirical VaR for a matrix of returns

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

# empirical VaR
VaR.empirical.05 = Value.at.Risk(ret.mat, p=0.05, method="empirical")
VaR.empirical.05
VaR.empirical.01 = Value.at.Risk(ret.mat, p=0.01, method="empirical")
VaR.empirical.01

# write a function to compute the annual normal VaR

# function to bootstrap VaR
ValueAtRisk.boot = function(x, idx, p=0.05, w=100000) {

	q = mean(x[idx]) + sd(x[idx])*qnorm(p)
	VaR = (exp(q) - 1)*w
	VaR
}

# bootstrap vfinx
VaR.05.boot.vfinx = boot(ret.mat[, "vfinx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vfinx
boot.ci(VaR.05.boot.vfinx, conf = 0.95, type = c("norm","perc"))
plot(VaR.05.boot.vfinx)

#
# rolling analysis
#

# rolling means and sd values
# rolling means and sd values
# compute rolling means over 24 month windows

# vfinx
roll.muhat = rollapply(projectReturns.z[,"vfinx",drop=FALSE], width=24,
                       FUN=mean, align="right")
roll.sigmahat = rollapply(projectReturns.z[,"vfinx",drop=FALSE], width=24,
                          FUN=sd, align="right")

# plot rolling estimates with data
plot(merge(roll.muhat, roll.sigmahat, projectReturns.z[,"vfinx",drop=FALSE]), 
     plot.type="single",
     main="24 month rolling estimates: Vfinx",ylab="returns",
     lwd=2, col=c("blue","orange", "black"))
abline(h=0)
legend(x="bottomright",legend=c("Rolling mean","Rolling sd", "Monthly returns"),
       lwd=2, col=c("blue","orange","black"))

# rolling correlations
rhohat = function(x) {
  cor(x)[1,2]
}

# do the rest yourself

#
# Portfolio theory questions. Portfolio functions are in IntroCompFinR
#

## compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=rf)
plot(gmin.port)

gmin.weights.mat = as.matrix(gmin.port$weights)

## compute global minimum variance portfolio with no short sales
gmin.port.ns <- globalMin.portfolio(muhat.vals, cov.mat, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=rf)
plot(gmin.port.ns)

gmin.ns.weights.mat = as.matrix(gmin.port.ns$weights)

## efficient portfolio with target return equal to max returns
target.return <- max(muhat.vals)
e.port.max<- efficient.portfolio(muhat.vals, cov.mat, target.return)
e.port.max
summary(e.port.max, risk.free=rf)
plot(e.port.max)


## compute tangency portfolio
tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
tan.port
summary(tan.port, risk.free=rf)
plot(tan.port)

tan.weights.mat = as.matrix(tan.port$weights)

## compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(muhat.vals, cov.mat, rf, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=rf)
plot(tan.port.ns)

tan.ns.weights.mat = as.matrix(tan.port.ns$weights)

# plot portfolio weights
par(mfrow=c(2,2))
plot(gmin.port)
plot(e.port.max)
plot(tan.port)
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(gmin.port.ns)
plot(tan.port.ns)
par(mfrow=c(1,1))


## compute efficient frontier
ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, 
                         alpha.max=1.5, nport=20)

## plot efficient frontier allowing shortsales
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port$sd, tan.port$er, col="red", lwd=2)
text(tan.port$sd, tan.port$er, labels="tangency", pos=4)
sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)
abline(v=0, h=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)

## compute efficient frontier not allowing short sales
ef.ns <- efficient.frontier(muhat.vals, cov.mat, alpha.min=0, 
                            alpha.max=1, nport=20, shorts=FALSE)

# show short sale and no short sale frontiers together
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(ef.ns$sd, ef.ns$er, type="b", col="red", lwd=2)
abline(h=0, v=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)
