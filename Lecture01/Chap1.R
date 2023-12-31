


#'
#' title: "Cap 1 Introduction to the Forecasting"
#' author: 
#'  - "Pedro Valls"
#' date: "14 de outubro de 2020"
#' ---
#+ warning = FALSE, message = FALSE



rm(list = ls())
setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2020/applied_economics_forecasting/appliedeconomicforecasting_R/cap1")


#'
#' ## Instala pacotes 
#' 
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package("urca")
load_package("vars")
load_package("tsDyn")
load_package(readxl)
load_package(vars)
load_package(tseries)
load_package(tsDyn)
load_package(scatterplot3d)
load_package(stargazer)
load_package(readxl)
load_package(data.table)
load_package(ggplot2)
load_package(xtable)



library(readxl)
library(urca)
library(vars)
library(data.table)
library(ggplot2)
library(xtable)

#'
#' ## 1.11.1 Data Simulation Procedure
#' 
sim.data <- list()
sim.data$full <- fread('Lecture01/simulated_data.csv')
#'
#' ## Estimation sample
#' 
sim.data$est <- sim.data$full[102:301]
#'
#'## forecasting sample
#'

# sim.data$fore <- sim.data$full[302:401] 
sim.data$fore <- sim.data$full[302:501] 
#'
#' ## Y, X, and scatter plot
#' 
ggplot(sim.data$est, aes(x = 102:301, y = y)) + geom_line() +
  theme_bw() + xlab('') + ylab('') + ggtitle('Y')
ggplot(sim.data$est, aes(x = 102:301, y = x)) + geom_line() +
  theme_bw() + xlab('') + ylab('') + ggtitle('X')
ggplot(sim.data$est, aes(x = x, y = y)) + geom_point() +
  theme_bw() + xlab('X') + ylab('Y')

#'
#' ## OLS for estimation period
#' 
ols.fit <- lm(y ~ x, data = sim.data$est)
print(xtable(ols.fit), floating = F) # LaTeX output

#'
#' ## Forecasts
#' 
yhat <- list()
yhat$y <- predict(ols.fit, newdata = sim.data$fore)
yhat$se <- sqrt(sum(ols.fit$residuals^2) / 198)
yhat$y.up <- yhat$y + 1.96 * yhat$se
yhat$y.low <- yhat$y - 1.96 * yhat$se


ts_yhat=ts(yhat$y)
ts_yhat_up=ts(yhat$y.up)
ts_yhat_low=ts(yhat$y.low)
ts_y=ts(sim.data$fore)


#'
#' ## Plot the series together
#' 
ts.plot(ts_y,ts_yhat,ts_yhat_up,ts_yhat_low,
        gpars = list(col = c("blue", "black", "red", "green")))
legend("bottom", inset=.01, c("Y","Yhat","Yhat_UP","Yhat_LOW" ), fill=c("blue", "black", "red", "green"), horiz=T)





#'
#' ## Recursive
#' 
#' 
#' 
yhat$y.rec <- yhat$y.recse <- yhat$rec.se <- yhat$y.rec.up <- yhat$y.rec.low <-rep(0, 100)
for (i in 1:200) {
  ols.rec <- lm(y ~ x, data = sim.data$full[102:(300 + i)])
  yhat$y.rec[i] <- predict(ols.rec, newdata = sim.data$full[301 + i])
  yhat$y.recse[i] <- sqrt(sum(ols.rec$residuals^2) / (197 + i))
  yhat$y.rec.up[i] <- yhat$y.rec[i]+1.96*yhat$y.recse[i]
  yhat$y.rec.low[i] <- yhat$y.rec[i]-1.96*yhat$y.recse[i]
  
}








#'
#' ## Plot - actual & recursive forecasts
#' 

ts_fore=ts(sim.data$fore[, y])
ts_rec=ts(yhat$y.rec)
ts_rec_low=ts(yhat$y.rec.low)
ts_rec_up=ts(yhat$y.rec.up)



#'
#' ## Plot the series together
#' 
ts.plot(ts_fore,ts_rec,ts_rec_up,ts_rec_low,
        gpars = list(col = c("blue", "black", "red", "green")))
legend("bottom", inset=.01, c("Y","YREC","YREC_UP","YREC_LOW" ), fill=c("blue", "black", "red", "green"), horiz=T)


#'
#' ## RMSE & MAE
#' 
yhat$Y <- cbind(yhat$y, yhat$y.rec)
RMSE <- sqrt(colSums((yhat$Y - sim.data$fore[, y])^2) / 200)
MAE <- colSums(abs(yhat$Y - sim.data$fore[, y])) / 200
error.mat <- rbind(RMSE, MAE)
colnames(error.mat) <- c('Simple', 'Recursive')
error.mat
#'
#' ## 1.12.1 Forecasting Euro Area GDP 
#' 
#' 
eu.gdp <- list()
eu.gdp$full <- fread('ex2_regress_gdp.csv')
eu.gdp$full[, date := as.Date(date, format = '%m/%d/%Y')]

#'
#' ##  Plots
#'  
plot.label <- c('96', '97', '98', '99', '00', '01', '02', '03', '04',
                '05', '06', '07', '08', '09', '10', '11', '12', '13')

for (var in c('y', 'ipr', 'sr', 'su', 'pr')) {
  plot(eu.gdp$full[[var]], type = 'n', main = toupper(var),
       xlab = '', ylab = '', xaxt = 'n')
  axis(1, at = c(0:17) * 4 + 2, labels = plot.label)
  lines(eu.gdp$full[[var]], type = 'l')
}

#'
#'## Full sample - 1996Q1 to 2013Q2
#'
gdp.fit <- list()
gdp.formula <- c('y ~ ipr + su + pr + sr', 'y ~ ipr + su + sr',
                 'y ~ ipr + su', 'y ~ ipr + pr + sr')

for (model in 1:4) {
  gdp.fit[[model]] <- lm(gdp.formula[model], data = eu.gdp$full)
  summary(gdp.fit[[model]])
}



#'
#' ## Estimation sample - 1996Q1 to 2006Q4
#' 
eu.gdp$est <- eu.gdp$full[1:44]
eu.gdp$fore <- eu.gdp$full[45:70]

gdp.est <- list()
for (model in 1:4) {
  gdp.est[[model]] <- lm(gdp.formula[model], data = eu.gdp$est)
  summary(gdp.est[[model]])
}


#yhat$se <- sqrt(sum(ols.fit$residuals^2) / 198)
#'
#' ## Static and recursive forecasts
#' 
gdp.fore <- list()
gdp.rec <- list()
#gdp.rec.se <- list()
#gdp.rec.up <- list()
#gdp.rec.low <- list()

for (model in 1:4) {
  gdp.fore[[model]] <- predict(gdp.est[[model]], newdata = eu.gdp$fore)
  
  gdp.rec[[model]] <- rep(0, 26)
  for (i in 1:26) {
    ols.rec <- lm(gdp.formula[model], data = eu.gdp$full[1:(43 + i)])
    gdp.rec[[model]][i] <- predict(ols.rec, newdata = eu.gdp$full[44 + i])
  }
}

#'
#' ## Plots - actual & forecasts
#' 
plot.label <- 2007:2013
for (model in 1:4) {
  gdp.plot <- cbind(data.table(eu.gdp$fore[, y]),
                    data.table(gdp.rec[[model]]),
                    data.table(gdp.fore[[model]]))
  setnames(gdp.plot, c('Y', paste0('YRFOREG', model),
                       paste0('YFOREG', model)))
  
  plot(gdp.plot[[1]], type = 'n', xlab = '', ylab = '',
       xaxt = 'n', ylim = c(-3, 2))
  axis(1, at = c(0:6) * 4 + 2, labels = plot.label)
  for (l in 1:3) {
    lines(gdp.plot[[l]], lty = l)
  }
  legend('bottomright', legend = colnames(gdp.plot), inset = 0.05, lty = 1:3)
}

#'
#' ## RMSE & MAE
#' 
gdp.rec$Y <- cbind(gdp.rec[[1]], gdp.rec[[2]], gdp.rec[[3]], gdp.rec[[4]])
RMSE <- sqrt(colSums((gdp.rec$Y - eu.gdp$fore[, y])^2) / 26)
MAE <- colSums(abs(gdp.rec$Y - eu.gdp$fore[, y])) / 26
error.mat <- rbind(RMSE, MAE)
colnames(error.mat) <- c('Model 1', 'Model 2', 'Model 3', 'Model 4')
error.mat


#'
#' ## 1.12.2 Forecating US GDP
#' 
us.gdp <- list()
us.gdp$full <- fread('ex2_regress_gdp_us.csv')
us.gdp$full[, date := as.Date(date, format = '%m/%d/%Y')]

#'
#' ## Plots 
#' 
plot.label <- c('84', '86', '88', '90', '92', '94', '96', '98',
                '00', '02', '04', '06', '08', '10', '12')
for (var in c('y', 'ipr', 'sr', 'su', 'pr')) {
  plot(us.gdp$full[[var]], type = 'n', main = toupper(var),
       xlab = '', ylab = '', xaxt = 'n')
  axis(1, at = c(1:15) * 8, labels = plot.label)
  lines(us.gdp$full[[var]], type = 'l')
}

#'
#' ## Full sample - 1983Q1 to 2013Q2
#' 
gdp.fit <- list()
gdp.formula <- c('y ~ ipr + su + pr + sr', 'y ~ ipr + su + sr',
                 'y ~ ipr + su', 'y ~ ipr + pr + sr')

#'
#' ##Summary
#' 
k <- c(5, 4, 3, 4)
n <- nrow(us.gdp$full)
summary.stat <- data.table('R2' = rep(0, 4))
for (model in 1:4) {
  gdp.fit[[model]] <- lm(gdp.formula[model], data = us.gdp$full)
  summary(gdp.fit[[model]])
  

# R2 / adjusted R2 / AIC / BIC / HQ
 
  summary.stat[model, c('R2', 'Adj R2', 'AIC', 'SC') :=
                 list(summary(gdp.fit[[model]])$r.squared,
                      summary(gdp.fit[[model]])$adj.r.squared,
                      AIC(gdp.fit[[model]]), BIC(gdp.fit[[model]]))]
}
summary.stat[, HQ := AIC + 2 * k * (log(log(n)) - 1)]
summary.stat <- t(summary.stat)
colnames(summary.stat) <- c('Model 1', 'Model 2', 'Model 3', 'Model 4')
summary.stat


#'
#' ## Estimation sample - 1983Q1 to 2006Q4
#' 
us.gdp$est <- us.gdp$full[1:96]
us.gdp$fore <- us.gdp$full[97:122]

gdp.est <- list()
for (model in 1:4) {
  gdp.est[[model]] <- lm(gdp.formula[model], data = us.gdp$est)
  summary(gdp.est[[model]])
}

#'
#' ## Static and recursive forecasts
#' 
gdp.fore <- list()
gdp.rec <- list()
for (model in 1:4) {
  gdp.fore[[model]] <- predict(gdp.est[[model]], newdata = us.gdp$fore)
  
  gdp.rec[[model]] <- rep(0, 26)
  for (i in 1:26) {
    ols.rec <- lm(gdp.formula[model], data = us.gdp$full[1:(95 + i)])
    gdp.rec[[model]][i] <- predict(ols.rec, newdata = us.gdp$full[96 + i])
  }
}

#'
#' ## Plots - actual & forecasts
#' 
plot.label <- 2007:2013
for (m in 1:4) {
  gdp.plot <- cbind(data.table(us.gdp$fore[, y]),
                    data.table(gdp.rec[[m]]),
                    data.table(gdp.fore[[m]]))
  setnames(gdp.plot, c('Y', paste0('YRFOREG', m), paste0('YFOREG', m)))

  plot(gdp.plot[[1]], type = 'n', xlab = '', ylab = '',
       xaxt = 'n', ylim = c(-3, 2))
  axis(1, at = c(0:6) * 4 + 2, labels = plot.label)
  lines(gdp.plot[[1]], lty = 1)
  lines(gdp.plot[[3]], lty = 2)
  legend('bottomright', legend = c('Y', paste0('YFOREG', m)),
         inset = 0.05, lty = 1:2)
  
 
  plot(gdp.plot[[1]], type = 'n', xlab = '', ylab = '',
       xaxt = 'n', ylim = c(-3, 2))
  axis(1, at = c(0:6) * 4 + 2, labels = plot.label)
  for (l in 1:3) {
    lines(gdp.plot[[l]], lty = l)
  }
  legend('bottomright', legend = colnames(gdp.plot), inset = 0.05, lty = 1:3)
}

RMSE <- list()
MAE <- list()

gdp.fore$Y <- cbind(gdp.fore[[1]], gdp.fore[[2]],
                    gdp.fore[[3]], gdp.fore[[4]])
gdp.rec$Y <- cbind(gdp.rec[[1]], gdp.rec[[2]],
                   gdp.rec[[3]], gdp.rec[[4]])

RMSE <- rbind(sqrt(colSums((gdp.fore$Y - us.gdp$fore[, y])^2)),
              sqrt(colSums((gdp.rec$Y - us.gdp$fore[, y])^2))) / sqrt(26)
MAE <- rbind(colSums(abs(gdp.fore$Y - us.gdp$fore[, y])),
             colSums(abs(gdp.rec$Y - us.gdp$fore[, y]))) / 26
error.mat <- rbind(RMSE[1, ], MAE[1, ], RMSE[2, ], MAE[2, ])
rownames(error.mat) <- c('RMSE', 'MAE', 'RMSE recursive', 'MAE recursive')
colnames(error.mat) <- c('Model 1', 'Model 2', 'Model 3', 'Model 4')
error.mat






#'
#' ## 1.13.1 Forecasting default risk
#' 
yield <- fread('ex3_regress_oas.csv')
yield[, Date := as.Date(Date, format = '%m/%d/%Y')]

#'
#' ## Plots
#' 
plot.label <- c('98', '99', '00', '01', '02', '03', '04', '05', '06',
                '07', '08', '09', '10', '11', '12', '13', '14', '15')
for (var in c('OAS', 'VIX', 'SENT', 'sp500', 'PMI')) {
  plot(yield[[var]], type = 'n', main = var,
       xlab = '', ylab = '', xaxt = 'n')
  axis(1, at = c(1:18) * 12, labels = plot.label, cex.axis = 0.6)
  lines(yield[[var]], type = 'l')
}

#'
#' ## Regressions - lagged regressors
#' 
n.months <- nrow(yield)
oas <- yield[2:n.months, OAS]
yield <- yield[1:n.months - 1]
yield[, OAS := oas]

yield.fit <- list()
yield.formula <- c('OAS ~ VIX', 'OAS ~ SENT', 'OAS ~ PMI',
                   'OAS ~ sp500', 'OAS ~ VIX + SENT + PMI + sp500')
for (model in 1:5) {
  yield.fit[[model]] <- lm(yield.formula[model], data = yield)
  summary(yield.fit[[model]])
}