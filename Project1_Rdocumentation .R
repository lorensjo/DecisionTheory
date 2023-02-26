#file = "C:\Users\loren\OneDrive\Documents\Uni\DecisionTheory\Project1\co2values.txt"

### DATA reading
file = "co2values.txt"
d = read.table(file, header=FALSE)

for (i in seq(1,743)){
  if (d[i,4] == -99.99){
    d[i,4] = NA
  }
}
y=d[,4]
x=d[,3]

### PLotting of historical data
plot(x, y, cex=0.4, 
     main=expression('Monthly averages of atmospheric CO'[2]), 
     xlab='Year', 
     ylab=expression('CO'[2]*'-value (ppm)'))



#################################################################################
### Forecasting by using 80/20 rule
#################################################################################
val80 = round(0.8*743,0)
val801 = val80 + 1
x80 = x[1:val80]
y80 = y[1:val80]
x20 = x[val801:743]
y20 = y[val801:743]
  
## Plotting
plot(x80, y80, cex=0.4, 
     main=expression('Monthly averages of atmospheric CO'[2]), 
     xlab='Year', 
     ylab=expression('CO'[2]*'-value (ppm)'),
     ylim = c(315,415),
     xlim = c(1958,2020))
lines(x20,y20, col="red")

## Polynomial model
x80_2 = x80^2
x80_3 = x80^3
linearmodel = lm(y80~poly(x80,3,raw=TRUE))
a0 = linearmodel$coefficients[1]
a1 = linearmodel$coefficients[2]
a2 = linearmodel$coefficients[3]
a3 = linearmodel$coefficients[4]
lines(x, a0+a1*x+a2*x^2+a3*x^3, col="blue")

## Logarithmic model
log80lm = lm(log(y80)~x80+x80_2)
a = log80lm$coefficients[1]
b = log80lm$coefficients[2]
c = log80lm$coefficients[3]
lines(x,exp(a+b*x+c*x^2), col="blue")
lines(x,y,col="red",lwd=0.5)

pred = exp( log80lm$coefficients[1])*exp( log80lm$coefficients[2]*x20)*exp( log80lm$coefficients[3]*x20^2)
lines(x20, pred, col="red")






#################################################################################
## Fitting and Forecasting by means of times series
#################################################################################
# reading data and converting to time series
data = ts(y80,start = c(1958,3),frequency = 12)
plot(log(data),
     xlab='Year',
     ylab = expression('Log CO'[2]*'-value (ppm)'),
     main = expression("Logarithm of monthly averages of atmospheric CO2"))
# now plotting diffs
plot(diff(data),
     xlab = "Year",
     ylab = expression('Transformed CO'[2]*'-value (ppm)'),
     main = expression("Transformed monthly averages of atmospheric CO2"),
     type="line")




## ARIMA with seasonality
#################################################################################
library(forecast)
ARIMAfit = auto.arima(data, approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
fvalues = forecast(ARIMAfit,23*12)
plot(fvalues,
     xlab = "Year",
     ylab = expression('Log CO'[2]*'-value (ppm)'))
lines(x,y,col="red",lwd=0.5)
## Zoomed in
plot(fvalues,
     xlab = "Year",
     ylab = expression('Log CO'[2]*'-value (ppm)'),
     xlim = c(2005,2030),
     ylim = c(380,440))
lines(x,y,col="red",lwd=0.5)

## Simple Exponential Smoothing
#################################################################################
SESfit = hw(data, h=length(data))
summary(SESfit)
SES_fvalues = forecast(SESfit, length(data))
plot(SES_fvalues,
     main = "SES forecast (additive seasonality)",
     xlab = "Year",
     ylab = expression('CO'[2]*'-value (ppm)'),
     xlim = c(1958,2030),
     ylim = c(315,440))
lines(x,y,col="red",lwd=0.5)
## Zoomed in 
plot(SES_fvalues,
     main = "SES forecast in period 2007-2030",
     xlab = "Year",
     ylab = expression('CO'[2]*'-value (ppm)'),
     xlim = c(2005,2030),
     ylim = c(380,440))
lines(x,y,col="red",lwd=0.5)

#################################################################################
# Determining best model by means of MAD (median absolute deviation) and MSE
#################################################################################
# we calculate the mad as the median of the difference between actual co2 values and forecast co2 values
# this we do only in x20,y20, since this is the validation/test data.

# Polynomial model
POLYexpected = a0+a1*x20+a2*x20^2
POLYdeviation = abs(POLYexpected - y20)
POLY_MAD = median(POLYdeviation)
POLY_MSE = mean( (POLYexpected - y20)^2 )

# Logarithmic model
LOGexpected = exp(a+b*x20+c*(x20)^2)
LOGdeviation = abs(LOGexpected - y20)
LOG_MAD = median(LOGdeviation)
LOG_MSE = mean( (LOGexpected - y20)^2 )


# ARIMA model
ARIMAexpected = forecast(ARIMAfit,12*12+5)[4]$mean
# determine deviation = abs difference between  forecast and real co2 value
ARIMAdeviation = abs(ARIMAexpected - y20)
ARIMA_MAD = median(ARIMAdeviation)
ARIMA_MSE = mean( (ARIMAexpected - y20)^2 )

# SES model
SESexpected = forecast(SESfit, 12*12+5)[2]$mean[1:149]
SESdeviation = abs(SESexpected - y20)
SES_MAD = median(SESdeviation)
SES_MSE = mean( (SESexpected - y20)^2 )





# convert SES to table
v = SES_fvalues
expect = v[2]$mean
upper80 = v[5]$upper[,1]
upper95 = v[5]$upper[,2]
lower80 = v[6]$lower[,1]
lower95 = v[6]$lower[,2]

SD80 = (abs(expect - lower80 ) + abs(expect - upper80) ) /2
SD95 = (abs(expect - lower95 ) + abs(expect - upper95) ) /2

table = data.frame(expect, SD80, SD95)
library(xtable  )
xtable(table)
