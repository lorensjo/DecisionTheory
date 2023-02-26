## Reading file
d = read.table('co2values.txt', header=FALSE)

for (i in seq(1,743)){
  if (d[i,4] == -99.99){
    d[i,4] = NA
  }
}

## Polynomial regression
xas= seq(1958,2020,1/12)
y=d[,4]
x=d[,3]

plot(x, y, cex=0.4, xlim=c(1958,2030), ylim=c(310,450), main=expression('Monthly averages of atmospheric CO'[2]*' - simple regression'), xlab='Date', ylab=expression('CO'[2]*'-value (ppm)'))

firstlm=lm(y~x)
abline(firstlm$coefficients)

x2=x^2
secondlm=lm(y~x+x2)
cf = secondlm$coefficients
lines(xas,cf[1]+xas*cf[2]+xas^2*cf[3], col='blue')

x3=x^3
thirdlm=lm(y~x+x2+x3)
cf = thirdlm$coefficients
lines(xas,cf[1]+xas*cf[2]+xas^2*cf[3]+xas^3*cf[4], col='red')


## Only using first 80% of the data
y80=d[1:round(0.8*743,digits=0),4]
x80=d[1:round(0.8*743,digits=0),3]
plot(x,y, cex=0.4, main=expression('Monthly averages of atmospheric CO'[2]), xlab='Date', ylab=expression('CO'[2]*'-value (ppm)'))
lm80_1=lm(y80~x80)
abline(lm80_1$coefficients)

xas80=seq(from=1958.208, to=2007.625, by=1/12)
x80_2=x80^2
lm80_2=lm(y80~x80+x80_2)
cf = lm80_2$coefficients
lines(xas80, cf[1]+xas80*cf[2]+xas80^2*cf[3], col='red', lwd=1)

xas20=seq(2007.708, 2020.708, 1/24)
lines(xas20, cf[1]+xas20*cf[2]+xas20^2*cf[3],col='blue')
xaspred = seq(2020.708, 2030, 1/12)
lines(xaspred, cf[1]+xaspred*cf[2]+xaspred^2*cf[3],col='blue')


## Logarithmic regression
log80lm = lm(log(y80)~x80+x80_2)
cf = log80lm$coefficients
lines(x80,exp(cf[1]+cf[2]*x80+cf[3]*x80^2), col="red")
lines(xas20, exp(cf[1]+cf[2]*xas20+cf[3]*xas20^2), col='blue')
lines(xaspred, exp(cf[1]+xaspred*cf[2]+xaspred^2*cf[3]), col='blue')

