## Importing Data
realised = c(25, 0.0011, 214, 10838, 1)

q1 = matrix(NA, 3,5)
q1[1,] = c(1,15,10,8,10)
q1[2,] = c(6,25,15,13,40)
q1[3,] = c(20,30,30,15,60)

q2 = matrix(NA, 3,5)
q2[1,] = c(0.0001,0.01,0.01,0.03,0.002)
q2[2,] = c(0.001,0.03,0.02,0.1,0.02)
q2[3,] = c(0.002,0.1,0.05,0.2,0.05)

q3 = matrix(NA, 3,5)
q3[1,] = c(5,5,4,250,5)
q3[2,] = c(150,12,10,360, 80)
q3[3,] = c(900,32,100,500,150)

q4 = matrix(NA, 3,5)
q4[1,] = c(1000,5000,3000,15000,30000)
q4[2,] = c(30000,10000,6000,20000,60000)
q4[3,] = c(65000,50000,7000,30000,200000)

q5 = matrix(NA, 3,5)
q5[1,] = c(5,30,20,40,5)
q5[2,] = c(65,70,30,60,13)
q5[3,] = c(95,90,50,70,30)

q6 = matrix(NA, 3, 5)
q6[1,] = c(1,1,2,1,0.1)
q6[2,] = c(7,3,3,2,0.6)
q6[3,] = c(20,5,5,4,0.95)

## Intrinsic range
L = numeric(6)
U = numeric(6)
for (i in 1:6){
  if (i == 1){q = q1}
  else if (i == 2){q = q2}
  else if (i == 3){q = q3}
  else if (i == 4){q = q4}
  else if (i == 5){q = q5}
  else {q = q6}
  L[i] = min(q[1,])
  U[i] = max(q[3,])
  Li[i] = max(L[i] - 0.1*(U[i]-L[i]), 0)
  Ui[i] = U[i] + 0.1*(U[i]-L[i])
}

## Distributions of questions
quantiles = c(0,0.05,0.5,0.95,1)
cdfexps = list()
for (j in 1:6){ # Question
  xas = seq(Li[j], Ui[j], by=(Ui[j]-Li[j])/10000)
  if (j == 1){q = q1}
  else if (j == 2){q = q2}
  else if (j == 3){q = q3}
  else if (j == 4){q = q4}
  else if (j == 5){q = q5}
  else {q = q6}
  cdfq = list()
  for (i in 1:5){ # Expert
    points = c(Li[j], q[,i], Ui[j])
    cdfexp = function(x){
      return(approx(points, quantiles,xout=x))
    }
    cdfq = append(x=cdfq, values=cdfexp(xas))
  }
  cdfexps = append(x=cdfexps, values=cdfq)
}

## Determine emperical probability vector
empirical = matrix(NA,4,5)
m = 5

for (expert in 1:5) {
  n = rep(1, 5)
  while (n[1] < 4 && realised[1] > q1[n[1],expert] ) {
    n[1] = n[1] + 1}
  while (n[2] < 4 && realised[2] > q2[n[2],expert]) {
    n[2] = n[2] + 1}
  while (n[3] < 4 && realised[3] > q3[n[3],expert]) {
    n[3] = n[3] + 1}
  while (n[4] < 4 && realised[4] > q4[n[4],expert]) {
    n[4] = n[4] + 1}
  while (n[5] < 4 && realised[5] > q5[n[5],expert] ) {
    n[5] = n[5] + 1}
  s1 = length(which(n==1))/m
  s2 = length(which(n==2))/m
  s3 = length(which(n==3))/m
  s4 = length(which(n==4))/m
  empirical[,expert] = c(s1,s2,s3,s4)
}

# Determine Calibaration scores of experts
p = c(0.05, 0.45, 0.45, 0.05)
I = rep(NA,5)
for (expert in 1:5) {
  logvec = rep(NA,4)
  for (i in 1:4) {
    if (empirical[i,expert] == 0) {
      logvec[i] = 0}
    else {
      logvec[i] = log(empirical[i,expert]/p[i]) }
  }
  I[expert] = t(logvec) %*% empirical[,expert]
}

Cal = rep(NA, 5)
for (expert in 1:5) {
  Cal[expert] = 1-pchisq(2*m*I[expert],df=3)
}

## Determine information score experts
Info = matrix(numeric(25), byrow = TRUE, ncol = 5)
for (i in 1:5){    #Question
  for (j in 1:5){  #Expert
    if (j == 1){e = e1}
    else if (j == 2){e = e2}
    else if (j == 3){e = e3}
    else if (j == 4){e = e4}
    else{e = e5}
    Info[i,j] = 0.05*log(0.05/(e[i,1]-Li[i])) + 0.45*log(0.45/(e[i,2]-e[i,1])) + 0.45*log(0.45/(e[i,3]-e[i,2])) + 0.05*log(0.05/(Ui[i]-e[i,3])) + log(Ui[i]-Li[i])
  }
}

Infoavg = numeric(5)
for (i in 1:5){
  Infoavg[i] = mean(Info[,i])
}

## Maximizing combined score PWDM
alphas = sort(Cal)
cs = Infoavg * Cal  #combined score/performance based weight
cs_PWDMs = numeric(5)
cal_PWDMs = numeric(5)
info_PWDMs = numeric(5)
PWDM_alphas = data.frame(nrow=10001)
PWDMquans_alphas = data.frame(nrow=3)
for (a in 1:5){
  pw = cs
  for (expert in 1:5) {
    if (Cal[expert] < alphas[a]) {
      pw[expert] = 0
    }
  }
  pw = pw/sum(pw)  #normalise the weights
  ## Performance based weights Decision maker
  PWDMquans = data.frame(nrows=3)
  PWDMs = data.frame(nrows=10001)
  for (q in 1:6){ #Question
    curPWDM = numeric(10001)
    for (i in 0:4){ #Expert
      expdistr = cdfexps[2+2*i+(q-1)*10]
      for (j in 1:length(curPWDM)){
        curPWDM[j] = curPWDM[j] + pw[i+1]*expdistr$y[j]
      }
    }
    PWDMs = cbind(PWDMs, curPWDM)
    xas = cdfexps[1+(q-1)*10]
    quan5i = length(curPWDM)-length(which(curPWDM >= 0.05))
    rc5 = (curPWDM[quan5i + 1] - curPWDM[quan5i])/(xas$x[quan5i+1]-xas$x[quan5i])
    quan5 = (0.05 - curPWDM[quan5i] + rc5*xas$x[quan5i])/rc5
    quan50i = length(curPWDM)-length(which(curPWDM >= 0.5))
    rc50 = (curPWDM[quan50i + 1] - curPWDM[quan50i])/(xas$x[quan50i+1]-xas$x[quan50i])
    quan50 = (0.5 - curPWDM[quan50i] + rc50*xas$x[quan50i])/rc50
    quan95i = length(curPWDM)-length(which(curPWDM >= 0.95))
    rc95 = (curPWDM[quan95i + 1] - curPWDM[quan95i])/(xas$x[quan95i+1]-xas$x[quan95i])
    quan95 = (0.95 - curPWDM[quan95i] + rc95*xas$x[quan95i])/rc95
    quantiles = c(quan5,quan50,quan95)
    PWDMquans = cbind(PWDMquans, quantiles)
  }
  print(PWDMquans)
  ## Cal & Inf score PWDM
  m = 5
  n = rep(1, 5)
  while (n[1] < 4 && realised[1] > PWDMquans[n[1],2]) {
    n[1] = n[1] + 1}
  while (n[2] < 4 && realised[2] > PWDMquans[n[2],3]) {
    n[2] = n[2] + 1}
  while (n[3] < 4 && realised[3] > PWDMquans[n[3],4]) {
    n[3] = n[3] + 1}
  while (n[4] < 4 && realised[4] > PWDMquans[n[4],5]) {
    n[4] = n[4] + 1}
  while (n[5] < 4 && realised[5] > PWDMquans[n[5],6] ) {
    n[5] = n[5] + 1}
  s1 = length(which(n==1))/m
  s2 = length(which(n==2))/m
  s3 = length(which(n==3))/m
  s4 = length(which(n==4))/m
  empiricalPWDM = c(s1,s2,s3,s4)
  p = c(0.05, 0.45, 0.45, 0.05)
  logvecPWDM = rep(NA,4)
  for (i in 1:4) {
    if (empiricalPWDM[i] == 0) {
      logvecPWDM[i] = 0}
    else {
      logvecPWDM[i] = log(empiricalPWDM[i]/p[i])}
  }
  I_PWDM = t(logvecPWDM) %*% empiricalPWDM
  CalPWDM = 1-pchisq(2*m*I_PWDM,df=3)
  cal_PWDMs[a] = CalPWDM
  
  Info_PWDM = numeric(5)
  for (i in 1:5){    #Question
    e = PWDMquans
    Info_PWDM[i] = 0.05*log(0.05/(e[1,i+1]-Li[i])) + 0.45*log(0.45/(e[2,i+1]-e[1,i+1])) + 0.45*log(0.45/(e[3,i+1]-e[2,i+1])) + 0.05*log(0.05/(Ui[i]-e[3,i+1])) + log(Ui[i]-Li[i])
  }
  Infoavg_PWDM = mean(Info_PWDM)
  info_PWDMs[a] = Infoavg_PWDM
  
  CS_PWDM = CalPWDM * Infoavg_PWDM
  cs_PWDMs[a] = CS_PWDM
  print(pw)
  PWDM_alphas = cbind(PWDM_alphas, PWDMs[,2:7])
  PWDMquans_alphas = cbind(PWDMquans_alphas, PWDMquans[,7])
}


## Equal weights Decision maker
EWDMs = data.frame(nrow=3)
EWDMquans = data.frame(nrow=3)
for (q in 1:6){
  curEWDM = numeric(10001)
  for (i in 0:4){
    expdistr = cdfexps[2+2*i+(q-1)*10]
    for (j in 1:length(curEWDM)){
      curEWDM[j] = curEWDM[j] + 0.2*expdistr$y[j]
    }
  }
  EWDMs = cbind(EWDMs, curEWDM)
  xas = cdfexps[1+(q-1)*10]
  quan5i = length(curEWDM)-length(which(curEWDM >= 0.05))
  rc5 = (curEWDM[quan5i + 1] - curEWDM[quan5i])/(xas$x[quan5i+1]-xas$x[quan5i])
  quan5 = (0.05 - curEWDM[quan5i] + rc5*xas$x[quan5i])/rc5
  quan50i = length(curEWDM)-length(which(curEWDM >= 0.5))
  rc50 = (curEWDM[quan50i + 1] - curEWDM[quan50i])/(xas$x[quan50i+1]-xas$x[quan50i])
  quan50 = (0.5 - curEWDM[quan50i] + rc50*xas$x[quan50i])/rc50
  quan95i = length(curEWDM)-length(which(curEWDM >= 0.95))
  rc95 = (curEWDM[quan95i + 1] - curEWDM[quan95i])/(xas$x[quan95i+1]-xas$x[quan95i])
  quan95 = (0.95 - curEWDM[quan95i] + rc95*xas$x[quan95i])/rc95
  quantiles = c(quan5,quan50,quan95)
  EWDMquans = cbind(EWDMquans, quantiles)
}

## Cal & inf score EWDM
m = 5
n = rep(1, 5)
while (n[1] < 4 && realised[1] > EWDMquans[n[1],2]) {
  n[1] = n[1] + 1}
while (n[2] < 4 && realised[2] > EWDMquans[n[2],3]) {
  n[2] = n[2] + 1}
while (n[3] < 4 && realised[3] > EWDMquans[n[3],4]) {
  n[3] = n[3] + 1}
while (n[4] < 4 && realised[4] > EWDMquans[n[4],5]) {
  n[4] = n[4] + 1}
while (n[5] < 4 && realised[5] > EWDMquans[n[5],6] ) {
  n[5] = n[5] + 1}
s1 = length(which(n==1))/m
s2 = length(which(n==2))/m
s3 = length(which(n==3))/m
s4 = length(which(n==4))/m
empiricalEWDM = c(s1,s2,s3,s4)
p = c(0.05, 0.45, 0.45, 0.05)
logvecEWDM = rep(NA,4)
for (i in 1:4) {
  if (empiricalEWDM[i] == 0) {
    logvecEWDM[i] = 0}
  else {
    logvecEWDM[i] = log(empiricalEWDM[i]/p[i])}
}
I_EWDM = t(logvecEWDM) %*% empiricalEWDM
CalEWDM = 1-pchisq(2*m*I_EWDM,df=3)


Info_EWDM = numeric(5)
for (i in 1:5){    #Question
  e = EWDMquans
  Info_EWDM[i] = 0.05*log(0.05/(e[1,i+1]-Li[i])) + 0.45*log(0.45/(e[2,i+1]-e[1,i+1])) + 0.45*log(0.45/(e[3,i+1]-e[2,i+1])) + 0.05*log(0.05/(Ui[i]-e[3,i+1])) + log(Ui[i]-Li[i])
}
Infoavg_EWDM = mean(Info_EWDM)

CS_EWDM = CalEWDM * Infoavg_EWDM

## Plot cdfs per question
colours = c('black', 'blue', 'red', 'green', 'yellow')
titles = c('CDFs Question 1', 'CDFs Question 2', 'CDFs Question 3', 'CDFs Question 4', 'CDFs Question 5', 'CDFs Question of interest')
for (q in 0:5){
  plot(NULL, xlim = c(cdfexps[1+q*10]$x[1], cdfexps[1+q*10]$x[10001]), ylim=c(0,1),main = titles[q+1], xlab="Variable value", ylab="CDF")
  for (j in 1:5){
    lines(unlist(cdfexps[1+2*(j-1) + q*10]), unlist(cdfexps[2+2*(j-1) + q*10]), col = colours[j])
  }
  lines(unlist(cdfexps[1+q*10]),PWDM_alphas[,q+20], lty = 'dashed')
  lines(unlist(cdfexps[1+q*10]),EWDMs[,q+2], lty = 'dotdash')
  if (q<=4){
    abline(v=realised[q+1], col='red', lty='dotted')
    legend(x=4*(Ui[q+1]-Li[q+1])/5, y=0.55, legend=c('Expert A', 'Expert B', 'Expert C', 'Expert D', 'Expert E', expression('DM'['perf']), expression('DM'['equal']), 'Real data'), col=c(colours, 'black', 'black', 'red'), lty=c('solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dotdash', 'dotted'))
  }
  else{legend(x=4*(Ui[q+1]-Li[q+1])/5, y=0.5, legend=c('Expert A', 'Expert B', 'Expert C', 'Expert D', 'Expert E', expression('DM'['perf']), expression('DM'['equal'])), col=c(colours, 'black', 'black'), lty=c('solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dotdash'))}
}