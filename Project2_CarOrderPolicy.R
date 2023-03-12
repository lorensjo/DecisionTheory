### Project 2
################################################################################

### constants:
W = 10^7                                   #budget
V = 10^3                                      #storage
p = c(50000,60000,65000,85000,95000,120000)   #sell price
v = c(36000,40000,42000,55000,61000,75000)    #buy price
g = c(25000,30000,32000,42500,57500,60000)    #dump price
k = c(5,6,4,9,8,9)                            #space coefficients
# Distribution parameters
demand = c(60,45,33,42,25,30)
std = c(10,15,6,12,5,5)

## solving without constraints
q = rep(NA, length(p))
sumcost = 0
sumstorage = 0
for (index in 1:length(p)) {
  quartile = (p[index]-v[index])/(p[index]-g[index])
  q[index] = qnorm(quartile, mean = demand[index], sd=std[index])
  sumcost = sumcost + v[index]*q[index]
  sumstorage = sumstorage + k[index]*q[index]
}
print(sumcost)
print(sumstorage)

### Solving with equality constraint for budget
#Thus, we set M2 = 0
W = 10^7 - 60000
M = 0.1
δ = 10^(-4)
ε = W*0.01
n = 0

sumcost = 0
sumstorage = 0
while (!(W-ε-60000 < sumcost && sumcost < W+ε-60000)) {
  n = n + 1
  if (n >10000) {
    break
  }
  
  if (sumcost < W) {
    M = M - δ
  } else {
    M = M + δ 
    }
  
  q_opt = rep(NA, length(p))
  sumcost = 0
  sumstorage = 0
  for (index in 1:length(p)) {
    quartile = (   p[index]- v[index]*(1+M)  ) / (p[index] - g[index])
    #if (quartile < 0) {break}
    q_opt[index] = qnorm(quartile, mean=demand[index], sd=std[index])
    sumcost = sumcost + v[index]*q_opt[index]
    sumstorage = sumstorage + k[index]*q_opt[index]
  }
}
print(sumcost)
print(M)
print(sumstorage)
## sumstorage is too large, > 1000



### Solving for M1 = 0 and thus only storage equality

M = 2799.999
δ = 0.00000001
ε = 0.01*V
n = 0

sumcost = 0
sumstorage = 0
while (!(V-ε < sumstorage && sumstorage <= V)) {
  n = n + 1
  if (n >100000) {
    break
  }
  
  if (sumstorage <= V-ε) {
    M = M - δ
  } else {
    M = M + δ 
  }
  
  q_opt = rep(NA, length(p))
  sumcost = 0
  sumstorage = 0
  for (index in 1:length(p)) {
    quartile = (   p[index]- v[index]- M*k[index]  ) / (p[index] - g[index])
    if (quartile < 0) {break}
    q_opt[index] = qnorm(quartile, mean=demand[index], sd=std[index])
    sumcost = sumcost + v[index]*q_opt[index]
    sumstorage = sumstorage + k[index]*q_opt[index]
  }
}
print(sumcost)
print(M)
print(sumstorage)

### Calculating expected loss
loss = 0
q_opt = floor(q_opt)
for (i in length(q_opt)){
  loss = loss + max((demand[i] - q_opt[i]), 0) * (p[i] - v[i])
  loss = loss + max((q_opt[i] - demand[i]), 0) * (v[i] - g[i])
}
print(loss)

### Calculating expected income
income = 0
q_opt = floor(q_opt)
for (i in length(q_opt)){
  income = income + min(q_opt[i], demand[i]) * (p[i] - v[i])
  income = income - max(0, (q_opt[i] - demand[i])) * (v[i] - g[i])
}
print(income)
