############## 5.1 ##############
#C# Ylogrets = diff(log(Y))



#C# c(Y[1],Y[1]*exp(cumsum(Ylogrets)))



#># 
Y = c(30,29,28,28,30,32,31)
Ylogrets = diff(log(Y))
round(Ylogrets,4)
#~[1] -0.0339 -0.0351 0.0000 0.0690 0.0645 -0.0317
Yprices = c(Y[1],Y[1]*exp(cumsum(Ylogrets)))
Yprices
#~[1] 30 29 28 28 30 32 31
#]#



Y=c(1.3,1.2,1.3,1.4,1.5,1.4,1.3,1.4,1.5)
toPrices <- function(Y1,Ylogrets){
  Yprices = c(Y1,Y1*exp(cumsum(Ylogrets)))
  Yprices
}
Y
toPrices(Y[1],diff(log(Y)))
#assert
sum(Y-toPrices(Y[1],diff(log(Y)))<.00000001) == length(Y)



############## 5.2 ##############
#### Figure 5.1
rmixture <- function(N,sigma1,sigma2=0,thresh=.9) {
  variates = vector(length=N)
  U = runif(N)
  for(i in 1:N)
    variates[i] = rnorm(1,0,sd=sigma1)
  if(sigma2 != 0) { #only mixture if sigma2 != 0
    for(i in 1:N)
      if(U[i] >= thresh)
        #replace original variate with mixture variate
        variates[i] = rnorm(1,0,sd=sigma2)
  }
  variates
}
hist(rmixture(10000,sigma1=1,sigma2=5),breaks=50)



#### Figure 5.2, 5.3, 5.4
simPricePath <- function(initPrice,N,seed,sigma1=.05,
                         sigma2=0,thresh=.9) {
  #Non mixture model
  set.seed(seed)
  Xlogrets = rmixture(N,sigma1,sigma2,thresh=thresh)
  Xprices = toPrices(initPrice,Xlogrets)
  list(Xprices,c(Xlogrets))
}
#unit test
seed=26
sigma1=0.007157
N=365
par(mfrow=c(2,2)); maxy=10*.007
Y <- simPricePath(1.3,N=365,seed=seed,sigma1)
Yprices <- Y[[1]]
Ylogrets <- Y[[2]]
plot(Yprices,type='l')
plot(Ylogrets,type='l',ylim=c(-maxy,maxy))
points(Ylogrets)
Z <- simPricePath(1.3,N=365,seed=seed,sigma1,sigma2=4*sigma1)
Zprices <- Z[[1]]
Zlogrets <- Z[[2]]
plot(Zprices,type='l')
plot(Zlogrets,type='l',ylim=c(-maxy,maxy))
points(Zlogrets)
sd(Ylogrets)
sd(Zlogrets)
par(mfrow=c(1,1))
plot(density(Ylogrets))
lines(density(Zlogrets),col=4)



#### Figure 5.5




