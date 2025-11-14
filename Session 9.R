library(wooldridge)
library(lmtest)
library(nlme)
library(stargazer)


#regression -> significance -> R^2 -> Classical Linear Model Regression Assumption
#1. Normality of residual -> Homoskedastic -> Multi-collinearity -> Autocorrelation

attach(wage1)

mod = lm(wage~educ)
stargazer(mod, type = 'text')

hist(mod$residuals, breaks=30, freq=F)
lines(density(mod$residuals), col='green', lwd=3)

plot(wage~educ, main="Relation bw wage and education", pch="&", col='pink')
abline(mod, col = "brown", lwd = 3)

# Visual
# Calc variance of errors for Heteroskedasticity
#p-value < 0.05, heterosk
#p=value> 0.05, homosk

plot(mod$residuals^2~educ, pch="^", col="blue")
bptest(mod)

# Remedial measures
# Extend the model

colnames(wage1)

mod_multi = lm(wage~educ+exper+tenure+nonwhite+female+married)

stargazer(mod_multi, type="text") #help in comparing both of them

stargazer(mod, mod_multi, type="text")

plot(mod_multi$residuals^2~mod_multi$fitted.values, main="Heteroskedasticity checks",
     xlab="Independent variables", ylab="ei^2", pch="%", col="red")

mod_HC= lm(mod_multi$residuals^2~mod_multi$fitted.values)

abline(mod_HC, col="blue", lwd=4)

#White's test

bptest(mod_multi, ~fitted.values(mod_multi)+ I(fitted.values(mod_multi)^2))                     

colnames(wage1)

mod_multi_sq = lm(wage~educ+exper+tenure+nonwhite+female+married+expersq+tenursq)

stargazer(mod_multi_sq, type="text")

stargazer(mod_multi, mod_multi_sq, type="text")

bptest(mod_multi_sq, ~fitted.values(mod_multi_sq)+ I(fitted.values(mod_multi_sq)^2))                     


#2. Log linearize

mod_log = lm(log(wage)~educ)

stargazer(mod, mod_log, type = "text")

plot(log(wage)~educ, main="Relation bw wage and education", pch="&", col='darkblue', ylim=c(-2,25))
abline(mod, col = "brown", lwd = 3)

bptest(mod_log)

#Beta_0 = Ybar- Beta_1*Xbar
#Bechmark= give best fitted line- OLS
#Summation Yi= Summation(Beta_0+Beta_1*Xi+Ei)
#Summation Ei = Summation(Yi-Beta_1*Xi+-Beta_0)
#to get best fitted line, value is 0


mod_gls = gls(wage~educ, weights=varIdent(form =~1|female), data=wage1)
stargazer(mod, mod_gls, type = "text")

colnames(wage1)










