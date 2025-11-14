## Regression Analysis

library(wooldridge)
wooldridge::wage1
View(wage1)

attach(wage1)

write.csv(wage1, "wage.csv")

getwd()

mod = lm(wage ~ educ)
summary(mod)

plot(wage ~ educ, main = 'Relation between Wages(Hourly) and Education', 
     xlab = "Education", ylab="Wages", col="green4",pch ="*", cex=1,
     ylim=c(-2,25))

abline(mod, lwd=3)
mod

library(stargazer)

stargazer(mod, type = "text")


# Classical linear model assumption

# 1. Normality of errors- imp argument

hist(mod$residuals, breaks=50, freq = F)
lines(density(mod$residuals), col = "red")

# 2. Heteroskedasticity
#Visualizations

plot(mod$residuals^2~educ, main = "Check for Heteroskedasticity",
     xlab="Education", ylab="Residuals sq", ylim= c(0,100), col="red")

mod_check = lm(mod$residuals^2-educ)
abline(mod, lwd = 5) 


#step:1- Run Regression (wages)
#step:2-  Ei  (Error of model)
#Step:3- Ei^2
#Step:4- Ei^2= delta_0+delta_1+Sum of Errors(Ei) *Reg + beta(i)
# H_0=delta_1 = 0 homosk
# H_1= delta_1 (not equal to) 0  heterosk

library(lmtest)

bptest(mod)



#Multivariate

mod_h = lm(wage ~ educ + exper + tenure + female)
summary(mod_h)
stargazer(mod_h, type = "text")

hist(mod_h$residuals, breaks=30, freq=F)
lines(density(mod$residuals), col="red", lwd=2)


plot(mod_h$residuals^2 ~ mod_h$fitted.values)


bptest(mod_h, ~fitted(mod_h) + I(fitted(mod_h)^2))



##Self-work
wooldridge::minwage
View(minwage)

mod_m = lm(mod, type="text")
summary(mod_m)

stargazer(mod_m, type="text")

plot(minwage$cpi, main="Determining relations for cpi", xlab="Wage", 
     ylab = "cpi", pch="w", col="blue", ylim=c(0,80))

     