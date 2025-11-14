#Multi-collinearity
library(lmtest)
library(nlme)
library(stargazer)
attach(longley)
library(wooldridge)

head(longley)

View(longley)

mod=lm(Employed~GNP+GNP.deflator+Population)
stargazer(mod, type="text")

colnames(longley)
vars = longley[ , c(1,2,5)]

corr=cor(vars)
corr

attach(wage1)

colnames(wage1)

#Variance Inflation Factor

#1/1-R^2

library(car)

vif(mod)
vif(mod_multi)


#Principal Component Analysis

pca_results = prcomp(vars, scale.=T)

library("factoextra")
fviz_eig(pca_results, addlabels = TRUE)  

Employed~pca_results$x[,1]


mod_pca=lm(Employed~pca_results$x[,1])
stargazer(mod_pca, type="text")

