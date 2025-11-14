library(MASS)
library(factoextra)
data("biopsy")
View(biopsy)

?biopsy

colSums(is.na(biopsy))
biopsy= na.omit(biopsy)

biopsy_sample = biopsy[ , -c(1,11)]

biopsy_pca = prcomp(biopsy_sample, scale. = T)

summary(biopsy_pca)

#Facto vista- factors visualization
fviz_eig(biopsy_pca, addlabels = T)

fviz_pca_biplot(biopsy_pca, label = "var", habillage = biopsy$class)

library(titanic)
View(Titanic)

library(pacman)

library(help=pacman)

p_data(datasets)

data("AirPassengers")
View("AirPassengers")

is.na(AirPassengers)
AP_pca=prcomp(AirPassengers, scale=T)
View(AP_pca)
summary(AP_pca)

fviz_eig(AP_pca, addlabels = T)
