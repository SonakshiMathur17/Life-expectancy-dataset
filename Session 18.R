#LINEAR DISCRIMINANT ANALYSIS

wine = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")

wine$quality_label = ifelse(wine$quality>=7, "High",
                            ifelse(wine$quality>=5, "Medium", "Low"))

glimpse(wine)

wine$quality_label = as.factor(wine$quality_label)

wine = wine[ , -12]
n = nrow(wine)

set.seed(123)

train_index = sample(1:n, size=0.7*n)

train_data = wine[train_index, ]
test_data = wine[-train_index, ]

library(MASS)

lda_model = lda(quality_label ~., data= train_data )

print(lda_model)

lda_values = predict(lda_model, newdata = test_data)

test_data$pred_class = lda_values$class

cm = table(Actuals = test_data$quality_label, Predicted = test_data$pred_class)
cm

library(ggplot2)


ggplot(data.frame(lda_values$x, quality_label=test_data$quality_label),
       aes(x=LD1, y=LD2, color=quality_label)) +
  geom_point(alpha=3) +
  labs(title="LDA:Wine Quality Separation", x="LDI", y="LDI2")+
  theme_minimal()

