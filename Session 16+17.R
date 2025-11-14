
date_week = read.csv("C:/Users/Admin/Downloads/NMIMS Mumbai BA Docs/Trim 2/Multivariate Data Analysis/date_to_week_id_map.csv")
product_price = read.csv("C:/Users/Admin/Downloads/NMIMS Mumbai BA Docs/Trim 2/Multivariate Data Analysis/product_prices.csv")
train_data = read.csv("C:/Users/Admin/Downloads/NMIMS Mumbai BA Docs/Trim 2/Multivariate Data Analysis/train_data.csv")

rm(list = ls())

colnames(date_week)
colnames(product_price)
colnames(train_data)
colnames(date_price)

date_price = merge(date_week, product_price, by='week_id')

final_data = merge(train_data, date_price, by=c('product_identifier', 'outlet', 'date'))

glimpse(final_data)

unique(final_data$state)

final_data$date = as.Date(final_data$date)

library(dplyr)

df_Maharashtra = subset(final_data, state == "Maharashtra")
df_Telangana = subset(final_data, state == "Telangana")
df_Kerala = subset(final_data, state == "Kerala")

best_products = final_data %>%
  group_by(state, product_identifier) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = 'drop') %>%
  arrange(state, desc(total_sales)) %>%
  slice(which.max(total_sales)) 

head(best_products)

final_data$date = as.Date(final_data$date)

final_data[ , c(2,3,4,5,6)] = lapply(final_data[ , c(2,3,4,5,6)], as.factor)

final_data$state
unique(final_data$state)
unique(final_data$category_of_product)

glimpse(final_data)
maha = final_data[final_data$state == 'Maharashtra', ] 
Tel = final_data[final_data$state == 'Telangana', ]
kerala = final_data[final_data$state == 'Kerala', ]

library(stargazer)

maharashtra_revenue = sum(maha$sell_price * maha$sales, na.rm = TRUE)
telangana_revenue = sum(Tel$sell_price * Tel$sales, na.rm = TRUE)
kerala_revenue = sum(kerala$sell_price * kerala$sales, na.rm = TRUE)

top_outlets_maharashtra = maha %>%
  group_by(outlet) %>%
  summarise(total_revenue = sum(sell_price * sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 5)

tp_kerala= kerala %>%
  group_by(outlet) %>%
  summarise(total_revenue = sum(sell_price * sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n= 5)

tp_telangana= Tel %>%
  group_by(outlet) %>%
  summarise(total_revenue = sum(sell_price * sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n= 5)

mod = lm(final_data$sales ~ final_data$state)

stargazer(mod, type = 'text')

unique(final_data$department_identifier)

library(broom)

# price sensitivity analysis
library(tidyr)
colnames(final_data)

sensitivity_results <- final_data %>%
  group_by(department_identifier) %>%
  do(model = lm(sales ~ sell_price, data = .)) %>%
  mutate(tidy_model = list(tidy(model))) %>%
  unnest(tidy_model) %>%
  filter(term == "sell_price") %>%
  select(department_identifier, estimate, std.error, statistic, p.value)

head(sensitivity_results)
library(ggplot2)

ggplot(sensitivity_results, aes(x = reorder(department_identifier, estimate), y = estimate)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Price Sensitivity by Department",
       x = "Department",
       y = "Change in Sales per 1 Unit Increase in Price") +
  theme_minimal()

elasticity_results <- final_data %>%
  group_by(department_identifier) %>%
  do(model = lm(log(sales + 1) ~ log(sell_price + 1), data = .)) %>%
  mutate(tidy_model = list(tidy(model))) %>%
  unnest(tidy_model) %>%
  filter(term == "log(sell_price + 1)") %>%
  select(department_identifier, estimate, p.value)

head(elasticity_results)
unique(final_data$department_identifier)

deptA = subset(final_data, department_identifier == "11")
deptB = subset(final_data, department_identifier == "12")
deptC = subset(final_data, department_identifier == "21")
deptD = subset(final_data, department_identifier == "22")
deptE = subset(final_data, department_identifier == "31")
deptF = subset(final_data, department_identifier == "33")

deptA = deptA[deptA$sales != 0, ]
deptB = deptB[deptB$sales != 0, ]
deptC = deptC[deptC$sales != 0, ]
deptD = deptD[deptD$sales != 0, ]
deptE = deptE[deptE$sales != 0, ]
deptF = deptF[deptF$sales != 0, ]

stargazer(mod1, type = 'text')

mod1 = lm(log(deptA$sales)~log(deptA$sell_price))
mod2 = lm(log(deptB$sales)~log(deptB$sell_price))
mod3 = lm(log(deptC$sales)~log(deptC$sell_price))
mod4 = lm(log(deptD$sales)~log(deptD$sell_price))
mod5 = lm(log(deptE$sales)~log(deptE$sell_price))
mod6 = lm(log(deptF$sales)~log(deptF$sell_price))

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = 'text')

e1 = mod1$coefficients[2]
e2 = mod2$coefficients[2]
e3 = mod3$coefficients[2]
e4 = mod4$coefficients[2]
e5 = mod5$coefficients[2]
e6 = mod6$coefficients[2]

elas = data.frame(x=c(11,12,21,22,31,33), y=c(e1,e2,e3,e4,e5,e6))

ggplot(elas, aes(x=as.factor(x), y=y, fill=as.factor(x)))+
  geom_bar(stat = "identity")





