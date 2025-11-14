employees = read.csv("C:/Users/Admin/Downloads/NMIMS Mumbai BA Docs/Trim 2/Multivariate Data Analysis/HR_Attrition_Indian_Dataset.csv")

library(readxl)     
library(dplyr)      
library(ggplot2)    
library(lubridate)  
library(skimr)      
library(stargazer)
library(knitr)
library(RColorBrewer)

glimpse(employees)

skim(employees)

employees %>%
  count(Gender) %>%
  mutate(Percent = n/sum(n)*100)

ggplot(employees, aes(x = MonthlySalary)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Monthly Salary", x = "Monthly Salary", y = "Count")

## Actual Session 6

breaks = seq(0,55, 10)
breaks

employees = employees%>%
  mutate(dist_home = cut(DistanceFromHome, breaks = breaks, 
                         labels = c("<10", "11 to 20", "21 to 30","31 to 40","41 to 50")))


breaks

ggplot(employees, aes(x = dist_home, fill = LeftCompany))+
  geom_bar(position = "dodge")+
  labs(title = "Distance vs Attrition")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.6))


# Workforce Analysis

library(lubridate)

emp_join = employees%>%
  mutate(JoinYear = year(DateOfJoining))%>%
  group_by(JoinYear)%>%
  summarise(Joiners = n())

kable(emp_join)

ggplot(emp_join, aes(x = factor(JoinYear), y = Joiners, fill = factor(JoinYear)))+
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Paired")+
  labs(title = "Employee Joining Per Year", x="Join Year", fill = "Join Year")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.6))+
  scale_fill_brewer(palette="Set3")


emp_leave = employees%>%
  mutate(JoinYear = year(DateOfJoining))%>%
  filter(LeftCompany=="Yes")%>%
  group_by(JoinYear)%>%
  summarise(Leavers=n())

kable(emp_leave)

ggplot(emp_leave, aes(x = factor(JoinYear), y = Leavers, fill = factor(JoinYear)))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "RdYlGn")+
  theme_light()+
  labs(title = "No. of Job Leavers Per Year", x = "Joining Year", fill = "Joining Year")+
  theme(plot.title = element_text(hjust = 0.5))

# Survival Analysis

survival = employees%>%
  mutate(JoinYear = year(DateOfJoining)) %>%
  group_by(JoinYear)%>%
  summarise(
    Total_Joined = n(),
    Still_Here = sum(LeftCompany == "No"),
    RetentionRate = Still_Here/Total_Joined
  )

kable(survival)

ggplot(survival, aes(x= factor(JoinYear), y = RetentionRate, fill = factor(JoinYear)))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "BrBG")+
  labs(title = "Retention Rate", x = "Joining Year", fill = "Joining Year")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

attrition = employees %>%
  mutate(JoinYear = year(DateOfJoining)) %>%
  group_by(JoinYear)%>%
  summarise(AttritionRate = mean(LeftCompany == "Yes"))

kable(attrition)

ggplot(attrition, aes(x= factor(JoinYear), y = AttritionRate, fill = factor(JoinYear)))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "PiYG")+
  labs(title = "Attrition Rate", x = "Joining Year", fill = "Joining Year")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

library(survival)


employees$Event = ifelse(employees$LeftCompany=="Yes", 1,0)


surv_obj = Surv(time = employees$YearsWithCompany, event = employees$Event)
surv_obj

km_fit = survfit(surv_obj ~ employees$Gender, data = employees)
summary(km_fit)


# Session 7


aov = aov(employees$MonthlySalary ~ factor(AppraisalRating), data = employees)
summary(aov)
