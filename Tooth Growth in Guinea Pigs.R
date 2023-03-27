#Libraries
library(ggplot2)
library(GGally)

#Analyse of data
?ToothGrowth
data("ToothGrowth")
View(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)

#Histogram
hist(ToothGrowth$len)

#Boxplot
qplot(supp,
      len,
      data = ToothGrowth,
      main = "Tooth Growth in Guinea Pigs",
      xlab = "Supplement type",
      ylab = "Tooth length")
+ geom_boxplot(aes(fill = supp))


#We'll use T-Test to compare the mean of two supplement, but firts we need test the suppositions o T-Test
# One of suppositions of T-Test is that residual variance is homogeneous, to prove this supposition, we'll use F-Test
#where H0 is that mean extract of a normally distributed population have the same variance
var.test(len ~ supp, data = ToothGrowth)

#p-value > 0.05, so don't have difference between the variance of two groups

#Now we'll see if the distribution is normally distributed using Shapiro-Wilk Test
shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'OJ'])
shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'VC'])

#The p-value of supp OJ is < 0.05, so we need reject H0 (the distribuition is normal)
# Is the dosage of the supplement what really makes the difference and not the type of supplement?

#For this, we will use the ANOVA test
dose_0.5 = ToothGrowth$len[ToothGrowth$dose == 0.5]
dose_1 = ToothGrowth$len[ToothGrowth$dose == 1]
dose_2 = ToothGrowth$len[ToothGrowth$dose == 2]

#Shapiro-Wilk Test to verify distribution
shapiro.test(dose_0.5)
shapiro.test(dose_1)
shapiro.test(dose_2)

#In three cases, p-value > 0.05, so the three sample show normal distribution

#ANOVA Test
anova_test <- aov(len ~ dose, data = ToothGrowth)
summary(anova_test)

#So, we can conclude that supplement don't affect the lenght of tooth pigs, but rather the dosage of supplement