#Market Basket Analysis

#Call libraries
library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)

#Reading CSV with data
data <- read.csv("dataset_bd3.csv")

#Overview
dim(data) #15002 lines and 20 columns
View(data)
summary(data)

#If we watch carefully the dataset, we can see that the even lines are complete,
#while the odd lines aren't. We can filter only even lines in the our dataset

even_lines <- seq(2, nrow(data), 2)
odd_lines <- seq(1, nrow(data), 2)

#New DF
df <- data[even_lines, ]
View(df)

#Checking if it has null values in firts column
sum(is.na(df$Item01))

#Warning: the function is.na(x) check completed data absence, the space isn't considered

which(nchar(trimws(df$Item01)) == 0)
which(nchar(trimws(df$Item02)) == 0)

#Or we can use:
grepl("^\\s*$", df$Item02)

#Creating a new dataframe where the second column isn't null
df_new <- df[!grepl("^\\s*$", df$Item02), ]
View(df_new)

#Limiting our df to six columns 
df_copy <- df_new
df_copy$Item01 <- as.factor(df_copy$Item01)
df_copy$Item02 <- as.factor(df_copy$Item02)
df_copy$Item03 <- as.factor(df_copy$Item03)
df_copy$Item04 <- as.factor(df_copy$Item04)
df_copy$Item05 <- as.factor(df_copy$Item05)
df_copy$Item06 <- as.factor(df_copy$Item06)

df_limited <- split(df_copy$Item01,
                    df_copy$Item02,
                    df_copy$Item03,
                    df_copy$Item04,
                    df_copy$Item05,
                    df_copy$Item06,
                    drop = FALSE)
View(df_limited)

#Transform data in class "transactions"
transactions <- as(df_limited, "transactions")

#Inspect the rules
inspect(head(transactions, 5))

#Creat the rules to check one product (HP 61 ink)
rules_product <- apriori(transactions, #algorithm apriori
                         parameter = list(minlen = 3, conf = 0.5), #determining the leght and confidence 
                         appearance = list(rhs = "HP 61 ink", default = "lhs")) #lhs: antecedent and rhs: consequence

#Inspect the rules
inspect(head(sort(rules_product, by = "confidence"), 5))

#Introducing a new parameter
rules_product1 <- apriori(transactions, 
                         parameter = list(minlen = 3, conf = 0.5, sup = 0.2, target = "rules"),
                         appearance = list(rhs = "HP 61 ink", default = "lhs"))

inspect(head(sort(rules_product1, by = "confidence"), 5))

#Filter the redundant rules
rules_clean <- rules_product1[!is.redundant(rules_product1)]
inspect(head(sort(rules_clean, by = "confidence"), 5))

#Save the rules of product one (HP 61 ink) in a dataframe
df_product <- as(rules_clean, "data.frame")
View(df_product)