library(shiny)
library(randomForest)
library(readr)
library(RCurl)
library(caret)
library(naniar)
library(dplyr)


stroke <- read_csv("healthcare-dataset-stroke-data.csv")
stroke <- stroke[,-1]
write.csv(stroke, "stroke.csv")
df <- stroke
df[df == "N/A"] <- NA
str(df)
df$gender <- factor(df$gender)
df$hypertension <- factor(df$hypertension)
df$heart_disease <- factor(df$heart_disease)
df$ever_married <- factor(df$ever_married)
df$work_type <- factor(df$work_type)
df$Residence_type <- factor(df$Residence_type)
df$smoking_status <- factor(df$smoking_status)
df$stroke <- factor(df$stroke)
##assign mean to fill na
df$bmi <- as.numeric(df$bmi)
df$bmi[is.na(df$bmi)] <- mean(df$bmi,na.rm = TRUE)
df$bmi <- format(round(df$bmi, 2), nsmall = 2)
df$bmi <- as.numeric(df$bmi)
##balance data
df.yes <- df[df$stroke == "1",]
df.no <- df[df$stroke == "0",]
df.train.yes <- df.yes[sample(nrow(df.yes), 125),]
df.train.no <- df.no[sample(nrow(df.no), 125),]
df.train <- rbind(df.train.yes, df.train.no)
df.test.yes <- df.yes[sample(nrow(df.yes), 124),]
df.test.no <- df.no[sample(nrow(df.no), 250),]
df.test <- rbind(df.test.yes, df.test.no)
str(df.train)
write.csv(df.train, "df.train.csv")

library(corrplot)
correlations <- cor(df.train[,c(2, 8, 9, 11)])
corrplot(correlations, method="circle")

pa <-plot(I(age^2)~stroke, df.train)
hist(pa$residuals)

test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)

test$gender <- factor(test$gender, levels = c("male", "female"))
test$hypertension <- factor(test$hypertension, levels = c("0", "1"))
test$heart_disease <- factor(test$heart_disease, levels = c("0", "1"))
test$ever_married <- factor(test$ever_married, levels = c("No", "Yes"))
test$work_type <- factor(test$work_type, levels = c("Private", "Govt_job", "Self-employed", "children", "Never_worked"))
test$Residence_type <- factor(test$Residence_type, levels = c("Urban", "Rural"))
test$smoking_status <- factor(test$smoking_status, levels = c("never smoked", "formerly smoked", "smokes", "Unknown"))
test$stroke <- factor(test$stroke, levels = c("0", "1"))
write.csv(test, "test.csv")




m1 <- glm(stroke~., df.train, family = "binomial")
summary(m1)
m1
plot(m1)


hist(m1$residuals)

m2 <- glm(stroke~ gender + log(age) + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status, df.train, family = "binomial")
m2
summary(m2)
plot(m2)
hist(m2$residuals)

set.seed(243)
m3 <- glm(stroke~ gender + I(age^2) + hypertension + heart_disease + ever_married + work_type + Residence_type + I(avg_glucose_level^2) + bmi + smoking_status, df.train, family = "binomial")
m3
summary(m3)
plot(m3)

hist(m3$residuals)


m4 <- glm(stroke ~ gender + I(age^2) + I(avg_glucose_level^2) + bmi, data = df.train, family = "binomial")
m4
summary(m4)


m5 <- glm(stroke ~ I(age^2) + I(avg_glucose_level^3) + bmi, data = df.train, family = "binomial")
m5
summary(m5)
saveRDS(m3, "m3.rds")





hist(m3$residuals)
library


df <- data.frame(
  Name = c("gender",
           "age",
           "hypertension",
           "heart_disease",
           "ever_married",
           "work_type",
           "Residence_type",
           "avg_glucose_level",
           "bmi",
           "smoking_status"),
  Value = (c(input$gender,
             input$age,
             input$hypertension,
             input$heart_disease,
             input$ever_married,
             input$work_type,
             input$Residence_type,
             input$avg_glucose_level,
             input$bmi,
             input$smoking_status)),
  stringsAsFactors = FALSE)

stroke <- "stroke"
df <- rbind(df, stroke)


str(df.train)
print(df.train)
last(df.train)
write.csv(input, "input.csv")


plot(stroke~, df.train)



plot(m3)


library(devtools)
install_github("mtennekes/tabplot")
library(tabplot)
tableplot(df.train,
          select = c(gender, age, hypertension, heart_disease, ever_married, work_type, Residence_type, avg_glucose_level, smoking_status, bmi, stroke),
          sortCol = stroke, title = "Table Plot of Stroke Data After Balancing and Replacing Missing Values")
