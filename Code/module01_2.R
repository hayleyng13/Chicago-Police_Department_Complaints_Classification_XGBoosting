library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(forecast)
library(lubridate)
library(dplyr)
library(glmnet)
#####################

##Xtr
Xtr_1 <- Xtr[c("Date_Recorded","Town","Assessed_Value","Property_Type","Residential_Type","Address")]
Xte_1 <- Xte[c("Date_Recorded","Town","Assessed_Value","Property_Type","Residential_Type","Address")]
Xtr_1 = rbind(Xtr_1,Xte_1)
Xtr_1$Date_Recorded <- as.Date(Xtr_1$Date_Recorded,format = "%m/%d/%Y")
Xtr_1$Month <- month(Xtr_1$Date_Recorded)
Xtr_1$Year <- year(Xtr_1$Date_Recorded)

Xtr_1$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr_1$Property_Type)
Xtr_1$Town <- as.factor(Xtr_1$Town)
Xtr_1$Month <- as.factor(Xtr_1$Month)
Xtr_1$Year <- as.factor(Xtr_1$Year)
Xtr_1$Type <- ifelse(Xtr_1$Property_Type != "Residential", Xtr_1$Property_Type, Xtr_1$Residential_Type)
Xtr_1$Type <- as.factor(Xtr_1$Type)
drop <-c("Date_Recorded","Property_Type","Residential_Type")
Xtr_1 <- Xtr_1[, !names(Xtr_1) %in% drop]
Xtr_1$Type[Xtr_1$Type == ""] <- NA
##Xte

Xte_1 <- Xte[c("Date_Recorded","Town","Assessed_Value","Property_Type","Residential_Type")]
Xte_1$Date_Recorded <- as.Date(Xte_1$Date_Recorded,format = "%m/%d/%Y")
Xte_1$Month <- month(Xte_1$Date_Recorded, label = TRUE)
Xte_1$Month <- ifelse(is.na(Xte_1$Month), "6", Xte_1$Month)
Xte_1$Year <- year(Xte_1$Date_Recorded)
Xte_1$Year <- ifelse(is.na(Xte_1$Year), 2002, Xte_1$Year)
Xte_1$Year <- gsub("1999", "2001", Xte_1$Year)
Xte_1$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xte_1$Property_Type)
Xte_1$Type <- as.factor(Xte_1$Type)
Xte_1$Town[Xte_1$Town == "***Unknown***"] <-  "Bridgeport"

Xte_1$Town <- as.factor(Xte_1$Town)
Xte_1$Month <- as.factor(Xte_1$Month)
Xte_1$Year <- as.factor(Xte_1$Year)
Xte_1$Type <- ifelse(Xte_1$Property_Type != "Residential", Xte_1$Property_Type, Xte_1$Residential_Type)
Xte_1$Type <- as.factor(Xte_1$Type)
Xte_1 <- Xte_1[, !names(Xte_1) %in% drop]
Xte_1$Type[Xte_1$Type == ""] <- NA


##RF for NA's in Xtr

training_data <- Xtr_1[!is.na(Xtr_1$Assessed_Value), ]
prediction_data <- Xtr_1[is.na(Xtr_1$Assessed_Value), ]
library(h2o)
h2o.init()
training_data_h2o <- as.h2o(training_data)

prediction_data_h2o <- as.h2o(prediction_data)

rf_model <- h2o.randomForest(y = "Assessed_Value",
                             training_frame = training_data_h2o,
                             ntrees = 500,
                             max_depth = 20)

predicted_values <- h2o.predict(rf_model, newdata = prediction_data_h2o)

predicted_values_vector <- as.vector(predicted_values$predict)

missing_rows <- is.na(Xtr_1$Assessed_Value)
Xtr_1$Assessed_Value[missing_rows] <- predicted_values_vector

h2o.shutdown()
write.table(Xtr_1, file = "data_mod2.csv", quote=F, row.names=F, sep = ",")

##RF for Xte
training_data2 <- Xte_1[!is.na(Xte_1$Assessed_Value), ]
prediction_data2 <- Xte_1[is.na(Xte_1$Assessed_Value), ]
h2o.init()
training_data2_h2o <- as.h2o(training_data2)

prediction_data2_h2o <- as.h2o(prediction_data2)


rf_model2 <- h2o.randomForest(y = "Assessed_Value",
                             training_frame = training_data2_h2o,
                             ntrees = 500,
                             max_depth = 20)


predicted_values2 <- h2o.predict(rf_model2, newdata = prediction_data2_h2o)

predicted_values_vector2 <- as.vector(predicted_values2$predict)

missing_rows2 <- is.na(Xte_1$Assessed_Value)
Xte_1$Assessed_Value[missing_rows2] <- predicted_values_vector2

h2o.shutdown()
write.table(Xte_1, file = "Xte_1.csv", quote=F, row.names=F, sep = ",")

##RF for Property type
Xtr_2 = Xtr_1


training_data3 <- Xtr_2[!is.na(Xtr_2$Type), ]

prediction_data3 <- Xtr_2[is.na(Xtr_2$Type), ]
h2o.init()
training_data3_h2o <- as.h2o(training_data3)

prediction_data3_h2o <- as.h2o(prediction_data3)



rf_model3 <- h2o.randomForest(y = "Type",
                              training_frame = training_data3_h2o,
                              ntrees = 500,
                              max_depth = 20)


predicted_values3 <- h2o.predict(rf_model3, newdata = prediction_data3_h2o)

predicted_values_vector3 <- as.vector(predicted_values3$predict)

missing_rows3 <- is.na(Xtr_2$Type)
Xtr_2$Type[missing_rows3] <- predicted_values_vector3

h2o.shutdown()
write.table(Xtr_2, file = "Xtr_2.csv", quote=F, row.names=F, sep = ",")

##RF for Type - Xte
# Set seed for reproducibility
set.seed(42)
Xte_1 <- read.csv("Xte_1.csv")
# Get the number of rows in the original dataset
n <- nrow(Xte_1)

# Generate a random sample of row indices for half of the dataset
sample_indices <- sample(1:n, size = n/2, replace = FALSE)

# Create the new dataset with half of the rows
Xte_first <- Xte_1[sample_indices, ]

Xte_last <- Xte_1[-sample_indices, ]

training_data4 <- rbind(Xte_first[!is.na(Xte_2$Type), ],Xte_last[!is.na(Xte_2$Type), ])

prediction_data4 <- Xte_first[is.na(Xte_first$Type), ]
prediction_data5 <- Xte_last[is.na(Xte_last$Type), ]
training_data4$Year <- factor(training_data4$Year, levels = levels(training_data4$Year))
h2o.init()
training_data4_h2o <- as.h2o(training_data4)

prediction_data4_h2o <- as.h2o(prediction_data4)
prediction_data5_h2o <- as.h2o(prediction_data5)


rf_model4 <- h2o.randomForest(y = "Type",
                              training_frame = training_data4_h2o,
                              ntrees = 300,
                              max_depth = 10)


predicted_values4 <- h2o.predict(rf_model4, newdata = prediction_data4_h2o)
predicted_values5 <- h2o.predict(rf_model4, newdata = prediction_data5_h2o)
predicted_values_vector4 <- as.vector(predicted_values4$predict)
predicted_values_vector5 <- as.vector(predicted_values5$predict)
missing_rows4 <- is.na(Xte_first$Type)
Xte_first$Type[missing_rows4] <- predicted_values_vector4
missing_rows5 <- is.na(Xte_last$Type)
Xte_last$Type[missing_rows5] <- predicted_values_vector5

h2o.shutdown()
Xte_2 <- rbind(Xte_first,Xte_last)
write.table(Xte_2, file = "Xte_2.csv", quote=F, row.names=F, sep = ",")


###Taking LOG
Xte_2 = Xte_1
Xtr_2 = mutate(Xtr_2,log10Value = log10(Xtr_2$Assessed_Value))
Xte_2 = mutate(Xte_2,log10Value = log10(Xte_2$Assessed_Value))
Xtr_lm_log2 <- Xtr_2$log10Value[!is.infinite(Xtr_2$log10Value)]
Xte_lm_log2 <- Xte_2$log10Value[!is.infinite(Xte_2$log10Value)]
mean_log = mean(c(Xtr_lm_log2,Xte_lm_log2),na.rm=T)


Xtr_2$log10Value[is.infinite(Xtr_2$log10Value)] = mean_log

Xte_2$log10Value[is.infinite(Xte_2$log10Value)] = mean_log


### MODEL

pred0 = read.csv("pred0.csv")
pred1 = pred0
Xtr_2 <- subset(Xtr_2, select = -Assessed_Value)
Xte_2 <- subset(Xte_2, select = -Assessed_Value)
Xtr_3 <- subset(Xtr_2, select = -Type)
Xte_3 <- subset(Xte_2, select = -Type)
poly_features_train <- poly(Xtr_2$log10Value, 3)
Xtr_2 <- cbind(Xtr_2, poly_features_train)
model1 = lm(y ~ . ,  data.frame(X = Xtr_2,y = Ytr$Sale_Amount))

model2 = lm(y ~. ^2, data.frame(X = Xtr_2, y = Ytr$Sale_Amount))

poly_features_test <- poly(Xte_2$log10Value, 3)
Xte_2 <- cbind(Xte_2, poly_features_test)
predictions <- predict(model, data.frame(X=Xte_2))
pred1$Sale_Amount <- predictions

pred1$Sale_Amount = predict(model, data.frame( X = Xte_2))
pred1$Sale_Amount[pred1$Sale_Amount < 0] <- abs(pred1$Sale_Amount[pred1$Sale_Amount < 0])
write.table(pred1, file = "pred1.csv", quote=F, row.names=F, sep = ",")

Xtr_2$Town <- as.character(Xtr_2$Town)
Xtr_2$Town[Xtr_2$Town %in% rare_categories] <- "Others"
Xte_2$Town <- as.character(Xte_2$Town)
Xte_2$Town[Xte_2$Town %in% rare_categories] <- "Others"

Xtr_2$Town <- as.factor(Xtr_2$Town)
Xte_2$Town <- as.factor(Xte_2$Town)
Xte_2$Year <- as.character(Xte_2$Year)
Xte_2$Year <- gsub("1999", "2001", Xte_2$Year)
Xte_2$Year <- as.factor(Xte_2$Year)

model <- lm(Sale_Amount ~ poly(log10Value, 3) * poly(Town, 3) * poly(Month, 3) * poly(Year, 3), data =Xtr_2)

# Stepwise regression model
step.model <- stepAIC(model, direction = "both", 
                      trace = FALSE)



# Get frequency counts of 'Town' in the training set
town_counts <- table(Xtr_2$Town)
town_counts2 <- table(Xte_2$Town)
adds_count <- table(Xtr$Address)
adds_count2 <- table(Xte$Add)
rare_add <- names(adds_count[adds_count < 20])
rare_add2 <- names(adds_count2[adds_count2 < 50])
# Identify categories occurring less than 1000 times
rare_categories <- names(town_counts[town_counts < 1000])
rare_categories2 <- names(town_counts[town_counts2 < 1000])

# Set these categories to "Others" in both training and test sets

# Assuming Xtr and Xte are data frames and 'Town' is a categorical variable

lasso.1 <- glmnet(y = Ytr$Sale_Amount, x = Xtr_2, family = "gaussian")
# ("%Dev" in output below is R-square in linear regression)
lasso.1
plot(lasso.1) # Plots coefficient path
##### Note that these are on original scales, even though LASSO scaled variables
coef(lasso.1)

cv.lasso.1 <- cv.glmnet(y = Ytr$Sale_Amount, x = as.matrix(Xtr_2), family = "gaussian")
cv.lasso.1
plot(cv.lasso.1) # Plot CV-MSPE
coef(cv.lasso.1) # Print out coefficients at optimal lambda
coef(cv.lasso.1, s = cv.lasso.1$lambda.min) # Another way to do this.
# Using the "+1SE rule" (see later) produces a sparser solution
coef(cv.lasso.1, s = cv.lasso.1$lambda.1se) # Another way to do this.

# Predict both halves using first-half fit
pred.las1.min <- predict(cv.lasso.1, newx = as.matrix(Xte_1), s = cv.lasso.1$lambda.min)
pred.las1.1se <- predict(cv.lasso.1, newx = Xte_2, s = cv.lasso.1$lambda.1se)
pred.las1 <- predict(cv.lasso.1, newx = Xte_2) # 1SE is the default!

MSPE1.las.min <- mean((y.2 - pred.las1.min)^2)
MSPE1.las.1se <- mean((y.2 - pred.las1.1se)^2)

pred1$Sale_Amount <- pred.las1.min