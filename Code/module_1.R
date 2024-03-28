library(lubridate)

subset_data <- Xtr[Xtr$Assessed_Value < 1000000, ]

Xtr <- Xtr %>% 
  mutate("Date Recorded" = as.Date(`Date_Recorded`, format = "%d/%m/%Y"))
Xtr$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr$Property_Type)
Xtr$Property_Type <- factor(Xtr$Property_Type,exclude = NULL)
Xtr$Residential_Type <- factor(Xtr$Residential_Type,exclude = NULL)

Xtr$`Date Recorded` <- as.Date(Xtr$`Date Recorded`)

# Aggregate data by month
monthly_data <- aggregate(Xtr$Assessed_Value ~ format(Xtr$`Date Recorded`, "%m"), data = Xtr, FUN = sum)
names(monthly_data) <- c("Month", "Total Value")

# Now, create the plot
ggplot(monthly_data, aes(x = Month, y = `Total Value`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Total Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(randomForest)
model <- randomForest(Y ~ ., data = data.frame(X = Xtr7, Y = Ytr_new$Sale_Amount))
set.seed(123)  # for reproducibility
Xtr_rf <- Xtr[c("Date_Recorded","Assessed_Value","Property_Type","Residential_Type")]

Xtr_rf$Date_Recorded <- as.Date(Xtr_rf$Date_Recorded,format = "%m/%d/%Y")
Xtr_rf$Date <- day(Xtr_rf$Date_Recorded)
Xtr_rf$Month <- month(Xtr_rf$Date_Recorded, label = TRUE)
Xtr_rf$Year <- year(Xtr_rf$Date_Recorded)


Xte_lm_log$Date_Recorded <- as.Date(Xte_lm_log$Date_Recorded,format = "%m/%d/%Y")
Xte_lm_log$Date <- day(Xte_lm_log$Date_Recorded)
Xte_lm_log$Month <- month(Xte_lm_log$Date_Recorded, label = TRUE)
Xte_lm_log$Year <- year(Xte_lm_log$Date_Recorded)

Xte_lm_log$Type <- ifelse(Xte_lm_log$Property_Type != "Residential", Xte_lm_log$Property_Type, Xte_lm_log$Residential_Type)
drop <-c("Date_Recorded","Property_Type","Residential_Type")
Xtr_rf <- Xtr_rf[, !names(Xtr_rf) %in% drop]

Xte_lm_log <- Xte_lm_log[, !names(Xte_lm_log) %in% drop]
training_data2 <- Xtr_rf[!is.na(Xtr_rf$Assessed_Value), ]
prediction_data2 <- Xtr_rf[is.na(Xtr_rf$Assessed_Value), ]
rf_model <- randomForest(Assessed_Value ~ ., data = training_data2, ntree = 500)
training_data3 <- Xtr_rf[Xtr_rf$Property_Type != "", ]
prediction_data3 <- Xtr_rf[Xtr_rf$Property_Type == "", ]

training_data3$Property_Type <- as.factor(training_data3$Property_Type)


k <- 5  # Set the number of neighbors to consider
predicted_property_types <- knn(train = training_data3[, -which(names(training_data3) == "Property_Type")], 
                                test = prediction_data3[, -which(names(prediction_data3) == "Property_Type")], 
                                cl = training_data3$Property_Type, k = k)

# Step 4: Replace empty strings with predicted values
Xtr_rf$Property_Type[Xtr_rf$Property_Type == ""] <- predicted_property_types

# Step 3: Train a Random Forest Classifier
rf_classifier <- randomForest(Property_Type ~ ., data = training_data3, ntree = 500)

# Step 4: Predict missing values
predicted_property_types <- predict(rf_classifier, newdata = prediction_data3)

# Step 5: Replace empty strings with predicted values
Xtr_rf$Property_Type[Xtr_rf$Property_Type == ""] <- predicted_property_types

library(lubridate)

train_index <- sample(1:nrow(Xtr_rf), 0.7*nrow(Xtr_rf))
                      train_data <- Xtr_rf[train_index, ]
                      test_data <- Xtr_rf[-train_index, ]

 
  predicted_values <- predict(rf_model, newdata = prediction_data2)
 
  Xtr_rf$Assessed_Value[is.na(Xtr_rf$Assessed_Value)] <- predicted_values
  
  

Xtr_rf$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr$Property_Type)
Xtr_rf$Property_Type <- as.factor(Xtr_rf$Property_Type)
Xtr_rf$Residential_Type <- as.factor(Xtr_rf$Residential_Type)
Xtr_rf$Type[Xtr_rf$Type == ""] <- NA
Xtr_rf$Type <- as.integer(factor(Xtr_rf$Type))
Xtr_rf$Date <- as.factor(Xtr_rf$Date)
Xtr_rf$Month <- factor(Xtr_rf$Month, ordered = FALSE)
Xtr_rf$Year <- as.factor(Xtr_rf$Year)

Xte_lm_log$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr$Property_Type)
Xtr_lm_log$Type <- ifelse(Xtr$Property_Type != "Residential", Xtr$Property_Type, Xtr$Residential_Type)
Xte_lm_log$Type[Xte_lm_log$Type == ""] <- NA

Xtr_rf$Date <- as.factor(Xtr_rf$Date)
Xtr_rf$Month <- factor(Xtr_rf$Month, ordered = FALSE)
Xtr_rf$Year <- as.factor(Xtr_rf$Year)

Xte_lm_log$Date <- as.factor(Xte_lm_log$Date)
Xte_lm_log$Month <- factor(Xte_lm_log$Month, ordered = FALSE)
Xte_lm_log$Year <- as.factor(Xte_lm_log$Year)
Xte_lm_log$Type <- as.factor(Xte_lm_log$Type)
library(h2o)
h2o.init()
training_data2_h2o <- as.h2o(training_data2)

prediction_data2_h2o <- as.h2o(prediction_data2)
Ytr <- as.h2o(Ytr)


# 2. Train Random Forest Model
rf_model <- h2o.randomForest(y = "Assessed_Value",
                             training_frame = training_data2_h2o,
                             ntrees = 500,
                             max_depth = 20)

# 3. Predict Missing Values
predicted_values <- h2o.predict(rf_model, newdata = prediction_data2_h2o)

predicted_values_vector <- as.vector(predicted_values$predict)

missing_rows <- is.na(Xtr_rf$Assessed_Value)
for (i in 1:length(predicted_values_vector)) {
Xtr_rf$Assessed_Value[missing_rows] <- predicted_values_vector[i]}
h2o.shutdown()

###########################
training_data3 <- Xte_lm_log[!is.na(Xte_lm_log$Assessed_Value), ]
prediction_data3 <- Xte_lm_log[is.na(Xte_lm_log$Assessed_Value), ]

h2o.init()
training_data3_h2o <- as.h2o(training_data3)


prediction_data3_h2o <- as.h2o(prediction_data3)



# 2. Train Random Forest Model
rf_model2 <- h2o.randomForest(y = "Assessed_Value",
                             training_frame = training_data3_h2o,
                             ntrees = 500,
                             max_depth = 20)

# 3. Predict Missing Values
predicted_values2 <- h2o.predict(rf_model2, newdata = prediction_data3_h2o)

predicted_values_vector2 <- as.vector(predicted_values2$predict)

missing_rows2 <- is.na(Xte_lm_log$Assessed_Value)
for (i in 1:length(predicted_values_vector2)) {
  Xte_lm_log$Assessed_Value[missing_rows2] <- predicted_values_vector2[i]}

h2o.shutdown()

#############
Xtr_lm_log <- Xtr_rf[c("Assessed_Value", "Date", "Month", "Year")]
Xte_lm_log <- Xte[c("Date_Recorded","Assessed_Value","Property_Type","Residential_Type")]
Xtr_lm_log$Type <- ifelse(Xtr$Property_Type != "Residential", Xtr$Property_Type, Xtr$Residential_Type)
Xtr_lm_log = mutate(Xtr_lm_log,log10Value = log10(Xtr_lm_log$Assessed_Value))
Xte_lm_log = mutate(Xte_lm_log,log10Value = log10(Xte_lm_log$Assessed_Value))
Xtr_lm_log2 <- Xtr_lm_log$log10Value[!is.infinite(Xtr_lm_log$log10Value)]
Xte_lm_log2 <- Xte_lm_log$log10Value[!is.infinite(Xte_lm_log$log10Value)]

mean_log = mean(c(Xtr_lm_log2,Xte_lm_log2),na.rm=T)

Xtr_lm_log$log10Value[is.na(Xtr_lm_log$log10Value)] = mean_log
Xtr_lm_log$log10Value[is.infinite(Xtr_lm_log$log10Value)] = mean_log
Xte_lm_log$log10Value[is.na(Xte_lm_log$log10Value)] = mean_log
Xte_lm_log$log10Value[is.infinite(Xte_lm_log$log10Value)] = mean_log