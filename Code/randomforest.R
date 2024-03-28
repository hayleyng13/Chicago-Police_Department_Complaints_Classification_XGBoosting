library(gbm)
Xtr_1 <- read.csv("data_mod.csv")
Xtr_2$Town <- as.factor(Xtr_1$Town)
Xtr_2$Month <- as.factor(Xtr_1$Month)
Xtr_2$Year <- as.factor(Xtr_1$Year)
Xtr_2$Type <- as.factor(Xtr_1$Type)
X_test$Town <- as.factor(X_test$Town)
X_test$Month <- as.factor(X_test$Month)
X_test$Year <- as.factor(X_test$Year)
X_test$Type <- as.factor(X_test$Type)
# Initialize the model (regression or classification)
model <- gbm(y~ .^2, data = data.frame(y = Ytr$Sale_Amount, Xtr_1), distribution = "gaussian", n.trees = 100, interaction.depth = 4)
X_test <- read.csv("Xte_1.csv")
# Train the model
# Make predictions
y_pred <- predict(model,  data.frame( X_test))
pred <- read.csv("pred0.csv")
pred$Sale_Amount <- y_pred
write.table(pred1, file = "pred1.csv", quote=F, row.names=F, sep = ",")

#############################
Xtr_2 = mutate(Xtr_2,log10Value = log10(Xtr_2$Assessed_Value))
Xte_2 = mutate(Xte_2,log10Value = log10(Xte_2$Assessed_Value))
Xtr_lm_log2 <- Xtr_2$log10Value[!is.infinite(Xtr_2$log10Value)]
Xte_lm_log2 <- Xte_2$log10Value[!is.infinite(Xte_2$log10Value)]
mean_log = mean(c(Xtr_lm_log2,Xte_lm_log2),na.rm=T)


Xtr_2$log10Value[is.infinite(Xtr_2$log10Value)] = mean_log

Xte_2$log10Value[is.infinite(Xte_2$log10Value)] = mean_log
Xtr_2 <- subset(Xtr_2, select = -log10Value)
Xte_2 <- subset(Xte_2, select = -Assessed_Value)
Xtr_2$Town <- as.character(Xtr_2$Town)
Xtr_2$Town[Xtr_2$Town %in% rare_categories] <- "Others"
Xte_2$Town <- as.character(Xte_2$Town)
Xte_2$Town[Xte_2$Town %in% rare_categories] <- "Others"
Xte_2$Town <- as.factor(Xte_2$Town)
Xtr_2$Add <- Xtr$Address
Xte_2$Add <- Xte$Address

Xtr_2$Add[Xtr_2$Add %in% rare_add] <- "Others"

Xte_2$Add[Xte_2$Add %in% rare_add] <- "Others"
X_test$Add <- Xte_2$Add
Xtr_2$Add <- as.factor(Xtr_2$Add)
Xte_2$Add <- as.factor(Xte_2$Add)
X_test$Town <- Xte_2$Town
# Convert Xte_2$Add to a factor with levels from Xtr_2$Add
Xte_2$Add <- factor(Xte_2$Add, levels = levels(Xtr_2$Add))

library(h2o)
h2o.init()
training_data <- cbind(Xtr_2,"Assessed_Value"=Xtr_1$Assessed_Value,Ytr$Sale_Amount)
prediction_data <- X_test
training_data_h2o <- as.h2o(training_data)

prediction_data_h2o <- as.h2o(prediction_data)

rf_model <- h2o.randomForest(y = "Ytr$Sale_Amount",
                             training_frame = training_data_h2o,
                             ntrees = 500,
                             max_depth = 20)

predicted_values <- h2o.predict(rf_model, newdata = prediction_data_h2o)

predicted_values_vector <- as.vector(predicted_values$predict)
pred1=pred
pred1$Sale_Amount <- predicted_values_vector
