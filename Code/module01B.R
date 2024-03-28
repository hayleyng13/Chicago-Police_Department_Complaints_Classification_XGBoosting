Xtr2 = Xtr1
Xte2 = Xte1
mu = mean(c(Xtr2$Assessed_Value, Xte2$Assessed_Value), na.rm = T)

# pred1 - Imputation by filling in mean for assessed value
Xtr2$Assessed_Value[is.na(Xtr2$Assessed_Value)] = mu
Xte2$Assessed_Value[is.na(Xte2$Assessed_Value)] = mu

pred0 = read.csv("pred0.csv")
pred1 = pred0

model = lm(y ~ ., data = data.frame(X = Xtr2, y = Ytr$Sale_Amount))
pred1$Sale_Amount = predict(model, data.frame(X = Xte2))
write.table(pred1, file = "pred1.csv", quote=F, row.names=F, sep = ",")

# pred2 - Imputation by filling in mean by year for assessed value

Xtr3 = Xtr1
Xte3 = Xte1
years = c(Xtr3$List_Year, Xte3$List_Year)
values = c(Xtr2$Assessed_Value, Xte2$Assessed_Value)
for (year in unique(years)) {
  mu = mean(values[years == year], na.rm = T)
  mask = Xtr3$List_Year == year & is.na(Xtr3$Assessed_Value)
  Xtr3$Assessed_Value[mask] = mu
  mask = Xte3$List_Year == year & is.na(Xte3$Assessed_Value)
  Xte3$Assessed_Value[mask] = mu
}

pred2 = pred0

model = lm(y ~ ., data = data.frame(X = Xtr3, y = Ytr$Sale_Amount))
pred2$Sale_Amount = predict(model, data.frame(X = Xte3))
write.table(pred2, file = "pred2.csv", quote=F, row.names=F, sep = ",")



pred3 = pred0
Xtr4 = Xtr1_new
Xte4=Xte1_new
Xtr4$Assessed_Value[is.na(Xtr4$Assessed_Value)] = mu
Xte4$Assessed_Value[is.na(Xte4$Assessed_Value)] = mu
Xtr4 = mutate(Xtr4,log10Value = log10(Xtr4$Assessed_Value))
Xte4 = mutate(Xte4,log10Value = log10(Xte4$Assessed_Value))

Xtr4 <- Xtr4[Reduce(`&`, lapply(Xtr4, is.finite)),]


xte4_2 <- Xte4$log10Value[!is.infinite(Xte4$log10Value)]


mean_Xte = mean(xte4_2,na.rm=T)
Xte4$log10Value[is.na(Xte4$log10Value)] = mean_Xte
Xte4$log10Value[is.infinite(Xte4$log10Value)] = mean_Xte
Ytr_new <- merge(Xtr4, Ytr, by = "ID")

model = lm(y~.,data = data.frame(X = Xtr4$log10Value, y = Ytr_new$Sale_Amount))


pred3$Sale_Amount = predict(model, data.frame(X = Xte4$log10Value))
pred3[,"ID"] = pred0[,"ID"]
pred_pos = pred3[pred3$Sale_Amount > 0,]

pred_mean = mean(pred_pos$Sale_Amount)

v <- pred3[pred3$Sale_Amount < 0,]

v$Sale_Amount = pred_mean
pred3$Sale_Amount[pred3$Sale_Amount < 0] <- pred_mean
pred3_new <- merge(v,pred3, by = "ID")


write.table(pred3, file = "pred3.csv", quote=F, row.names=F, sep = ",")


Xtr5 = Xtr_new2
Xtr5$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr5$Property_Type)
Xtr5$Property_Type <- factor(Xtr5$Property_Type,exclude = NULL)



summary(Xtr5$Property_Type)





pred4 = pred0

Xte6 = Xte_new2
Xte6$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xte6$Property_Type)
Xte6$Property_Type <- factor(Xte6$Property_Type,exclude = NULL)

Xtr5$Assessed_Value[is.na(Xtr2$Assessed_Value)] = mu
Xte6$Assessed_Value[is.na(Xte2$Assessed_Value)] = mu


model = lm(y ~ ., data = data.frame(X = Xtr5, y = Ytr$Sale_Amount))
pred4$Sale_Amount = predict(model, data.frame(X = Xte6))
write.table(pred4, file = "pred4.csv", quote=F, row.names=F, sep = ",")
levels_df <- levels(Xtr5$Property_Type)

# Set the levels of new_df$Property_Type to match those of df$residential_type
Xte6$Property_Type <- factor(Xte6$Property_Type, levels = levels_df)

#######################


pred6 = pred0
Xtr7 = Xtr_new2
Xte5 = Xte_new2
Xtr7$Assessed_Value[is.na(Xtr7$Assessed_Value)] = mu
Xte5$Assessed_Value[is.na(Xte5$Assessed_Value)] = mu
Xtr7$Assessed_Value = Xtr3$Assessed_Value
Xte5$Assessed_Value = Xte3$Assessed_Value
Xtr7 = mutate(Xtr7,log10Value = log10(Xtr7$Assessed_Value))
Xte5 = mutate(Xte5,log10Value = log10(Xte5$Assessed_Value))


Xte5_2 <- Xte5$log10Value[!is.infinite(Xte5$log10Value)]
Xtr5_2 <- Xtr7$log10Value[!is.infinite(Xtr7$log10Value)]

mean_Xte = mean(c(Xte5_2,Xtr5_2),na.rm=T)
Xte5$log10Value[is.na(Xte5$log10Value)] = mean_Xte
Xte5$log10Value[is.infinite(Xte5$log10Value)] = mean_Xte
Xtr7$log10Value[is.na(Xtr7$log10Value)] = mean_Xte
Xtr7$log10Value[is.infinite(Xtr7$log10Value)] = mean_Xte

Xtr7$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr7$Property_Type)
Xtr7$Property_Type <- factor(Xtr7$Property_Type,exclude = NULL)
Xtr7$Residential_Type <- factor(Xtr7$Residential_Type,exclude = NULL)

Xte5$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xte5$Property_Type)
Xte5$Property_Type <- factor(Xte5$Property_Type,exclude = NULL)
Xte5$Residential_Type <- factor(Xte5$Residential_Type,exclude = NULL)
Ytr_new <- merge(Xtr7, Ytr, by = "ID")

model = lm(y~.^2,data = data.frame(X = Xtr7, y = Ytr_new$Sale_Amount))

pred7 = pred0
pred7$Sale_Amount = predict(model, data.frame(X = Xte5))
#pred3[,"ID"] = pred0[,"ID"]
pred_pos2 = pred6[pred6$Sale_Amount > 0,]

pred_mean2 = mean(pred_pos2$Sale_Amount)

v2 <- pred6[pred6$Sale_Amount < 0,]

v2$Sale_Amount = pred_mean2
pred6$Sale_Amount[pred6$Sale_Amount < 0] <- pred_mean2

# pred5_new <- merge(v2,pred5, by = "ID")

pred6 = pred0
write.table(pred7, file = "pred7.csv", quote=F, row.names=F, sep = ",")

model = lm(y ~ ., data = data.frame(X = Xtr_mod, y = Ytr$Sale_Amount))
library(forecast)
pred6$Sale_Amount = predict(model, data.frame(X = Xte7))
Xtr9 = Xtr[, c("Date Recorded", "Assessed_Value")]
fit <- auto.arima (Xtr9)
