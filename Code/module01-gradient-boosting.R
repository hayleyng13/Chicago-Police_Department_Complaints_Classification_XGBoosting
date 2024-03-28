library(caret)
library(dplyr)


setwd("~/Desktop/STAT 440/module01")

Xtr2_data = read.csv("Xtr.csv", header = TRUE)

#Xtr_data = read.csv("imputed_Xtr.csv", header = TRUE)
Xtr_data = read.csv("Xtr.csv", header = TRUE)
Ytr_data = read.csv("Ytr.csv", header = TRUE)
Xte_data = read.csv("Xte.csv", header = TRUE)

# quick conversions 
Xtr_data$ID = as.integer(Xtr_data$ID)
Xtr_data$List_Year = as.integer(Xtr_data$List_Year)
Xtr_data$List_Year = as.integer(Xtr_data$List_Year)
Xtr_data$Assessed_Value = as.numeric(Xtr_data$Assessed_Value)


# sapply(Xtr_data, class)
# sapply(Xtr2_data, class)


# all the true training data
all_train_data = merge(Xtr_data, Ytr_data, by = "ID")

# creating the train/validation/test split

# p is the % of data for the training set
ind = createDataPartition(all_train_data$Sale_Amount, p = 0.75, list = FALSE)

train_data = all_train_data[ind, ]
valid_data = all_train_data[-ind, ]


# weird fix I had to do
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
valid_data = rbind(train_data[1, ], valid_data)
valid_data = valid_data [-1, ]

Xte_data = rbind(subset(train_data[1, ], select = -c(Sale_Amount)), Xte_data)
Xte_data = Xte_data[-1, ]


# Data Preprocessing + Feature Engineering

# missingDateRecorded = function(df) {
#   median_date = median(df$Date_Recorded, na.rm = TRUE)
#   df$Date_Recorded[is.na(df$Date_Recorded)] = median_date
#   return(df)
# }

missingDateRecorded = function(df) {
  if (any(is.na(df$Date_Recorded))) {
    median_date = median(df$Date_Recorded, na.rm = TRUE)
    df$Date_Recorded[is.na(df$Date_Recorded)] = median_date
    return(df)
  }
  return(df)
}

convertToSeason = function(month) {
  if (!is.na(month)) { # if there is no month
    if (month == "12" | month == "01" | month == "02") {
      season = "winter"
    }
    else if (month == "03" | month == "04" | month == "05") {
      season = "spring"
    }
    else if (month == "06" | month == "07" | month == "08") {
      season = "summer"
    }
    else if (month == "09" | month == "10" | month == "11") {
      season = "fall"
    }
    return(season)
  }
  return("")
}

# converts variables to factors, creates new variables through feature engineering and log transformation and mean imputation for the log transformation
# inefficient way but it works
processData = function(df) {
  
  df$Date_Recorded = as.Date(df$Date_Recorded, format = "%m/%d/%Y")
  df = missingDateRecorded(df)
  
  df = df %>% 
    mutate(Date_Recorded_Since = as.numeric(as.POSIXct(Date_Recorded, format = "%Y-%m-%d")))
  
  df$Year_Recorded = format(as.Date(df$Date_Recorded, format = "%m/%d/%Y"), "%Y")
  df$Year_Recorded = factor(df$Year_Recorded)
  
  df$Years_On_Market = as.numeric(format(as.Date(df$Date_Recorded, format = "%m/%d/%Y"), "%Y")) - df$List_Year 
  
  df$Month_Recorded = format(as.Date(df$Date_Recorded, format = "%m/%d/%Y"), "%m")
  df$Month_Recorded = factor(df$Month_Recorded)
  
  df = df %>% 
    mutate(Season_Recorded = sapply(Month_Recorded, convertToSeason))
  df$Season_Recorded = factor(df$Season_Recorded)
  
  df$List_Year = factor(df$List_Year)
  df$Residential_Type = factor(df$Residential_Type)
  df$Property_Type = factor(df$Property_Type)
  df$Town = factor(df$Town)
  df$Address = factor(df$Address)
  
  log transformation + mean imputation
  df$Assessed_Value[is.na(df$Assessed_Value)] = mean(df$Assessed_Value, na.rm = TRUE)
  df$Assessed_Value[(df$Assessed_Value) == 0] = mean(df$Assessed_Value, na.rm = TRUE)
  df$log10_Assessed_Value = log10(df$Assessed_Value)
  
  return(df)
}

# train_data2 = train_data
# train_data2 = processData(all_train_data)
# head(subset(train_data2, select = -c(ID, Date_Recorded, Sale_Amount)), n = 6)

train_data = processData(train_data)
valid_data = processData(valid_data)
Xte_data = processData(Xte_data)

# also need to try a variant of gradient boosting with getting rid of date recorded and address?


# gradient boosting

library(h2o)
library(ranger)
library(caret)
library(vip)

# remove ID from training set and validation set
train_data = subset(train_data, select = -c(Date_Recorded, ID))
valid_data = subset(valid_data, select = -c(Date_Recorded, ID))


tr_x = names(subset(train_data, select = -c(Sale_Amount)))
tr_y = names(subset(train_data, select = c(Sale_Amount)))


library(h2o)

h2o.no_progress()

h2o.init(max_mem_size = "5g")

data_train_h2o = as.h2o(train_data)
data_valid_h2o = as.h2o(valid_data)

# ---------------------- NOW FITTING MY OG AGAIN (remove date recorded)

#data_train = subset(data_train, select = -c(Date_Recorded, Address))
#data_valid_x = subset(data_valid, select = -c(Date_Recorded, Sale_Amount, Address))

hyperparam_grid_og_h2o = list(
  #ntrees = c(500, 1000, 2500, 5000, 10000)
  #ntrees = c(1000, 1250, 1500, 1600, 2000)
  
  max_depth = c(4, 6, 8), #seq(1, 20, 1),
  #max_depth = c(4, 5),
  #min_rows = ranges from 5 to 15
  min_rows = c(5, 8, 9, 11, 12),
  sample_rate = c(0.6, 0.7, 0.8),
  col_sample_rate = c(0.6, 0.7, 0.8)
  #learn_rate = c(0.01)
  
)

search_criteria = list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001, 
  stopping_rounds = 10, 
  max_runtime_secs = 3600 
)


h2o_gbm_og = h2o.grid(x = tr_x,
                      y = tr_y,
                      training_frame = data_train_h2o,
                      #validation_frame = data_valid_h2o,
                      hyper_params = hyperparam_grid_og_h2o,
                      algorithm = "gbm",
                      grid_id = "gbm_grid_og",
                      #ntrees = 10000, # go down to 9000 perhaps? with larger nodes it fails
                      ntrees = 1300,
                      seed = 123,
                      nfolds = 5,
                      learn_rate = 0.01,
                      #search_criteria = list(strategy = "Cartesian"),
                      search_criteria = search_criteria
)

grid_perf_gbm_og = h2o.getGrid(
  grid_id = "gbm_grid_og",
  sort_by = "rmse",
  decreasing = FALSE
)

best_model_og_id = grid_perf_gbm_og@model_ids[[1]]
best_model_og = h2o.getModel(best_model_og_id)

h2o_gbm_perf_og = h2o.performance(model = best_model_og, data_valid_h2o)
h2o.rmse(h2o_gbm_perf_og)


# NOW... refit on ALL the data
# then submit to kaggle

# fitting the model with the best hyperparameters
all_data_fixed = rbind(train_data, valid_data)

# have to do my special little thing!
all_data_fixed = rbind(train_data[1, ], all_data_fixed)
all_data_fixed = all_data_fixed[-1, ]
all_data_fixed_h2o = as.h2o(all_data_fixed)

final_model = do.call(h2o.gbm, 
                      {
                        p <- best_model_og@parameters
                        p$model_id = NULL
                        p$training_frame = all_data_fixed_h2o
                        p$validation_frame = NULL
                        p$nfolds = 10
                        p
                      })

final_model@model$cross_validation_metrics_summary

# PREDICTING ON KAGGLE
gbm_og_preds = h2o.predict(final_model, newdata = as.h2o(subset(Xte_data, select = -c(ID, Date_Recorded))))


# gbm_og_preds = gbm_og_preds[1:796398] from missing values, code above fixes it 
m = as.data.frame(gbm_og_preds)

Xte_data$Sale_Amount = m[["predict"]]
gbm_og_tuned = subset(Xte_data, select = c(ID, Sale_Amount))
write.table(gbm_og_tuned , file = "model3_gbm_tuned.csv", quote = F, row.names = F, sep = ",")


# ------------- trying with the random forest imputed assessed value

library(h2o)

h2o.no_progress()

h2o.init(max_mem_size = "5g")

data_train_h2o = as.h2o(train_data)
data_valid_h2o = as.h2o(valid_data)

hyperparam_grid2_h2o = list(
 # max_depth = c(2, 4, 6, 8, 10, 12), #seq(1, 20, 1),
  
  max_depth = c(4, 5, 6),
  
  # #max_depth = c(4, 5),
  # #min_rows = ranges from 5 to 15
  
  min_rows = c(5, 8, 9, 12),
  sample_rate = c(0.6, 0.7, 0.8),
  col_sample_rate = c(0.6, 0.7, 0.8)
  
  #ntrees = c(1000, 1750, 1900, 2000, 2500)
  # learn_rate = c(0.01, 0.05, 0.1)

)

search_criteria = list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,
  #max_models = 5000,
  stopping_rounds = 10, # over the last 10 models
  max_runtime_secs = 1200 # stops searching after X seconds
)

h2o_gbm2 = h2o.grid(x = tr_x,
                    y = tr_y,
                    training_frame = data_train_h2o,
                    #validation_frame = data_valid_h2o,
                    hyper_params = hyperparam_grid2_h2o,
                    algorithm = "gbm",
                    grid_id = "gbm_grid2",
                    #ntrees = 10000, # go down to 9000 perhaps? with larger nodes it fails
                  #  ntrees = 5000,
                    ntrees = 1750, # 400 # then * 5 since reducing learn rate from 0.05 to 0.01
                    seed = 123,
                    nfolds = 5,
                    learn_rate = 0.01,
                    #learn_rate = 0.1, 
                    #search_criteria = list(strategy = "Cartesian")
                    search_criteria = search_criteria
)


grid_perf_gbm2 = h2o.getGrid(
  grid_id = "gbm_grid2",
  sort_by = "rmse",
  decreasing = FALSE
)



best_model2_id = grid_perf_gbm2@model_ids[[1]]
best_model2 = h2o.getModel(best_model2_id)

h2o_gbm_perf_2 = h2o.performance(model = best_model2, data_valid_h2o)
h2o.rmse(h2o_gbm_perf_2)

# ----------------------


hyperparam_grid1_h2o = list(
  #max_depth = c(2, 4, 6, 8, 10, 12, 14, 16, 18), #seq(1, 20, 1),
  max_depth = c(4, 5),
  #min_rows = ranges from 5 to 15
  min_rows = c(5, 8, 11),
  sample_rate = c(0.7, 0.8, 1),
  learn_rate = c(0.01)

)


search_criteria = list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001, 
  stopping_rounds = 10, 
  max_runtime_secs = 3600 
)


h2o_gbm1 = h2o.grid(x = tr_x,
                    y = tr_y,
                    training_frame = data_train_h2o,
                    #validation_frame = data_valid_h2o,
                    hyper_params = hyperparam_grid1_h2o,
                    algorithm = "gbm",
                    grid_id = "gbm_grid1",
                    ntrees = 10000, # go down to 9000 perhaps? with larger nodes it fails
                    seed = 123,
                    nfolds = 5,
                    # learn_rate = 0.01,
                    #search_criteria = list(strategy = "Cartesian"),
                    search_criteria = search_criteria
)

grid_perf_gbm1 = h2o.getGrid(
  grid_id = "gbm_grid1",
  sort_by = "rmse",
  decreasing = FALSE
)


# best_model_id = grid_perf_gbm1@model_ids[[17]]
# best_model = h2o.getModel(best_model_id)

# max depth of 4 or 5 or 8 or 6

# best_model1_id = grid_perf_gbm1@model_ids[[1]]
# best_model1 = h2o.getModel(best_model1_id)



#4, 5, 6, 7, 8,  with smallest learning rate

# from gbm 3 (and others)... learned that 5, 7, or 3 seem to be pretty good consistently 


h2o.shutdown()





# referenced: https://rpubs.com/DRMORRIS/874470





