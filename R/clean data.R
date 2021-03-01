








##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################

# Load libraries
library("keras")
library("lime")
library("tidyquant")
library("rsample")
library("recipes")
library("yardstick")
library("corrr")
library("skimr")
library("here")
library("janitor")
library("lubridate")
library("caret")
library("purrr")
library("pROC")
library("tidymodels")
library("tidyverse")



##############################
##      Load Saved Data     ## --------------------------------------------------------------------------
##############################

# load organization data
org_subscription_events <- read_csv(here("data/org subscription events.csv"),
                                    col_types = cols(X1 = col_skip())) %>%
  mutate(day_of = ymd(day_of))

# view data
glimpse(org_subscription_events)

one_sample <- org_subscription_events %>%
  filter(organization_id == "d87d5d0c78b8400236dbab698044c5e3")

## each org has a subscription event...
#' Starts with New Business
#'
#' 1. New Business
#' 2. Expansion == if they add more seats to the contract
#' 3. Contractions == if they decrease seats to the contract
#' 4. Churn == They stop (0 seats)
#'


skim(org_subscription_events)

# load product usage data
product_usage <- read_csv(here("data/product usage.csv"),
                          col_types = cols(X1 = col_skip())) %>%
  mutate(day_of = ymd(day_of))

# view data
glimpse(product_usage)

## each org uses the product differently.
#' Meeting types include
#' 1. Group Meetings (11.74%)
#' 2. Pooled Meetings (43.51%)
#' 3. Prof Feature Meeting (8%)
#' 4. Web Conference Tool Meeting (100%)
#' 5. Total Meetings (I'm guessing Other meetings - 2.57)
summary(product_usage)



## Get Labeled Churned/Active Orgs
churn_org <- org_subscription_events %>%
  mutate(
    Churn = ifelse(arr_event_type == "Churn", 1, 0)
  ) %>%
  group_by(organization_id) %>%
  summarise(Churn = max(Churn)) %>%
  mutate(Churn_label = if_else(Churn == 1, "Churn", "Active"))


## Get Plan for orgs and dates (starting date and 1 month later)
org_plans <- org_subscription_events %>%
  group_by(organization_id) %>%
  summarise(
    first_plan_name = first(plan_name),
    first_seats = first(seats),
    first_arr_amount = first(arr_amount),
    start_date = first(day_of)
  ) %>%
  mutate(one_month = start_date + 31)



#' I need to determine what the usage was for orgs before they churned
#' Some companies churned multiple times so I am matching up their usage before each churn
#' First I will determine the point each org churned then join with usage data
#' I will filter out any usage that occurs after the Churn Date
#'

### Churn Data
churn_data <- org_subscription_events %>%
  filter(arr_event_type == "Churn") %>%
  select(
    organization_id,
    arr_event_type,
    day_of
  ) %>%
  rename(Churn = arr_event_type,
         Churn_date = day_of)

summary(churn_data)


## Product Usage and Churn Data
#' Remove all activity after 1 month of signing up for the product
churn_usage <- product_usage %>%
  left_join(churn_data, by = "organization_id") %>%
  left_join(org_plans, by = "organization_id") %>%
  mutate(Churn = if_else(is.na(Churn), "Active", Churn)) %>%
  group_by(organization_id) %>%
  filter(day_of <= one_month,
         day_of >= start_date) %>%
  rename(Usage_date = day_of)
summary(churn_usage)
glimpse(churn_usage)



## Determine how long it took for people to churn?
churn_behavior <- churn_usage %>%
  filter(Churn == "Churn") %>%
  group_by(organization_id, Churn, End_date) %>%
  summarise(
    days_used = n(),
    start_date = min(Usage_date),
    end_date = max(End_date),
    time_active = as.numeric(end_date - start_date),
    usage_rate = days_used / time_active,
    total_meetings = sum(total_meetings),
    group_meetings = sum(group_meetings),
    pooled_meetings = sum(pooled_meetings),
    prof_feature_meetings = sum(prof_feature_meetings),
    web_conf_tool_meetings = sum(web_conf_tool_meetings),
    avg_meetings = total_meetings / days_used,
    avg_group_meetings = group_meetings / days_used,
    avg_pooled_meetings = pooled_meetings / days_used,
    avg_prof_meetings = prof_feature_meetings / days_used,
    avg_conf_meetings = web_conf_tool_meetings / days_used,
    meetings_per_day = total_meetings / time_active,
    .groups = "drop"
  )

summary(churn_behavior)




# 32 orgs churned now
churn_usage %>%
  filter(Churn == "Churn") %>%
  distinct(organization_id, End_date) %>%
  nrow()


str(churn_usage)

## All Behavior
usage_group <- churn_usage %>%
  group_by(organization_id, Churn) %>%
  summarise(
    days_used = n(),
    start_date = min(start_date),
    end_date = first(one_month),
    time_active = as.numeric(end_date - start_date),
    usage_rate = days_used / time_active,
    total_meetings = sum(total_meetings),
    group_meetings = sum(group_meetings),
    pooled_meetings = sum(pooled_meetings),
    prof_feature_meetings = sum(prof_feature_meetings),
    web_conf_tool_meetings = sum(web_conf_tool_meetings),
    avg_meetings = total_meetings / days_used,
    avg_group_meetings = group_meetings / days_used,
    avg_pooled_meetings = pooled_meetings / days_used,
    avg_prof_meetings = prof_feature_meetings / days_used,
    avg_conf_meetings = web_conf_tool_meetings / days_used,
    meetings_per_day = total_meetings / time_active,
    .groups = "drop"
  ) %>%
  mutate(usage_rate = if_else(usage_rate >= 1, 1, usage_rate))

tabyl(usage_group$Churn)
##############################
##       Join All Data      ## --------------------------------------------------------------------------
##############################



#### FINAL TRAINING DATA
final_data <- churn_org %>%
  left_join(org_plans, by = "organization_id") %>%
  left_join(usage_group, by = c("organization_id", "Churn_label" = "Churn")) %>%
  select(
    Churn_label,
    first_plan_name,
    first_seats,
    first_arr_amount,
    usage_rate,
    avg_meetings,
    avg_group_meetings,
    avg_pooled_meetings,
    avg_prof_meetings,
    avg_conf_meetings,
    meetings_per_day
  ) %>%
  mutate(
    first_seats = if_else(first_seats == 0, 1, first_seats), # 2 orgs first event was churn, we'll assume they started with just 1 seat
    first_arr_amount = if_else(first_arr_amount == 0, 120, first_arr_amount), # those 2 orgs were on premium monthly so the price should be 120
    price = first_arr_amount / first_seats,
    seats = if_else(first_seats > 1, 0, 1) # Dummy Var. 1 Seat vs Multiple Seats
  ) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate_if(is.numeric, funs(ifelse(. == Inf, 1, .))) %>%
  rename(Churn = Churn_label,
         plan_name = first_plan_name) %>%
  select(-c(first_arr_amount, first_seats))

# Churn by plan name
final_data %>%
  tabyl(plan_name, Churn) %>%
  adorn_percentages() %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


# Create Dummy variables for Plan Name
final_data <-
  fastDummies::dummy_columns(final_data,
                             select_columns = "plan_name",
                             remove_selected_columns = TRUE)

## Churn as Factor for Model
final_data$Churn <- factor(final_data$Churn, levels = c("Churn", "Active"))
summary(final_data)

skim(final_data)

final_data %>%
  tabyl(Churn)

final_data %>%
  tabyl(price)


final_data %>%
  tabyl(price, Churn) %>%
  adorn_percentages() %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()



hist(final_data$price)
hist(log(final_data$price))
hist()
### Feature Engineering
# Determine if log transformation improves correlation
# between TotalCharges and Churn
final_data %>%
  select(Churn, price) %>%
  mutate(
    Churn = Churn  %>% as.numeric(),
    LogPrice = log(price)
  ) %>%
  correlate() %>%
  corrr::focus(Churn) %>%
  fashion()
## Log Price is slightly better to use

## Log Usage Rates?
final_data %>%
  select(Churn, usage_rate) %>%
  mutate(
    Churn = Churn  %>% as.numeric(),
    LogPrice = log(usage_rate)
  ) %>%
  correlate() %>%
  corrr::focus(Churn) %>%
  fashion()
## Log Price is slightly better to use
log(final_data$usage_rate)

model_data <- bake(rec_obj, new_data = final_data)# %>% select(-Churn)
str(model_data)


# Feature correlations to Churn
corrr_analysis <- model_data %>%
  mutate(Churn = if_else(Churn == "Churn", 1, 0)) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = term) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature))
corrr_analysis

# a data frame with all the annotation info
good_bad_anno <- data.frame(
  x = c(-0.2, 0.2),
  y = c(9.25, 9.25),
  label = c("Good", "Bad")
)

# Correlation visualization
corrr_analysis %>%
  ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
  geom_point() +
  # Positive Correlations - Contribute to churn
  geom_segment(aes(xend = 0, yend = feature),
               color = palette_light()[[2]],
               data = corrr_analysis %>% filter(Churn > 0)) +
  geom_point(color = palette_light()[[2]],
             data = corrr_analysis %>% filter(Churn > 0)) +
  # Negative Correlations - Prevent churn
  geom_segment(aes(xend = 0, yend = feature),
               color = palette_light()[[1]],
               data = corrr_analysis %>% filter(Churn < 0)) +
  geom_point(color = palette_light()[[1]],
             data = corrr_analysis %>% filter(Churn < 0)) +
  # Vertical lines
  geom_vline(
    xintercept = 0,
    color = palette_light()[[5]],
    size = 1,
    linetype = 2
  ) +
  geom_vline(
    xintercept = -0.25,
    color = palette_light()[[5]],
    size = 1,
    linetype = 2
  ) +
  geom_vline(
    xintercept = 0.25,
    color = palette_light()[[5]],
    size = 1,
    linetype = 2
  ) +
  # Add arrow
  annotate(
    "segment",
    x = 0,
    xend = 0.15,
    y = 9.25,
    yend = 9.25,
    colour = "red",
    size = 1,
    alpha = 1,
    arrow = arrow()
  ) +
  # Add arrow
  annotate(
    "segment",
    x = 0,
    xend = -0.15,
    y = 9.25,
    yend = 9.25,
    colour = "blue",
    size = 1,
    alpha = 1,
    arrow = arrow()
  ) +
  geom_text(
    data = good_bad_anno,
    aes(x = x, y = y, label = label),
    color = c("blue", "red"),
    size = 4,
    fontface = "bold"
  ) +
  # Aesthetics
  theme_tq() +
  labs(
    title = "Churn Correlation Analysis",
    subtitle = paste(
      "Negative Correlations (prevent churn),",
      "Positive Correlations (contribute to churn)"
    ),
    y = "Feature Importance"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),
  )



str(final_data)

##############################
##       Tidy Modeling      ## --------------------------------------------------------------------------
##############################


## split data 75% training 25% testing
set.seed(1234)
data_split <- initial_split(final_data, prop = 3/4)
train <- training(data_split)
test <- testing(data_split)

# Create recipe
rec_obj <-
  recipe(
    Churn ~ `plan_name_premium-monthly` + `plan_name_pro-monthly` + `plan_name_premium-yearly` + `plan_name_pro-yearly` + price + meetings_per_day + usage_rate + avg_meetings + seats,
    data = train
  ) %>%
  # step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(price) %>%
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train)


rec_obj

# Predictors
x_train <- bake(rec_obj, new_data = train) %>% select(-Churn)
x_test  <- bake(rec_obj, new_data = test) %>% select(-Churn)
glimpse(x_train)

# Response variables for training and testing sets
y_train <- ifelse(pull(train, Churn) == "Churn", 1, 0)
y_test  <- ifelse(pull(test, Churn) == "Churn", 1, 0)
glimpse(y_train)


# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>%

  # First hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu",
    input_shape        = ncol(x_train)) %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Second hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu") %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Output layer
  layer_dense(
    units              = 1,
    kernel_initializer = "uniform",
    activation         = "sigmoid") %>%

  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

model_keras

# Fit the keras model to the training data
history <- fit(
  object           = model_keras,
  x                = as.matrix(x_train),
  y                = y_train,
  batch_size       = 53,
  epochs           = 50,
  validation_split = 0.35
)

# Print a summary of the training history
print(history)

# Plot the training/validation history of our Keras model
plot(history)

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test)) %>%
  as.vector()

# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test) %>% fct_recode(Churn = "1", Active = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(Churn = "1", Active = "0"),
  class_prob = round(yhat_keras_prob_vec, 6)
)
str(estimates_keras_tbl)

estimates_keras_tbl$truth <- factor(estimates_keras_tbl$truth, levels = c("Churn", "Active"))
estimates_keras_tbl$estimate <- factor(estimates_keras_tbl$estimate, levels = c("Churn", "Active"))


estimates_keras_tbl

options(yardstick.event_first = TRUE)
options(yardstick.event_first = FALSE)
# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# Accuracy
estimates_keras_tbl %>% metrics(truth, estimate) %>% filter(.metric == "accuracy") %>% pull(.estimate)
estimates_keras_tbl %>% metrics(truth, estimate) %>% filter(.metric == "kap") %>% pull(.estimate)


# AUC
estimates_keras_tbl %>% roc_auc(truth, class_prob) %>% pull(.estimate)

# All Metrics
tibble(
  AUC       = estimates_keras_tbl %>% roc_auc(truth, class_prob) %>% pull(.estimate),
  Accuracy  = estimates_keras_tbl %>% metrics(truth, estimate) %>% filter(.metric == "accuracy") %>% pull(.estimate),
  Kappa     = estimates_keras_tbl %>% metrics(truth, estimate) %>% filter(.metric == "kap") %>% pull(.estimate),
  Precision = precision_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate),
  Recall    = recall_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate),
  F_Score   = f_meas_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)
)

precision_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)
recall_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)
yardstick::f_meas_vec(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)

##############################
##      Caret Modeling      ## --------------------------------------------------------------------------
##############################

library(caret)


##### preprocessing
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE)
seed <- 123
metric <- "Accuracy"
preProcess <- c("center", "scale")


## split data 75% training 25% testing
set.seed(1234)
data_split <- initial_split(final_data, prop = 3/4)
train <- training(data_split)
test <- testing(data_split)



tabyl(final_data$Churn)
dim(train)
dim(test)
summary(train)
summary(test)


summary(train)

# fit.glm$coefnames
summary(fit.glm)
##### 3 different models
# Logistic Regression

str(train)

set.seed(seed)
fit.glm <-
  train(
    Churn ~ .,
    data = train,
    method = "glm",
    metric = metric,
    preProc = preProcess,
    trControl = control
  )

set.seed(seed)
fit.glmnet <-
  train(
    Churn ~ .,
    data = train,
    method = "glmnet",
    metric = metric,
    preProc = preProcess,
    trControl = control
  )

set.seed(seed)
fit.rf <-
  train(
    Churn ~ .,
    data = train,
    method = "rf",
    metric = metric,
    preProc = preProcess,
    trControl = control
  )


set.seed(seed)
fit.gbm <-
  train(
    Churn ~ .,
    data = train,
    method = "gbm",
    metric = metric,
    preProc = preProcess,
    trControl = control
  )


summary(fit.glm)
varImp(fit.glm)

summary(fit.glmnet)
varImp(fit.glmnet)

summary(fit.rf)
varImp(fit.rf)

summary(fit.gbm)
varImp(fit.gbm)
##
# logistic
test$pred_lm <- predict(fit.glm, newdata = test)
test$prob_lm <- predict(fit.glm, newdata = test, type = "prob")[[1]]

# GLMNET
test$pred_glmnet <- predict(fit.glmnet, newdata = test)
test$prob_glmnet <- predict(fit.glmnet, newdata = test, type = "prob")[[1]]

# Random
test$pred_rf <- predict(fit.rf, newdata = test)
test$prob_rf <- predict(fit.rf, newdata = test, type = "prob")[[1]]

# Boosted
test$pred_gbm <- predict(fit.gbm, newdata = test)
test$prob_gbm <- predict(fit.gbm, newdata = test, type = "prob")[[1]]


# logistic
final_data$pred_lm <- predict(fit.glm, newdata = final_data)
final_data$prob_lm <- predict(fit.glm, newdata = final_data, type = "prob")[[1]]

# GLMNET
final_data$pred_glmnet <- predict(fit.glmnet, newdata = final_data)
final_data$prob_glmnet <- predict(fit.glmnet, newdata = final_data, type = "prob")[[1]]

# Random
final_data$pred_rf <- predict(fit.rf, newdata = final_data)
final_data$prob_rf <- predict(fit.rf, newdata = final_data, type = "prob")[[1]]

# Boosted
final_data$pred_gbm <- predict(fit.gbm, newdata = final_data)
final_data$prob_gbm <- predict(fit.gbm, newdata = final_data, type = "prob")[[1]]



confusionMatrix(final_data$pred_lm, final_data$Churn, mode = "prec_recall")
confusionMatrix(final_data$pred_glmnet, final_data$Churn, mode = "prec_recall")
confusionMatrix(final_data$pred_rf, final_data$Churn, mode = "prec_recall")
confusionMatrix(final_data$pred_gbm, final_data$Churn, mode = "prec_recall")








# group all the models together
results <- resamples(list(log = fit.glm,
                          glmnet = fit.glmnet,
                          rf = fit.rf,
                          gbm = fit.gbm))

# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)





table(train$Churn)

# Create model weights (they sum to one)
model_weights <- ifelse(train$Churn == "Churn",
                        (1/table(train$Churn)[1]) * 0.5,
                        (1/table(train$Churn)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits
control$seeds <- fit.glm$control$seeds

# Build weighted model
weighted_fit <- train(Churn ~ .,
                      data = train,
                      method = "glm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = control)

# Build down-sampled model
control$sampling <- "down"
down_fit <- train(Churn ~ .,
                  data = train,
                  method = "glm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = control)

# Build up-sampled model
control$sampling <- "up"
up_fit <- train(Churn ~ .,
                data = train,
                method = "glm",
                verbose = FALSE,
                metric = "ROC",
                trControl = control)

# Build smote model
control$sampling <- "smote"
smote_fit <- train(Churn ~ .,
                   data = train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = control)
summary(smote_fit)

# Build custom AUC function to extract AUC
# from the caret model object
test_roc <- function(model, data) {

  roc(data$Churn,
      predict(model, data, type = "prob")[, "Churn"])

}

# Examine results for test set
model_list <- list(original = fit.glm,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = final_data)

model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc){

  results_list_roc[[num_mod]] <-
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])

  num_mod <- num_mod + 1

}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



new_data <- readRDS("app/data/new_data.rds")
# Save Data for App

# logistic
final_data$pred <- predict(fit.glm, newdata = final_data)
final_data$prob  <- round(predict(fit.glm, newdata = final_data, type = "prob")[[1]], 4)

confusionMatrix(final_data$Churn, final_data$pred)

summary(smote_fit)
str(final_data)
## create deciles
decile_results <- final_data %>%
  mutate(
    Quantile = ntile(prob, 6),
    Churn_num = case_when(
      Churn == "Churn" ~ 1,
      Churn == "Active" ~ 0),
    Pred_num = case_when(
      pred == "Churn" ~ 1,
      pred == "Active" ~ 0
    )
  ) %>%
  group_by(Quantile) %>%
  summarise(
    Actual = mean(Churn_num),
    Predicted = mean(prob),
    Sample_Size = n(),
  ) %>%
  arrange(desc(Quantile))

## Plot Deciles
decile_results %>%
  ggplot() +
  geom_point(aes(x = factor(Quantile), y = Actual), col = "blue") +
  geom_line(aes(x = Quantile, y = Actual, color = "blue")) +
  geom_point(aes(x = factor(Quantile), y = Predicted), col = "red") +
  geom_line(aes(x = Quantile, y = Predicted, color = "red")) +
  scale_colour_manual(
    name = '',
    values = c('blue' = 'blue', 'red' = 'red'),
    labels = c('Actual', 'Predicted')
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Calendly Churn Rate by Decile",
    subtitle = "Actual Rates vs. Predicted Rates",
    y = "Churn Rate",
    x = "Decile"
  ) +
  theme_bw()

## View splits by plan name
data %>%
  group_by(plan_name) %>%
  mutate(
    Churn_num = case_when(
      Churn == "Churn" ~ 1,
      Churn == "No_Churn" ~ 0
    )
  ) %>%
  summarise(Actual = mean(Churn_num),)

corrr_analysis
## Save Data
saveRDS(final_data, here("app/data/new_data.rds"))
saveRDS(corrr_analysis, here("app/data/corrr_analysis.rds"))
## Save Model
saveRDS(fit.glm, here("app/models/fit_glm.rds"))
## Load Model
super_model <- readRDS(here("app/models/fit_glm.rds"))
## Load Data
new_data <- readRDS("app/data/new_data.rds")






