library(renv)
renv::restore()
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(GGally)
library(modelr)
library(MASS)
library(tidyverse)
library(tidyr)
library(randomForest)
library(xgboost)
library(e1071)
library(glmnet)
library(neuralnet)
library(ranger)
library(catboost)
library(keras)
library(keras3)
library(tensorflow)
library(kernlab)
options(renv.config.sandbox.enabled = FALSE)
#install_keras() 
set.seed(123)
#renv::init()
#renv::snapshot()

# ---------------------------------------------------------
# Cleaning Data
# ---------------------------------------------------------
Cropdata <- read.csv("EcoCrop_DB.csv")
Cropdata <- subset(Cropdata, select = -c(AUTH, EcoPortCode, FAMNAME, SYNO))

factor_cols <- c("LIFO","HABI","LISPA","PHYS","PLAT","LIOPMN","LIOPMX",
                 "LIMN","LIMX","DEP","DEPR","TEXT","TEXTR","FER","FERR",
                 "TOX","TOXR","SAL","SALR","DRA","DRAR","PHOTO","CLIZ",
                 "ABISUS","ABITOL","INTRI","PROSY")

Cropdata[factor_cols] <- lapply(Cropdata[factor_cols], factor)

# ---- Keep only first CAT entry and convert to factor ----
Cropdata <- Cropdata %>%
  mutate(
    CAT = ifelse(is.na(CAT) | CAT == "", NA,
                 trimws(strsplit(CAT, ",") |> sapply(`[`, 1)))
  ) %>%
  mutate(CAT = factor(CAT))

Crop_noCAT <- Cropdata %>% filter(is.na(CAT) | CAT == "")
Crop_withCAT <- Cropdata %>% filter(!(is.na(CAT) | CAT == ""))

Crop_long <- Crop_withCAT %>% mutate(Category = CAT)

Cropsfinal <- Crop_long %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Category, values_from = value, values_fill = 0)

# Remove Column1 if present
Cropsfinal <- Cropsfinal[, setdiff(names(Cropsfinal), "Column1")]

table(Cropsfinal$CAT)
# ---------------------------------------------------------
# Assigning Yields
# ---------------------------------------------------------
faostat <- read.csv("FAOSTAT_data_en_8-18-2025.csv")

# ---------------------------------------------------------
# 1. CATEGORY MAP (explicit FAOSTAT crop → category)
# ---------------------------------------------------------
category_map <- tribble(
  ~Item, ~Category,
  # Cereals & pseudocereals
  "Wheat", "cereals",
  "Rice", "cereals",
  "Maize (corn)", "cereals",
  "Barley", "cereals",
  "Oats", "cereals",
  "Rye", "cereals",
  "Millet", "cereals",
  "Sorghum", "cereals",
  "Buckwheat", "cereals",
  "Quinoa", "cereals",
  "Mixed grain", "cereals",
  
  # Pulses
  "Beans, dry", "pulses",
  "Chick peas, dry", "pulses",
  "Lentils, dry", "pulses",
  "Peas, dry", "pulses",
  "Cow peas, dry", "pulses",
  "Pigeon peas, dry", "pulses",
  "Bambara beans, dry", "pulses",
  
  # Roots & tubers
  "Potatoes", "roots_tubers",
  "Cassava, fresh", "roots_tubers",
  "Sweet potatoes", "roots_tubers",
  "Yams", "roots_tubers",
  "Taro", "roots_tubers",
  "Yautia", "roots_tubers",
  
  # Vegetables
  "Tomatoes", "vegetables",
  "Cabbages", "vegetables",
  "Carrots and turnips", "vegetables",
  "Cucumbers and gherkins", "vegetables",
  "Eggplants (aubergines)", "vegetables",
  "Lettuce and chicory", "vegetables",
  "Onions and shallots, dry (excluding dehydrated)", "vegetables",
  "Onions and shallots, green", "vegetables",
  "Peas, green", "vegetables",
  "Spinach", "vegetables",
  "Pumpkins, squash and gourds", "vegetables",
  "Other vegetables, fresh n.e.c.", "vegetables",
  
  # Fruits & nuts
  "Apples", "fruits_nuts",
  "Bananas", "fruits_nuts",
  "Grapes", "fruits_nuts",
  "Oranges", "fruits_nuts",
  "Mangoes, guavas and mangosteens", "fruits_nuts",
  "Pears", "fruits_nuts",
  "Peaches and nectarines", "fruits_nuts",
  "Strawberries", "fruits_nuts",
  "Blueberries", "fruits_nuts",
  "Watermelons", "fruits_nuts",
  "Papayas", "fruits_nuts",
  "Pineapples", "fruits_nuts",
  "Avocados", "fruits_nuts",
  
  # Materials (fibres)
  "Jute, raw or retted", "materials",
  "Sisal, raw", "materials",
  "True hemp, raw or retted", "materials",
  "Ramie, raw or retted", "materials",
  "Abaca, manila hemp, raw", "materials",
  "Agave fibres, raw, n.e.c.", "materials",
  
  # Medicinals & aromatic
  "Ginger, raw", "medicinal_aromatic",
  "Cinnamon and cinnamon-tree flowers, raw", "medicinal_aromatic",
  "Cloves (whole stems), raw", "medicinal_aromatic",
  "Peppermint, spearmint", "medicinal_aromatic",
  "Vanilla, raw", "medicinal_aromatic"
)

# ---------------------------------------------------------
# 2. JOIN (no 'other' fallback)
# ---------------------------------------------------------
faostat_cat <- faostat %>%
  left_join(category_map, by = "Item")

table(faostat_cat$Category)

# ---------------------------------------------------------
# 3. CALCULATE CATEGORY YIELDS
# ---------------------------------------------------------
category_yields <- faostat_cat %>%
  group_by(Category) %>%
  summarise(
    mean_yield_kg_ha = mean(Value, na.rm = TRUE),
    n_crops = n()
  ) %>%
  mutate(mean_yield_t_ha = mean_yield_kg_ha / 1000)

# ---------------------------------------------------------
# 4. ADD FORAGE CATEGORY
# ---------------------------------------------------------
forage_row <- tibble(
  Category = "forage_pasture",
  mean_yield_kg_ha = 17170,
  n_crops = 1,
  mean_yield_t_ha = 17.17
)

category_yields_final <- bind_rows(category_yields, forage_row)

# ---------------------------------------------------------
# 5. CATEGORY MAP FOR Cropsfinal (no 'other')
# ---------------------------------------------------------
category_cols <- names(Cropsfinal)[51:64]

category_map <- c(
  "cereals & pseudocereals" = "cereals",
  "pulses (grain legumes)"  = "pulses",
  "roots/tubers"            = "roots_tubers",
  "vegetables"              = "vegetables",
  "fruits & nuts"           = "fruits_nuts",
  "materials"               = "materials",
  "medicinals & aromatic"   = "medicinal_aromatic",
  "forage/pasture"          = "forage_pasture",
  "cover crop"              = "forage_pasture",
  "environmental"           = "forage_pasture",
  "forest/wood"             = "forage_pasture",
  "ornamentals/turf"        = "forage_pasture",
  "other" = "forage_pasture"
)

# ---------------------------------------------------------
# 6. LOOKUP TABLE
# ---------------------------------------------------------
yield_lookup <- setNames(category_yields_final$mean_yield_t_ha,
                         category_yields_final$Category)
fallback_yield <- mean(category_yields_final$mean_yield_t_ha, na.rm = TRUE)
# ---------------------------------------------------------
# 7. ASSIGN YIELDS (no 'other' logic)
# ---------------------------------------------------------
Cropsfinal$assigned_yield_t_ha <- NA_real_

for (i in seq_len(nrow(Cropsfinal))) {
  
  row_vals <- Cropsfinal[i, category_cols]
  active_categories <- names(row_vals)[row_vals == 1]
  
  # Map to FAOSTAT categories
  faostat_cats <- category_map[active_categories]
  
  # REMOVE NA categories so they never enter yield_lookup
  faostat_cats <- faostat_cats[!is.na(faostat_cats)]
  
  # If nothing left → fallback
  if (length(faostat_cats) == 0) {
    Cropsfinal$assigned_yield_t_ha[i] <- fallback_yield
    next
  }
  
  # Otherwise compute mean yield
  yields <- yield_lookup[faostat_cats]
  Cropsfinal$assigned_yield_t_ha[i] <- mean(yields, na.rm = TRUE)
}


# ---------------------------------------------------------
# 8. FINAL OUTPUT
# ---------------------------------------------------------
cropsfull <- Cropsfinal %>%
  mutate(
    assigned_yield_t_ha = ifelse(
      is.na(assigned_yield_t_ha),
      fallback_yield,
      assigned_yield_t_ha
    )
  )
yield_lookup <- yield_lookup[!is.na(names(yield_lookup))]
table(yield_lookup)
            
table(cropsfull$assigned_yield_t_ha)
#--------------------------------------------------------------------------
#Multilinear Regression and Reverse
Cropsfinalnumeric <- Cropsfinal %>%
  mutate(across(all_of(factor_cols), ~ as.numeric(.)))

category_cols <- names(Cropsfinal)[51:64]
colnames(Cropsfinalnumeric)

model_data <- Cropsfinalnumeric %>%
  dplyr::select(
    -ScientificName,
    -COMNAME,
    -all_of(category_cols),
    -CAT
  )


model_data <- model_data %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

full_model <- lm(
  assigned_yield_t_ha ~ .,
  data = model_data
)

summary(full_model)

auto_backward <- step(full_model, direction = "backward")
summary(auto_backward)

auto_results <- data.frame(
  Predictor = rownames(summary(auto_backward)$coefficients),
  P_value   = summary(auto_backward)$coefficients[, "Pr(>|t|)"]
)

manual_backward_full <- function(data, response = "assigned_yield_t_ha") {
  predictors <- setdiff(names(data), response)
  
  # Store p-values for predictors the first time they become significant
  sig_pvals <- list()
  
  repeat {
    f <- as.formula(
      paste(response, "~", paste(predictors, collapse = " + "))
    )
    model <- lm(f, data = data)
    sm <- summary(model)
    
    # Extract p-values (excluding intercept)
    pvals <- sm$coefficients[-1, "Pr(>|t|)"]
    
    # Identify significant predictors
    sig_now <- names(pvals[pvals < 0.05])
    
    # Save p-values the FIRST time they become significant
    for (s in sig_now) {
      if (is.null(sig_pvals[[s]])) {
        sig_pvals[[s]] <- pvals[s]
      }
    }
    
    # Stop if only one predictor left
    if (length(predictors) == 1) {
      message("Only one predictor left. Stopping.")
      break
    }
    
    # Remove worst predictor
    worst <- names(which.max(pvals))
    message("Removing predictor: ", worst, " (p = ", round(max(pvals), 4), ")")
    predictors <- predictors[predictors != worst]
  }
  
  # Convert list to data frame
  sig_df <- data.frame(
    Predictor = names(sig_pvals),
    P_value   = unlist(sig_pvals),
    row.names = NULL
  )
  
  list(
    final_model = model,
    significant_predictors = sig_df
  )
}

manual_output <- manual_backward_full(model_data)

manual_model <- manual_output$final_model   # <-- You were missing this

manual_results <- data.frame(
  Predictor = rownames(summary(manual_model)$coefficients),
  P_value   = summary(manual_model)$coefficients[, "Pr(>|t|)"]
)

auto_results
manual_results
manual_output$significant_predictors

#--------------------------------------------------------------------
# Machine Learning

significant_predictors <- manual_output$significant_predictors$Predictor


cols_wanted <- c(
  "assigned_yield_t_ha",
  significant_predictors
)

# keep only those that actually exist in model_data
cols_present <- cols_wanted[cols_wanted %in% names(model_data)]

modeldata <- model_data[, cols_present, drop = FALSE]

table(model_data$assigned_yield_t_ha)
#modeldata <- na.omit(modeldata)


X <- model.matrix(~ . - 1, data = modeldata[, significant_predictors])
colnames(X) <- make.names(colnames(X), unique = TRUE)
y <- modeldata$assigned_yield_t_ha


# Random Forest
rf_model <- randomForest(
  assigned_yield_t_ha ~ .,
  data = modeldata,
  importance = TRUE
)


# SVR
svr_model <- svm(
  x = X,
  y = y,
  kernel = "radial"
)


# Lasso
lasso_model <- cv.glmnet(
  X,
  y,
  alpha = 1
)


# Elastic Net
enet_model <- cv.glmnet(
  X,
  y,
  alpha = 0.5
)

enet_coef <- coef(enet_model, s = "lambda.min")


# -----------------------------
# 1. Build nn_data
# -----------------------------
nn_data <- data.frame(assigned_yield_t_ha = y, X)

# Remove constant columns
nn_data <- nn_data[, sapply(nn_data, function(x) length(unique(x)) > 1)]

# Remove rows with NA
nn_data <- na.omit(nn_data)

# Scale all predictors + response
nn_scaled <- as.data.frame(scale(nn_data))

# Split into X and y matrices
X_mat <- as.matrix(nn_scaled[, -1])
y_vec <- as.matrix(nn_scaled[, 1])

# -----------------------------
# 2. Define Keras model
# -----------------------------
model <- keras_model_sequential() %>%
  layer_dense(units = 5, activation = "relu", input_shape = ncol(X_mat)) %>%
  layer_dense(units = 1, activation = "linear")

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.01),
  loss = "mse",
  metrics = list("mae")
)

# -----------------------------
# 3. Train model
# -----------------------------
history <- model %>% fit(
  X_mat, y_vec,
  epochs = 200,
  batch_size = 16,
  validation_split = 0.2,
  verbose = 1
)

# -----------------------------
# 4. Predict
# -----------------------------
pred_nn_scaled <- model %>% predict(X_mat)

# -----------------------------
# 5. Unscale predictions
# -----------------------------
# Extract scaling attributes
# Scale and keep attributes
scaled_obj <- scale(nn_data)
nn_scaled <- as.data.frame(scaled_obj)

# Extract y scaling
response_mean <- attr(scaled_obj, "scaled:center")["assigned_yield_t_ha"]
response_sd   <- attr(scaled_obj, "scaled:scale")["assigned_yield_t_ha"]

# Unscale predictions
pred_nn <- pred_nn_scaled * response_sd + response_mean






# Ranger ExtraTrees


et_model <- ranger(
  dependent.variable.name = "assigned_yield_t_ha",
  data = data.frame(assigned_yield_t_ha = y, X),
  num.trees = 500,
  splitrule = "extratrees"
)

# CatBoost
train_pool <- catboost.load_pool(
  data = modeldata[, -1],                 
  label = modeldata$assigned_yield_t_ha   
)

cat_model <- catboost.train(
  train_pool,
  params = list(
    loss_function = "RMSE",
    iterations = 500,
    depth = 6,
    learning_rate = 0.05,
    logging_level = "Silent" 
  )
)


#---------------------------------------------------------------
# Predictions for all models

# Random Forest
pred_rf <- predict(rf_model, newdata = modeldata)

# SVR
pred_svr <- predict(svr_model, X)

# Lasso
pred_lasso <- predict(lasso_model, X, s = "lambda.min")

# Elastic Net
pred_enet <- predict(enet_model, X, s = "lambda.min")

# Neural Network
pred_nn <- pred_nn_scaled * response_sd + response_mean

# Ranger ExtraTrees
pred_et <- predict(et_model, data.frame(X))$predictions

# CatBoost
pred_cat <- catboost.predict(cat_model, train_pool)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae  <- function(actual, predicted) mean(abs(actual - predicted))

y_true <- modeldata$assigned_yield_t_ha

results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees",
    "CatBoost"
  ),
  RMSE = c(
    rmse(y_true, pred_rf),
    rmse(y_true, pred_svr),
    rmse(y_true, pred_lasso),
    rmse(y_true, pred_enet),
    rmse(y_true, pred_nn),
    rmse(y_true, pred_et),     
    rmse(y_true, pred_cat)
  ),
  MAE = c(
    mae(y_true, pred_rf),
    mae(y_true, pred_svr),
    mae(y_true, pred_lasso),
    mae(y_true, pred_enet),
    mae(y_true, pred_nn),
    mae(y_true, pred_et),      
    mae(y_true, pred_cat)
  )
)

results

resultsdf <- data.frame(cbind(y_true, pred_rf, pred_svr, pred_lasso, pred_enet, pred_nn, pred_et,pred_cat))
# Define your colours
colors <- c(
  "SVR" = "pink",
  "Random Forest" = "red",
  "Lasso" = "orange",
  "Elastic Net" = "yellow",
  "Neural Network" = "green",
  "Ranger ExtraTrees" = "blue",
  "CatBoost" = "purple"
)

ggplot() +
  # Points
  geom_point(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            shape = 1, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  shape = 2, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          shape = 3, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    shape = 4, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), shape = 5, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), shape = 6, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       shape = 7, size = 2.5) +
  
  # Trend lines
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       method = lm, se = FALSE) +
  
  # 1:1 reference line
  geom_smooth(data = resultsdf, aes(x = y_true, y = y_true), color = "black", method = lm, se = FALSE) +
  
  # Labels + legend
  labs(
    x = "True Value",
    y = "Predicted Value",
    color = "Model"
  ) +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 14) +
  
  # ⭐ Add this to hide low values
  coord_cartesian(xlim = c(2, max(resultsdf$y_true)),
                  ylim = c(2, max(resultsdf[, grep("pred_", names(resultsdf))], na.rm = TRUE)))


#--------------------------------------------------------------------------------------
#cross validation
library(caret)

set.seed(123)

# ---------------------------------------------------------
# 1. Repeated k-fold CV setup
# ---------------------------------------------------------
cv_control <- trainControl(
  method = "repeatedcv",
  number = 10,       # 10 folds
  repeats = 5,       # repeated 5 times
  verboseIter = FALSE
)

# ---------------------------------------------------------
# 2. Prepare data for caret
# ---------------------------------------------------------
df <- data.frame(assigned_yield_t_ha = y, X)

# ---------------------------------------------------------
# 3. Train models using caret wrappers
# ---------------------------------------------------------

# Random Forest
cv_rf <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "rf",
  trControl = cv_control,
  metric = "RMSE"
)

# SVR (radial)
cv_svr <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "svmRadial",
  trControl = cv_control,
  metric = "RMSE"
)

# Lasso
cv_lasso <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 20)),
  trControl = cv_control,
  metric = "RMSE"
)

# Elastic Net
cv_enet <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0.5, lambda = seq(0.0001, 1, length = 20)),
  trControl = cv_control,
  metric = "RMSE"
)

# Neural Network (nnet)
cv_nn <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "nnet",
  trControl = cv_control,
  linout = TRUE,
  trace = FALSE,
  metric = "RMSE",
  tuneLength = 5
)

# Ranger ExtraTrees
cv_et <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "ranger",
  trControl = cv_control,
  metric = "RMSE",
  tuneGrid = expand.grid(
    mtry = floor(sqrt(ncol(X))),
    splitrule = "extratrees",
    min.node.size = 5
  )
)

cat_pool <- catboost.load_pool(
  data = X,
  label = y
)

cat_params <- list(
  loss_function = "RMSE",
  eval_metric = "MAE",
  iterations = 1000,
  depth = 6,
  learning_rate = 0.05,
  random_seed = 123,
  logging_level = "Silent"
)

cv_cat <- catboost.cv(
  pool = cat_pool,
  params = cat_params,
  fold_count = 10,
  type = "Classical"
)

cat_rmse <- tail(cv_cat$test.RMSE.mean, 1)
cat_mae  <- tail(cv_cat$test.MAE.mean, 1)
cat_model <- catboost.train(
  learn_pool = cat_pool,
  params = cat_params
)


# ---------------------------------------------------------
# 4. Collect CV results
# ---------------------------------------------------------
cv_results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees",
    "CatBoost"
  ),
  RMSE = c(
    cv_rf$results$RMSE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$RMSE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$RMSE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$RMSE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$RMSE[which.min(cv_nn$results$RMSE)],
    cv_et$results$RMSE[which.min(cv_et$results$RMSE)],
    cat_rmse
  ),
  MAE = c(
    cv_rf$results$MAE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$MAE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$MAE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$MAE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$MAE[which.min(cv_nn$results$RMSE)],
    cv_et$results$MAE[which.min(cv_et$results$RMSE)],
    cat_mae
  )
)


cv_results

# Extract predictions for cross-validated models
cv_rf_pred <- predict(cv_rf, newdata = df)
cv_svr_pred <- predict(cv_svr, newdata = df)
cv_lasso_pred <- predict(cv_lasso, newdata = df)
cv_enet_pred <- predict(cv_enet, newdata = df)
cv_nn_pred <- predict(cv_nn, newdata = df)
cv_et_pred <- predict(cv_et, newdata = df)
cv_cat_pred <- catboost.predict(cat_model, cat_pool)



# Update the results data frame with the correct predictions
resultsdf_cv <- data.frame(
  y_true = y_true,
  cv_rf = cv_rf_pred,
  cv_svr = cv_svr_pred,
  cv_lasso = cv_lasso_pred,
  cv_enet = cv_enet_pred,
  cv_nn = cv_nn_pred,
  cv_et = cv_et_pred,
  cv_cat = cv_cat_pred
)

# Now use resultsdf_cv for plotting
ggplot() +
  # Points
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_svr,  color = "SVR"),            shape = 1, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_rf,   color = "Random Forest"),  shape = 2, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_lasso,color = "Lasso"),          shape = 3, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_enet, color = "Elastic Net"),    shape = 4, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_nn,   color = "Neural Network"), shape = 5, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_et,   color = "Ranger ExtraTrees"), shape = 6, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_cat,  color = "CatBoost"),       shape = 7, size = 2.5) +
  
  # Trend lines
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_svr,  color = "SVR"),            method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_rf,   color = "Random Forest"),  method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_lasso,color = "Lasso"),          method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_enet, color = "Elastic Net"),    method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_nn,   color = "Neural Network"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_et,   color = "Ranger ExtraTrees"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_cat,  color = "CatBoost"),       method = lm, se = FALSE) +
  
  # 1:1 reference line
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = y_true), color = "black", method = lm, se = FALSE) +
  
  # Labels + legend
  labs(
    x = "True Value",
    y = "Predicted Value",
    color = "Model"
  ) +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 14) +
  
  # ⭐ Add zoom to hide lower values
  coord_cartesian(
    xlim = c(2, max(resultsdf_cv$y_true, na.rm = TRUE)),
    ylim = c(2, max(resultsdf_cv[, grep("^cv_", names(resultsdf_cv))], na.rm = TRUE))
  )



# -----------------------------
# COLOUR PALETTE (light = original, dark = CV)
# -----------------------------
colors <- c(
  # Original (lighter)
  "SVR"               = "lightpink",  # light pink
  "Random Forest"     = "tomato",  # light red
  "Lasso"             = "orange",  # light orange
  "Elastic Net"       = "yellow",  # light yellow
  "Neural Network"    = "springgreen1",  # light green
  "Ranger ExtraTrees" = "turquoise2",  # light blue
  "CatBoost"          = "mediumorchid1",  # light purple
  
  # CV (darker)
  "SVR_cv"               = "deeppink",  # dark pink
  "Random Forest_cv"     = "red",  # dark red
  "Lasso_cv"             = "darkorange",  # dark orange
  "Elastic Net_cv"       = "gold",  # dark yellow
  "Neural Network_cv"    = "forestgreen",  # dark green
  "Ranger ExtraTrees_cv" = "royalblue2",  # dark blue
  "CatBoost_cv"          = "darkviolet"   # dark purple
)

# -----------------------------
# COMBINED PLOT
ggplot() +
  # --- CV POINTS ---
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_svr,  color = "SVR_cv"),            shape = 1, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_rf,   color = "Random Forest_cv"),  shape = 2, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_lasso,color = "Lasso_cv"),          shape = 3, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_enet, color = "Elastic Net_cv"),    shape = 4, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_nn,   color = "Neural Network_cv"), shape = 5, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_et,   color = "Ranger ExtraTrees_cv"), shape = 6, size = 2.5) +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_cat,   color = "CatBoost_cv"), shape = 7, size = 2.5)+
  
  # --- CV TREND LINES ---
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_svr,  color = "SVR_cv"),            method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_rf,   color = "Random Forest_cv"),  method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_lasso,color = "Lasso_cv"),          method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_enet, color = "Elastic Net_cv"),    method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_nn,   color = "Neural Network_cv"), method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_et,   color = "Ranger ExtraTrees_cv"), method = lm, se = FALSE, linetype = "F1") +
  geom_smooth(data = resultsdf_cv, aes(x = y_true, y = cv_cat,   color = "CatBoost_cv"), method = lm, se = FALSE)+
  
  # --- ORIGINAL POINTS ---
  geom_point(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            shape = 1, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  shape = 2, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          shape = 3, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    shape = 4, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), shape = 5, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), shape = 6, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       shape = 7, size = 2.5) +
  
  # --- ORIGINAL TREND LINES ---
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       method = lm, se = FALSE) +
  
  # --- 1:1 LINE ---
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  
  labs(
    x = "True Value",
    y = "Predicted Value",
    color = "Model"
  ) +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 14) +
  
  # ⭐ ZOOM FIX ADDED HERE
  coord_cartesian(
    xlim = c(2, max(resultsdf$y_true, resultsdf_cv$y_true, na.rm = TRUE)),
    ylim = c(
      2,
      max(
        resultsdf[, grep("^pred_", names(resultsdf))],
        resultsdf_cv[, grep("^cv_", names(resultsdf_cv))],
        na.rm = TRUE
      )
    )
  )


cv_results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees",
    "CatBoost"
  ),
  RMSE = c(
    cv_rf$results$RMSE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$RMSE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$RMSE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$RMSE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$RMSE[which.min(cv_nn$results$RMSE)],
    cv_et$results$RMSE[which.min(cv_et$results$RMSE)],
    cat_rmse
  ),
  MAE = c(
    cv_rf$results$MAE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$MAE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$MAE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$MAE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$MAE[which.min(cv_nn$results$RMSE)],
    cv_et$results$MAE[which.min(cv_et$results$RMSE)],
    cat_mae
  )
)

cv_rf_pred   <- predict(cv_rf,   newdata = df)
cv_svr_pred  <- predict(cv_svr,  newdata = df)
cv_lasso_pred<- predict(cv_lasso,newdata = df)
cv_enet_pred <- predict(cv_enet, newdata = df)
cv_nn_pred   <- predict(cv_nn,   newdata = df)
cv_et_pred   <- predict(cv_et,   newdata = df)
cv_cat_pred <- catboost.predict(cat_model, cat_pool)

resultsdf_cv <- data.frame(
  y_true = y_true,
  cv_rf = cv_rf_pred,
  cv_svr = cv_svr_pred,
  cv_lasso = cv_lasso_pred,
  cv_enet = cv_enet_pred,
  cv_nn = cv_nn_pred,
  cv_et = cv_et_pred,
  cv_cat = cv_cat_pred
)

results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees",
    "CatBoost"
  ),
  RMSE = c(
    rmse(y_true, pred_rf),
    rmse(y_true, pred_svr),
    rmse(y_true, pred_lasso),
    rmse(y_true, pred_enet),
    rmse(y_true, pred_nn),
    rmse(y_true, pred_et),
    rmse(y_true, pred_cat)
  ),
  MAE = c(
    mae(y_true, pred_rf),
    mae(y_true, pred_svr),
    mae(y_true, pred_lasso),
    mae(y_true, pred_enet),
    mae(y_true, pred_nn),
    mae(y_true, pred_et),
    mae(y_true, pred_cat)
  )
)


combined_model_results <- data.frame(
  Model = cv_results$Model,
  Regular_RMSE = results$RMSE[match(cv_results$Model, results$Model)],
  Regular_MAE  = results$MAE[match(cv_results$Model, results$Model)],
  CV_RMSE      = cv_results$RMSE,
  CV_MAE       = cv_results$MAE
)

summary_results <- data.frame(
  Group = c("All Regular Models", "All CV Models"),
  Average_RMSE = c(
    mean(results$RMSE, na.rm = TRUE),
    mean(cv_results$RMSE, na.rm = TRUE)
  ),
  Average_MAE = c(
    mean(results$MAE, na.rm = TRUE),
    mean(cv_results$MAE, na.rm = TRUE)
  )
)

# Print outputs
cv_results
results
combined_model_results
summary_results

# ----------------------------------------------------------------------------------------
#Boxplots of main ML models based on line of best fit

#Catboost, Random Forest, Ranger, Neural Network

# --- lookup vector ---
#yield_lookup <- c(
 # cereals = 2.754618,
 # fruits_nuts = 19.849769,
 # materials = 1.735017,
 # medicinal_aromatic = 5.787840,
 # pulses = 1.016057,
 # roots_tubers = 12.314667,
 # vegetables = 26.440667,
 # forage_pasture = 17.170000
#)

map_with_tol <- function(x, lookup, tol = 1e-3) {
  sapply(x, function(v) {
    idx <- which(abs(lookup - v) < tol)
    if (length(idx) == 1) names(lookup)[idx] else NA
  })
}
y_true

mapped_categories <- map_with_tol(y_true, yield_lookup)
mapped_categories

df <- data.frame(
  mapped_categories = mapped_categories,
  pred_cat = pred_cat,
  cv_cat_pred = cv_cat_pred,
  pred_rf = pred_rf,
  cv_rf_pred = cv_rf_pred,
  pred_et = pred_et,
  cv_et_pred = cv_et_pred,
  pred_nn = pred_nn,
  cv_nn_pred = cv_nn_pred
)

long_df <- df %>%
  pivot_longer(
    cols = -mapped_categories,
    names_to = "model",
    values_to = "prediction"
  )


true_df <- data.frame(
  mapped_categories = names(yield_lookup),
  true_value = as.numeric(yield_lookup)
)

true_df

ggplot(long_df, aes(x = mapped_categories, y = prediction, fill = mapped_categories)) +
  geom_boxplot() +
  geom_point(
    data = true_df,
    aes(x = mapped_categories, y = true_value),
    colour = "red",
    size = 3
  ) +
  facet_wrap(~ model, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Model Prediction",
    x = "Category",
    y = "Predicted Value"
  )

box_values <- long_df %>%
  group_by(model, mapped_categories) %>%
  summarise(
    Min = min(prediction, na.rm = TRUE),
    Q1 = quantile(prediction, 0.25, na.rm = TRUE),
    Median = median(prediction, na.rm = TRUE),
    Q3 = quantile(prediction, 0.75, na.rm = TRUE),
    Max = max(prediction, na.rm = TRUE),
    .groups = "drop"
  )
box_values
ggplot(long_df, aes(x = model, y = prediction, fill = model)) +
  geom_boxplot() +
  geom_hline(
    data = true_df,
    aes(yintercept = true_value),
    colour = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(~ mapped_categories, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Model Prediction Comparison by Category",
    x = "Model",
    y = "Predicted Value"
  )

selected_model <- "cv_cat_pred"   # <-- change this to any model name

df_one_model <- long_df %>% 
  dplyr::filter(model == selected_model)

ggplot(df_one_model, aes(x = mapped_categories, y = prediction, fill = mapped_categories)) +
  geom_boxplot() +
  geom_point(
    data = true_df,
    aes(x = mapped_categories, y = true_value),
    colour = "red",
    size = 3
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = paste("Model Prediction –", selected_model),
    x = "Category",
    y = "Predicted Value"
  )

#----------------------------------------------------
#top 4 ML plots
ggplot() +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_rf), color = "blue", size = 2.5) +
  geom_smooth(data = resultsdf_cv,aes(x = y_true, y = cv_rf),method = lm,se = FALSE,linetype = "F1",color = "blue")+
  geom_point(data = resultsdf, aes(x = y_true, y = pred_rf), color = "orange", size = 2.5) +
  geom_smooth(data = resultsdf,aes(x = y_true, y = pred_rf),method = lm,se = FALSE,linetype = "F1",color = "orange")

ggplot() +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_cat), color = "darkviolet", size = 2.5) +
  geom_smooth(data = resultsdf_cv,aes(x = y_true, y = cv_cat),method = lm,se = FALSE,linetype = "F1",color = "darkviolet")+
  geom_point(data = resultsdf, aes(x = y_true, y = pred_cat), color = "gold", size = 2.5) +
  geom_smooth(data = resultsdf_cv,aes(x = y_true, y = pred_cat),method = lm,se = FALSE,linetype = "F1",color = "gold")

ggplot() +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_et), color = "aquamarine", size = 2.5) +
  geom_smooth(data = resultsdf_cv,aes(x = y_true, y = cv_et),method = lm,se = FALSE,linetype = "F1",color = "aquamarine")+
  geom_point(data = resultsdf, aes(x = y_true, y = pred_et), color = "tomato", size = 2.5) +
  geom_smooth(data = resultsdf,aes(x = y_true, y = pred_et),method = lm,se = FALSE,linetype = "F1",color = "tomato")

ggplot() +
  geom_point(data = resultsdf_cv, aes(x = y_true, y = cv_nn), color = "forestgreen", size = 2.5) +
  geom_smooth(data = resultsdf_cv,aes(x = y_true, y = cv_nn),method = lm,se = FALSE,linetype = "F1",color = "forestgreen")+
  geom_point(data = resultsdf, aes(x = y_true, y = pred_nn), color = "magenta", size = 2.5) +
  geom_smooth(data = resultsdf,aes(x = y_true, y = pred_nn),method = lm,se = FALSE,linetype = "F1",color = "magenta")

table(mapped_categories)
