url <- "https://raw.githubusercontent.com/rogon666/Disability/main/bendata.csv"
data <- read.csv(url)

# --------------------- Super learner --------------------------
# install.packages("SuperLearner")

# SuperLearner is an algorithm that uses cross-validation to estimate the 
# performance of multiple machine learning models, or the same model with 
# different settings. It then creates an optimal weighted average of those 
# models, aka an “ensemble”, using the test data performance. This approach 
# has been proven to be asymptotically as accurate as the best possible 
# prediction algorithm that is tested.
# https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html

# Check for any missing values:
colSums(is.na(data))

# Extract our outcome variable from the dataframe.
outcome1 = data$IGA
outcome2 = data$nIGA

# Create a dataframe to contain our explanatory variables.
dfX = subset(data, select = -IGA)
dfX = subset(data, select = -nIGA)

# Check structure of our dataframe.
str(dfX)

# If we had factor variables we would use model.matrix() to convert to numerics.

# Review our dimensions.
dim(dfX)

# Set a seed for reproducibility in this random sampling.
set.seed(1)

# Reduce to a dataset of 150 observations to speed up model fitting.
train_obs = sample(nrow(data), 150)

# X is our training sample.
x_train = data[train_obs, ]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
x_holdout = data[-train_obs, ]

# Create a binary outcome variable: towns in which median home value is > 22,000.
outcome_bin = as.numeric(outcome > 22)

y_train = outcome_bin[train_obs]
y_holdout = outcome_bin[-train_obs]

# Review the outcome variable distribution.
table(y_train, useNA = "ifany")

library(SuperLearner)

# Review available models.
listWrappers()

set.seed(1)

# Fit the SuperLearner.
# We need to use list() instead of c().
cv_sl = CV.SuperLearner(Y = y_train, X = x_train, family = binomial(),
                        # For a real analysis we would use V = 10.
                        V = 3,
                        parallel = "multicore",
                        SL.library = list("SL.mean", "SL.glmnet", c("SL.glmnet", "screen.corP")))
summary(cv_sl)


set.seed(666)
sl = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                  SL.library = c("SL.mean", "SL.glmnet", "SL.ranger"))
sl

# Review how long it took to run the SuperLearner:
sl$times$everything

# Predict back on the holdout dataset.
# onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
pred = predict(sl, x_holdout, onlySL = TRUE)

# Check the structure of this prediction object.
str(pred)

# Review the columns of $library.predict.
summary(pred$library.predict)

# Histogram of our predicted values.
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()

# Review AUC - Area Under Curve
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

# Nested cross-validation
set.seed(666)

# Don't have timing info for the CV.SuperLearner unfortunately.
# So we need to time it manually.

system.time({
  # This will take about 2x as long as the previous SuperLearner.
  cv_sl = CV.SuperLearner(Y = y_train, X = x_train, family = binomial(),
                          # For a real analysis we would use V = 10.
                          V = 3,
                          SL.library = c("SL.mean", "SL.glmnet", "SL.ranger"))
})

# We run summary on the cv_sl object rather than simply printing the object.
summary(cv_sl)

# Review the distribution of the best single learner as external CV folds.
table(simplify2array(cv_sl$whichDiscreteSL))

# Plot the performance with 95% CIs (use a better ggplot theme).
plot(cv_sl) + theme_bw()