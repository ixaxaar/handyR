
# Model comparison
require(caret)
fit1 = train(pred ~ .,
 data = dat,
 method = "svmRadial",
 preProc = c("center", "scale"),
 tuneLength = 10,
 trControl = trainControl(method = "repeatedcv",
 repeats = 5))

fit2 = train(pred ~ .,
 data = dat,
 method = "glm",
 preProc = c("center", "scale"),
 tuneLength = 10,
 trControl = trainControl(method = "repeatedcv",
 repeats = 5))

# Difference between model fits
diff(resamples(list(SVM=fit1, Logistic=fit2)))

# Regression performance
R2(predicted, observed)
RMSE(predicted, observed)
cor(predicted, observed)
cor(predicted, observed, method = "spearman")

