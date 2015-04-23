
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

# Plot predictions and residuals
df = data.frame(Training=dat, Predicted=predict(trained))
ggplot(data=df, aes(Training ~ Predicted)) + geom_point()
df1 = data.frame(Training=resid(model), Predicted=predict(trained))
ggplot(data=df1, aes(Residual, Predicted)) + geom_point()

# Labelled resudials
plot(model, which=1)

# Scale-location plot
plot(model, which=3)

# Cooks distance
plot(model, which=5)

# Confidence intervals
confint(model)

# Influence and outlier detection
influence(model)

# Covariances of residuals
covariances = sapply(c("pred1", "pred2"), function(o) { return(cov(model$residuals, dat[o])) })

# RMSE of residuals
sqrt(sum(model$residuals^2)/(nrow(dat)-num_predictors)) == model$sigma == sqrt(deviance(model)/(nrow(dat)-num_predictors))

# R squared - variation explained by model
summary(model)$r.squared

roc_curve <- function(pr.m)
{
  pr <- prediction(pr.m, actual)
  pe <- performance(pr, "tpr", "fpr")
  au <- performance(pr, "auc")@y.values[[1]]
  pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))

  p <- ggplot(pd, aes(x=fpr, y=tpr))
  p <- p + geom_line(colour="red")
  p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
  p <- p + ggtitle(deparse(substitute(pr.m)))
  p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
  label=paste("Area under curve =", round(au, 2)))

  return(p)
}

# Dicky-Fuller test on stationarity
require(tseries)
adf.test(dat, alternative="stationary", k=0)
adf.test(dat, alternative="explosive", k=0)

# GLS
require(nlme)
plot(ACF(model, form=~var), alpha=0.05)
model1 = update(model, correlation=corAR1())
require(MuMIn)
AICc(model, model1)
