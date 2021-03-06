
# Naive Bayes
library(e1071)
model = m.nb = naiveBayes(form, ds[train, vars])
cl.nb = predict(model, ds[test, vars], type="class")
pr.nb = predict(model, ds[test, vars], type="raw")[,2]

# Decision tree
library(rpart)
model = m.rp = rpart(form, ds[train, vars])
cl.rp = predict(model, ds[test, vars], type="class")
pr.rp = predict(model, ds[test, vars], type="prob")[,2]
train(Species ~., data=dat, method="rpart",trControl=tc,tuneGrid=rpart.grid)

# Regression tree
library(tree)
model = m.rt = rpart(form, ds[train, vars])
cl.rt = predict(model, ds[test, vars], type="class")
pr.rt = predict(model, ds[test, vars], type="prob")[,2]

# Random forest
library(randomForest)
model = m.rf = randomForest(form, ds[train, vars], ntree=100)
cl.rf = predict(model, ds[test, vars], type="class")
pr.rf = predict(model, ds[test, vars], type="prob")[,2]
library(party)
model = m.rf = cforest(form, ds[train, vars], ntree=100)
cl.rf = predict(model, ds[test, vars], type="class")
pr.rf = predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace rpart
library(wsrpart)
model = m.wsrp = wsrpart(form, ds[train, vars], ntree=10)
cl.wsrp = predict(model, ds[test, vars], type="class")
pr.wsrp = predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace random forest
library(wsrf)
model = m.wsrf = wsrf(form, ds[train, vars], ntree=10)
cl.wsrf = predict(model, ds[test, vars], type="class")
pr.wsrf = predict(model, ds[test, vars], type="prob")[,2]

# Linear Regression
model = lm(Predictor ~ ., data=dat)
summary(model)
pred = predict(model, dat)
require(caret)
defaultSummary(data.frame(obs=dat, pred=pred))

# Robust linear model
model = rlm(Predictor ~ ., data=dat)
ctrl = trainControl(method="cv", number=10)
trained = train(x=predictor, y=outcome, method="lm", trControl=ctrl)
trained = train(x=predictor, y=outcome, method="rlm", preProcess="pca", trControl=ctrl)
weights = model$w

# Partial least squares
require(pls)
model = pslr(Predictor ~ ., data=dat)
ctrl = trainControl(method="cv", number=10)
trained = train(x=predictor, y=outcome, method="pls", preProcess=c("center", "scale"), trControl=ctrl)

# Ridge regression
model = lm.ridge(x=predictor, y=outcome, data=dat, lambda=0.001)
penalties = data.frame(.lambda = seq(0, .1, length = 15))
model = train(x=predictor, y=outcome, method="ridge", tuneGrid=penalties,
  trControl=ctrl, preProc=c("center", "scale"))

# Multivariate Adaptive Regression Splines
require(earth)
model = earth(x=predictor, y=outcome)
plotmo(model)
trained = train(x=predictor, y=outcome, method="earth",
  tuneGrid=expand.grid(.degree=1:2, .nprune=2:38),
  trControl=trainControl(method="cv"))
varImp(trained)

# Logistic
model = glm(formula, data=dat, family="binomial")

# Generalized linear models
model = glm(formula, data=dat, family="poisson")
plot(model)
require(nlme)
# gls - account for temporal autocorrelation
model = gls(formula, data=dat, family="poisson")
plot(model)

# Time series
decomposed = decompose(dat)
plot(decomposed)

# ARIMA
acf(dat)
pacf(dat)
model = arima(dat, order=c(1,0,0))
predicted = predict(model, n.ahead=100)
plot(dat)
lines(predicted$pred, col="blue")
lines(predicted$pred-predicted$se, col="red")
lines(predicted$pred+predicted$se, col="red")

# Survival
# Kaplan-Meier
model = survfit(Surv(dat$time, dat$event) ~ group)
# Nelson-Aalen
model = survfit(coxph(dat$time, dat$event) ~ group, type="aalen")
# Cox
model = coxph(Surv(time, event) ~ groups, method="breslow")
# Parametric models
model = survreg(Surv(time, event) ~ groups, dist="exponential")
model = survreg(Surv(time, event) ~ groups, dist="weibull")
model = survreg(Surv(time, event) ~ groups, dist="loglogistic")

hazard = -log(model$surv)
plot(model)
summary(model)
