
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

# Random forest
library(randomForest)
model = m.rf = randomForest(form, ds[train, vars], ntree=100)
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
trained = train(x=xtrain, y=ytrain, method="lm", trControl=ctrl)
trained = train(x=xtrain, y=ytrain, method="rlm", preProcess="pca", trControl=ctrl)

# Partial least squares
require(pls)
model = pslr(Predictor ~ ., data=dat)
ctrl = trainControl(method="cv", number=10)
trained = train(x=xtrain, y=ytrain, method="pls", preProcess=c("center", "scale"), trControl=ctrl)

# Ridge regression
model = lm.ridge(x=xtrain, y=ytrain, data=dat, lambda=0.001)
penalties = data.frame(.lambda = seq(0, .1, length = 15))
ridgeRegFit = train(x=xtrain, y=ytrain, method="ridge", tuneGrid=penalties,
  trControl=ctrl, preProc=c("center", "scale"))

# Evaluate performance
df = data.frame(Training=dat, Predicted=predict(trained))
ggplot(data=df, aes(Training ~ Predicted)) + geom_point()
df1 = data.frame(Training=resid(model), Predicted=predict(trained))
ggplot(data=df1, aes(Residual, Predicted)) + geom_point()


