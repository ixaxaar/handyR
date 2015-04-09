
# Naive Bayes
library(e1071)
model <- m.nb <- naiveBayes(form, ds[train, vars])
cl.nb <- predict(model, ds[test, vars], type="class")
pr.nb <- predict(model, ds[test, vars], type="raw")[,2]

# Decision tree
library(rpart)
model <- m.rp <- rpart(form, ds[train, vars])
cl.rp <- predict(model, ds[test, vars], type="class")
pr.rp <- predict(model, ds[test, vars], type="prob")[,2]

# Random forest
library(randomForest)
model <- m.rf <- randomForest(form, ds[train, vars], ntree=100)
cl.rf <- predict(model, ds[test, vars], type="class")
pr.rf <- predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace rpart
library(wsrpart)
model <- m.wsrp <- wsrpart(form, ds[train, vars], ntree=10)
cl.wsrp <- predict(model, ds[test, vars], type="class")
pr.wsrp <- predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace random forest
library(wsrf)
model <- m.wsrf <- wsrf(form, ds[train, vars], ntree=10)
cl.wsrf <- predict(model, ds[test, vars], type="class")
pr.wsrf <- predict(model, ds[test, vars], type="prob")[,2]