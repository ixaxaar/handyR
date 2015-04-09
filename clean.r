
# Identify the dataset
names(dat) <- normVarNames(names(dat))
# Lower case variable names.
vars <- names(dat)
target <- "rain_tomorrow"
risk <- "risk_mm"
id <- c("date", "location")

# Ignore the IDs and the risk variable.
ignore <- c(id, if (exists("risk")) risk)

# Ignore variables which are completely missing.
mvc <- sapply(dat[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(dat)))
ignore <- union(ignore, mvn)

# Initialise the variables
vars <- setdiff(vars, ignore)

# Variable roles.
inputs <- setdiff(vars, target)
numi <- which(sapply(dat[inputs], is.numeric))
numc <- names(numi)
cati <- which(sapply(dat[inputs], is.factor))
catc <- names(cati)

# Remove all observations with a missing target.
dat <- dat[!is.na(dat[target]),]

# Impute missing values needed for randomForest().
if (sum(is.na(dat[vars]))) dat[vars] <- na.roughfix(dat[vars])

# Ensure the target is categoric.
dat[target] <- as.factor(dat[[target]])

# Number of observations.
nobs <- nrow(dat)

# Prepare for model building.
form <- formula(paste(target, "~ ."))
seed <- 328058
train <- sample(nobs, 0.7*nobs)
test <- setdiff(seq_len(nobs), train)
actual <- dat[test, target]
risks <- dat[test, risk]

