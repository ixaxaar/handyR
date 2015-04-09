
# Identify the dataset
dsname <- "weatherAUS"
ds <- tbl_df(get(dsname))
names(ds) <- normVarNames(names(ds))
# Lower case variable names.
vars <- names(ds)
target <- "rain_tomorrow"
risk <- "risk_mm"
id <- c("date", "location")

# Ignore the IDs and the risk variable.
ignore <- c(id, if (exists("risk")) risk)

# Ignore variables which are completely missing.
mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)

# Initialise the variables
vars <- setdiff(vars, ignore)

# Variable roles.
inputs <- setdiff(vars, target)
numi <- which(sapply(ds[inputs], is.numeric))
numc <- names(numi)
cati <- which(sapply(ds[inputs], is.factor))
catc <- names(cati)

# Remove all observations with a missing target.
ds <- ds[!is.na(ds[target]),]

# Impute missing values needed for randomForest().
if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])

# Ensure the target is categoric.
ds[target] <- as.factor(ds[[target]])

# Number of observations.
nobs <- nrow(ds)

# Prepare for model building.
form <- formula(paste(target, "~ ."))
seed <- 328058
train <- sample(nobs, 0.7*nobs)
test <- setdiff(seq_len(nobs), train)
actual <- ds[test, target]
risks <- ds[test, risk]

