
# Type of data
class(dat)
dim(dat)
names(dat)

# Summary stats
summary(dat)
str(dat)
pairs(dat)

# Sneak peek
head(dat)
tail(dat)
dat[sample(nrow(dat), 5), ]

# Remove annoying naming into normal ones
library(rattle)
names(dat) = normVarNames(names(dat))

# Sanitize dates
library(lubridate)
dat$date = ymd(dat$date)

# Remove NAs
dat = na.omit(dat)

# Subset numeric columns
dat[, sapply(dat, is.numeric)]


# Feature selection
library(FSelector)
information.gain(target ~ ., data=dat)

# Resample missing values
library(randomForest)
datnum = dat[, sapply(dat, is.numeric)]
datnum[names(datnum)] = na.roughfix(datnum[names(datnum)])



