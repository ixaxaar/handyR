

# Prepare for model building.
form = formula(paste(target, "~ ."))
seed = 328058
train = sample(nobs, 0.7*nobs)
test = setdiff(seq_len(nobs), train)
actual = dat[test, target]
risks = dat[test, risk]

# Box-cox transformation
require(caret)
lambdas = apply(sg, 2, function(x){ return(BoxCoxTrans(x)$lambda) })

# PCA
pr = princomp(dat, center=TRUE, scale=TRUE)
loadings(pr)
plot(pr)
pr$sdev^2/sum(pr$sdev^2)*100

# Do multiple preprocessing
require(caret)
preProcess(dat, method = c("BoxCox", "center", "scale", "pca"))

# Plot correlations and filter highly correlated variables
require(corrplot)
corrplot(cor(dat), order="hclust")
filtered = dat[,-(findCorrelation(cor(dat), cutoff = .75))]

# Data splitting
require(caret)
createDataPartition
createFolds
createResamples
createMultiFolds

# Periodogram from fft
f = fft(t)
require(TSA)
periodogram(f, method="ar")

