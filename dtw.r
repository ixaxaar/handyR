

require(dtw)
require(RColorBrewer)

# Data
a = c(rep(0, 1000), 10*sin(seq(1, 10*pi, length.out=500)) + 2*rnorm(500), rep(0, 1500)) +
  arima.sim(n=3000, model=list(1,0,0))
# Pattern
b = 100*cos(seq(0, 2*pi, length.out=100))

# Number of sliding windows
winnum = 100
overlap = 10

# calculate window size
winsize = 0
winsize = ifelse (length(a) / winnum < length(b), length(b), length(a) / winnum)
print(paste("Window size ", winsize + overlap))
n = seq(winnum)

distances = rep(0, winnum)
n1s = rep(0, winnum)
n2s = rep(0, winnum)

last = 1
# Perform dtw fitting for each sliding window
for (ctr in n) {
  n1 = last
  n2 = last + winsize + overlap
  last = last + (length(a) / winnum)
  c = a[n1:n2]

  d = dtw(x=b, y=c,
    keep=TRUE,
    open.end=TRUE,
    open.begin=TRUE,
    step=asymmetric,
    window.type = "sakoechiba",
    window.size=winsize+overlap)

  # Store dtw distances for each window
  distances[ctr] = d$distance
  n1s[ctr] = n1
  n2s[ctr] = n2
}

distances=data.frame(dist=distances, n1=n1s, n2=n2s)

distances.sub = subset(distances, dist>0)
distances.sorted = distances.sub[with(distances.sub, order(dist, decreasing=FALSE)),]

distances.mean = mean(distances.sorted$dist)
distances.min = subset(distances.sorted, dist < distances.mean)

matches = as.numeric(rownames(distances.min))
a.max = max(a)
a.min = min(a)

matches.unique = rep(TRUE, length(matches))

# Filter out overlapping solutions and keep ones with lowest distance
for (match in matches[matches.unique]) {
  d = distances[match,]
  d.int = sapply(matches, function(x) {
    d2 = distances[x,]
    n = length(intersect( seq(d$n2, d$n1), seq(d2$n2, d2$n1) ))
    ifelse(n > (winsize+overlap)/2 && d$dist < d2$dist, FALSE, TRUE)
  })
  matches.unique = matches.unique & d.int
}
matches.unique = matches[matches.unique]
print(paste("Found ", length(matches.unique), " matches"))

ctr = 1
colors = rainbow(length(matches.unique))

par(mfrow=c(2,1))

# Plot the given signal
plot.ts(a,
  ylim=c(a.min, a.max + 10))

for (match in matches.unique) {
  d = distances[match,]

  # Highlight the matched parts of the signal
  lines(y=a[d$n1 : d$n2],
   x=d$n1 : d$n2, col=colors[ctr])

  # Indicate a bar depciting the range of match and heigh denoting distance
  points(y=rep(a.max + 5, d$n2-d$n1+1),
    x=d$n1 : d$n2, col=colors[ctr], pch="-")
  ctr = ctr + 1
}

plot.ts(distances$dist)
