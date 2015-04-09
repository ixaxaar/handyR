
require("deSolve")

f = function(r, x) {
  return(  r[1] - r[2]*x - exp(-1*x)  )
}

sequencer <- function(seq.range, fragments) {
  return(seq(from=seq.range[1], to=seq.range[2], by=(seq.range[2]-seq.range[1])/fragments))
}

plot.phase <- function(f, range.main, ranges, fragments=1000, no.runs=100, ylim=c(-100, 100), color.var=2) {
  num.params = nrow(ranges)
  values = apply(ranges, 1, sequencer, fragments)
  values = data.frame(values)

  color.mul = no.runs/(ranges[color.var,]$high - ranges[color.var,]$low)

  x = sequencer(range.main, fragments)

  colors = rainbow(n=1.2*no.runs, alpha=1)
  plot(x=0, type="n", xlim=c(min(x), max(x)), ylim=ylim)

  for (r in seq(no.runs)) {
    samples = apply(values, 2, sample, 1)
    lines(x=x, y=f(samples, x), col=colors[samples[color.var]*color.mul + no.runs/2])
  }

  lines(y=c(0,0), x=c(min(x), max(x)))
  lines(x=c(0,0), y=ylim)

  # dev.new()
}

plot.bifurcation = function() {
}

plot.phase(f, c(-20, 20), ranges=data.frame(low=c(-10, -10), high=c(10, 10)), no.runs=1000)
