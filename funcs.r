

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
