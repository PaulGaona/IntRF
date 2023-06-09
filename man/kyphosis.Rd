\name{kyphosis}
\alias{kyphosis}
\title{Data on Children who have had Corrective Spinal Surgery}
\description{
  The \code{kyphosis} data frame has 81 rows and 4 columns.
  representing data on children who have had corrective spinal surgery
}
\usage{
data(kyphosis)
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{Kyphosis}}{
a factor with levels
\code{absent}
\code{present}
indicating if a kyphosis (a type of deformation)
was present after the operation.
}
\item{\code{Age}}{
in months
}
\item{\code{Number}}{
the number of vertebrae involved
}
\item{\code{Start}}{
the number of the first (topmost) vertebra operated on.
}
}}
\source{
   John M. Chambers and Trevor J. Hastie eds. (1992)
  \emph{Statistical Models in S}, Wadsworth and Brooks/Cole,
  Pacific Grove, CA 1992.
}
\examples{
#data(kyphosis)
#fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
#fit2 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
#              parms=list(prior=c(.65,.35), split='information'))
#fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
#              control=rpart.control(cp=.05))
#par(mfrow=c(1,2))
#plot(fit)
#text(fit, use.n=TRUE)
#plot(fit2)
#text(fit2, use.n=TRUE)
}
\keyword{datasets}
