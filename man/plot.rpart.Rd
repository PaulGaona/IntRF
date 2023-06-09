\name{plot.rpart}
\alias{plot.rpart}
\title{
Plot an Rpart Object
}
\description{
Plots an rpart object on the current graphics device.
}
\usage{
\method{plot}{rpart}(x, uniform=FALSE, branch=1, compress=FALSE, nspace,
     margin=0, minbranch=.3, bar,\dots)
}
\arguments{
\item{x}{
  a fitted object of class \code{rpart}, containing a classification,
  regression, or rate tree.
}
\item{uniform}{
if \code{TRUE}, uniform vertical spacing of the nodes is used; this may be
less cluttered when fitting a large plot onto a page.
The default is to use a non-uniform spacing proportional to the
error in the fit.
}
\item{branch}{
controls the shape of the branches from parent to child node.
Any number from 0 to 1 is allowed.  A value of 1 gives square
shouldered branches, a value of 0 give V shaped branches,
with other values being intermediate.
}
\item{compress}{
If \code{FALSE}, the leaf nodes will be at the horzontal plot coordinates of
\code{1:nleaves}.
If \code{TRUE}, the routine attempts a more compact arrangement of
the tree.
The compaction algorithm assumes \code{uniform=TRUE}; surprisingly, the result
is usually an improvement even when that is not the case.
}
\item{nspace}{
the amount of extra space between a node with children and
a leaf, as compared to the minimal space between leaves.
Applies to compressed trees only.  The default is the value of
\code{branch}.
}
\item{margin}{
an extra percentage of white space to leave around the borders of the tree.
(Long labels sometimes get cut off by the default computation).
}
\item{minbranch}{
set the minimum length for a branch to \code{minbranch} times the average
branch length.  This parameter is ignored if \code{uniform=TRUE}.
Sometimes a split will give very little improvement, or even (in
the classification case) no improvement at all.
A tree with branch lengths strictly proportional to improvement
leaves no room to squeeze in node labels.
}
\item{bar}{
 length of bar at root (default = 0.03) -- used instead of char "|"
}
\item{\dots}{
  arguments to be passed to or from other methods.
}}
\value{
the coordinates of the nodes are returned as a list, with
components \code{x} and \code{y}.
}
\section{Side Effects}{
an unlabeled plot is produced on the current graphics device.
}
\details{
This function is a method for the generic function \code{plot}, for objects
of class \code{rpart}.
The y-coordinate of the top node of the tree will always be 1.
}
\seealso{
\code{\link{rpart}}, \code{\link{text.rpart}}
}
\examples{
#data(car.test.frame)
#fit <- rpart(Price ~ Mileage + Type + Country, car.test.frame)
#plot(fit, compress=TRUE)
#text(fit, use.n=TRUE)
}
\keyword{tree}
% docclass is function
% Converted by Sd2Rd version 1.20.
