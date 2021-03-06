% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animatoR-functions-knitr.R
\name{trect}
\alias{trect}
\title{Morf Rectangle}
\usage{
trect(xleft0, ybottom0, xright0, ytop0, xleft1, ybottom1, xright1, ytop1, t,
  p = 1, when, ...)
}
\arguments{
\item{xleft0}{a vector (or scalar) of left x positions.}

\item{ybottom0}{a vector (or scalar) of bottom y positions.}

\item{xright0}{a vector (or scalar) of right x positions.}

\item{ytop0}{a vector (or scalar) of top y positions.}

\item{xleft1}{a vector (or scalar) of left x positions.}

\item{ybottom1}{a vector (or scalar) of bottom y positions.}

\item{xright1}{a vector (or scalar) of right x positions.}

\item{ytop1}{a vector (or scalar) of top y positions.}

\item{t}{numeric, homotopy parameter, limited between 0 and 1.
This parameter can be considered as fraction of animation duration time.}

\item{p}{numeric, homotopy power parameter. Defaults to 1.}

\item{when}{numeric vector. This parameter controls
the times of: entrance, exit, start of movement and, end of movement.}

\item{...}{additional arguments passed to \code{\link{rect}}.}
}
\value{
List with numerical components \code{x} and \code{y} with
  current position
}
\description{
Morf one rectangle to another
}
\examples{
xleft0   <- 1
ybottom0 <- 1
xright0  <- 5
ytop0    <- 5
xleft1   <- 3
ybottom1 <- 3
xright1  <- 6
ytop1    <- 9
newplot()
rect( xleft0, ybottom0, xright0, ytop0,lty=2,border=2)
rect( xleft1, ybottom1, xright1, ytop1,lty=2,border=4)
arrows(
c(xleft0,xright0),c(ybottom0,ytop0),
c(xleft1,xright1),c(ybottom1,ytop1),lty=2)
## Intermediate rectangle
pos <- trect(xleft0, ybottom0, xright0, ytop0,
                xleft1, ybottom1, xright1, ytop1, t= 0.75)
str(pos)
#############
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\code{\link[graphics]{rect}} and
  \code{\link{tpolygon}} for moving a polygon.
}
\keyword{aplot}
\keyword{dynamic}
\keyword{graphics}

