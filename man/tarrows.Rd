% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animatoR-functions-knitr.R
\name{tarrows}
\alias{tarrows}
\title{Move Arrows}
\usage{
tarrows(x0, y0, x1, y1, x2 = x0, y2 = y0, x3 = x1, y3 = y1, t, when,
  p = 1, length = 0.125, ...)
}
\arguments{
\item{x0, x1}{numeric vectors, start arrow defining x coordinates.}

\item{y0, y1}{numeric vector, start arrow defining y coordinates.}

\item{x2, x3}{numeric vector, end arrow defining x coordinates.}

\item{y2, y3}{numeric vector, end arrow defining y coordinates.}

\item{t}{numeric, homotopy parameter, limited between 0 and 1.
This parameter can be considered as fraction of animation duration time.}

\item{when}{numeric vector. This parameter controls
the times of: entrance, exit, start of movement and, end of movement.}

\item{p}{numeric, homotopy power parameter. Defaults to 1.}

\item{length}{length of the edges of the arrow head (in inches).}

\item{...}{additional arguments passed to \code{\link{arrows}}.}
}
\value{
List with numerical components \code{x} and \code{y} with
  current position
}
\description{
Move arrows from start to end location
}
\examples{
x0 <- c(0,4,7)
y0 <- c(0,7,0)
x1 <- c(4,0,7)
y1 <- c(2,7,5)
x2 <- x0+1
y2 <- y0+2
x3 <- x1+1
y3 <- y1+2
newplot()
arrows(x0,y0,x2,y2,lty=2)
arrows(x1,y1,x3,y3,lty=2)
arrows(x0,y0,x1,y1,col=2)
pos <- tarrows(x0,y0,x1,y1,x2,y2,x3,y3,0.75,lwd=3)
arrows(x2,y2,x3,y3,col=4)
pos
points(pos$start)
points(pos$end)
d <- 0.2
points(x0-d,y0+d,pch="0")
points(x1+d,y1+d,pch="1")
points(x2-d,y2+d,pch="2")
points(x3+d,y3+d,pch="3")
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\code{\link[graphics]{arrows}}, \code{\link{darrows}}
  for drawing arrows and \code{\link{tsegments}} for moving segments.
}
\keyword{aplot}
\keyword{dynamic}
\keyword{graphics}

