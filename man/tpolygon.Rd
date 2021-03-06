% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animatoR-functions-knitr.R
\name{tpolygon}
\alias{tpolygon}
\title{Move Polygon}
\usage{
tpolygon(x0, y0, x1, y1, t, when, p = 1, ...)
}
\arguments{
\item{x0}{numeric vector, start x coordinates.}

\item{y0}{numeric vector, start y coordinates.}

\item{x1}{numeric vector, end x coordinates.}

\item{y1}{numeric vector, end y coordinates.}

\item{t}{numeric, homotopy parameter, limited between 0 and 1.
This parameter can be considered as fraction of animation duration time.}

\item{when}{numeric vector. This parameter controls
the times of: entrance, exit, start of movement and, end of movement.}

\item{p}{numeric, homotopy power parameter. Defaults to 1.}

\item{...}{additional arguments passed to \code{\link{polygon}}.}
}
\value{
List with numerical components \code{x} and \code{y} with
  current position
}
\description{
Move polygon from start to end location
}
\examples{
x0 <- c(0,5)
y0 <- c(0,5)
x1 <- c(1,10)
y1 <- c(1,10)
print(tpolygon(x0,y0,x1,y1,0.5))
#############
par(mfrow=c(2,2))
for( t in seq(0,1,1/3)) {
newplot()
tpolygon(x0,y0,x1,y1,t)
}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\code{\link[graphics]{polygon}}
}
\keyword{aplot}
\keyword{dynamic}
\keyword{graphics}

