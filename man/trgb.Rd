% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animatoR-functions-knitr.R
\name{trgb}
\alias{trgb}
\title{Interpolate Colors}
\usage{
trgb(x0, x1 = x0, t, when = c(0, 1), p = 1, alpha0 = 1, alpha1 = 1,
  ...)
}
\arguments{
\item{x0}{vector specifying start colors or factor.
Colors can be specified as numbers or
character strings, see \code{\link{colors}}.
Factors are also acceptable
in which case the input will be transformed into level numbers.}

\item{x1}{vector specifying end colors or factor.
Colors can be specified as numbers or
character strings, see \code{\link{colors}}.
Factors are also acceptable
in which case the input will be transformed into level numbers.}

\item{t}{numeric, homotopy parameter, limited between 0 and 1.
This parameter can be considered as fraction of animation duration time.}

\item{when}{numeric vector. This parameter controls
the times of: entrance, exit, start of movement and, end of movement.}

\item{p}{numeric, homotopy power parameter. Defaults to 1.}

\item{alpha0, alpha1}{numeric, transparency value
\code{\link[grDevices]{rgb}}.}

\item{...}{additional arguments passed to \code{\link[grDevices]{rgb}}.}
}
\value{
A character vector with elements of 7 or 9 characters, "#"
  followed by the red, blue, green and optionally alpha
  values in hexadecimal (after rescaling to 0 ... 255).
  The optional alpha values range from 0
  (fully transparent) to 255 (opaque).
}
\description{
This function interpolates the color between start and end color.
}
\examples{
if(interactive()){
animator('
newplot()
points(5,5,col=trgb("red","blue",t,alpha1=1),pch=16,cex=tcex(80,160,t))',
life=1)
}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\code{\link[grDevices]{rgb}} for setting colors and
  \code{\link[grDevices]{colors}} for color names.

  See \code{link{makeTransparent}} to add transparency to the colors.
}
\keyword{aplot}
\keyword{dynamic}
\keyword{graphics}

