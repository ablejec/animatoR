% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animatoR-functions-knitr.R
\name{plot.animator}
\alias{plot.animator}
\title{Plot Method for Class \code{animator}.}
\usage{
\method{plot}{animator}(x, life, ...)
}
\arguments{
\item{x}{character, expression (block) or
object of class \code{animator} containing  graphical timed commands.}

\item{life}{numerical, duration of animation.}

\item{...}{additional arguments passed to function \code{animator}.}
}
\value{
Object of class \code{animator}.
}
\description{
Performs and plots the animation of an object.
}
\examples{
x <- as.animator(
"newplot();tpoints(2,2,5,8,cex=2,pch=16)", life=2)
print(x)
if(interactive()) plot(x)
## Equivalent
x <- as.animator(expression({
newplot()
tpoints(2,2,5,8,cex=2,pch=16)
}),
life=2)
print(x)
if(interactive()) plot(x)
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\code{\link{as.animator}}, \code{\link{is.animator}}.
}

