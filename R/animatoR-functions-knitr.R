## ----echo=FALSE----------------------------------------------------------
###################################################
##                                               ##
## (c) Andrej Blejec (andrej.blejec@nib.si) new  ##
##                                               ##
###################################################

## ----echo=FALSE----------------------------------------------------------
.testing <- FALSE
animator <- function(...){ warning("animator function not yet defined\n")}

## ------------------------------------------------------------------------
#' animatoR: Animated Graphics in R (Base Graphics)
#'
#' The package provides several functions for support
#' of animated graphics in base R graphics.
#'
#' @section Usage:
#'
#' Animated graphics can be useful for demonstration of
#' results that change in a succession
#' (e.g. time or direction).
#' It can be used for making of statistical 'cartoons'
#' for demonstartion of statistical concepts in teaching.
#'
#'
#' @docType package
#' @name animatoR
NULL

## ----t params------------------------------------------------------------
#' Argument t
#'
#' Homotopy argument
#'
#' @param t numeric, homotopy parameter, limited between 0 and 1.
#' This parameter can be considered as fraction of animation duration time.
#' @name tParams
NULL

## ----when params---------------------------------------------------------
#' Argument when
#'
#' Description of argument when
#'
#' @param when  numeric vector. This parameter controls
#' the times of: entrance, exit, start of movement and, end of movement.
#' @name whenParams
NULL

## ----p params------------------------------------------------------------
#' Argument p
#'
#' Power of homotopy
#'
#' @param p numeric, homotopy power parameter. Defaults to 1.
#' @name pParams
NULL

## ----coord params--------------------------------------------------------
#' Coordinate Arguments
#'
#' The core coordinate arguments for animatoR plotting functions
#'
#' @param x0 numeric vector, start x coordinates.
#' @param y0 numeric vector, start y coordinates.
#' @param x1 numeric vector, end x coordinates.
#' @param y1 numeric vector, end y coordinates.
#' @name coordParams
NULL

## ----fct newplot---------------------------------------------------------
#' Open New Empty Plotting Window
#'
#' Opens a new empty plotting window with default settings
#'  (0, 10) x (0, 10), no axes nor labels.
#'
#' @param xlim see \code{\link{plot}}.
#' @param ylim see \code{\link{plot}}.
#' @param ann see \code{\link{plot}}.
#' @param axes see \code{\link{plot}}.
#' @param type see \code{\link{plot}}.
#' @param asp aspect ratio, default y/x = 1 see \code{\link{plot.new}}.
#' @param stamp logical, should time be visible on plots.
#' @param ... additional arguments passed to \code{\link{plot}}.
#' @return NULL
#' @note Some additional arguments will not wok (for example argument
#'   \code{bty} are overriden by \code{ann}. You have to use \code{box}
#'   command to produce the box around the plot).
#' @export
#' @import graphics
#' @import grDevices
#' @seealso \code{\link{plot}}
#' @keywords  graphics dynamic hplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' newplot()
#' points(0.5,0.5)
newplot <-
function(xlim=c(0,10),ylim=c(0,10),ann=FALSE,axes=FALSE,type="n",asp=1,
stamp=FALSE,...){
plot(0,0,xlim=xlim,ylim=ylim,ann=ann,axes=axes,type=type,asp=asp,...)
if(stamp) text(par("usr")[2]*0.8,par("usr")[4]*1.05,paste("t =",
round(get("t",envir=sys.frame(-1)),2)),adj=0,xpd=TRUE)
}

## ----fct h---------------------------------------------------------------
#' Homotopy Function
#'
#' Interpolates a position between start and end value(s).
#' Homotopy is controled by a homotopy parameter \code{t} and power parameter \code{p} is used for interpolation.
#'
#' @param x0 numeric vector of start values.
#' @param x1 numeric vector of end values.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return interpolated value (see Note)
#' @export
#' @note Returned coordinates are determined using the homotopy function
#'   \deqn{x_t=x_0  (1-t^p)+x_1 t^p,  t\in[0,1]}
#' @keywords dynamic
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' h(1, 2, 0)
#' h(1, 2, 1)
#' h(1, 2, .5)
#' h(1,c(2,3),.5)
#'
h <-
function(x0, x1, t=0, when, p=1) {
# get timing parameters
start <- 0
end <- 1
enter <- 0
exit <- 1
vNames <- c("start","end","enter","exit")
if(!missing(when)) {
for(i in 1:length(when)) assign(vNames[i],when[i])
if(length(when)<3) enter <- start
}
#
#   print(t)
    if((t<enter)||(t>exit)) return(rep(NA,length.out=length(x0)))
    # scale t

    t <- min(1,max(0,(t-start)/(end-start)))^p
#    cat("->",enter,"\n")
    x <- x0 * (1 - t) + x1 * t
    return(x)
}
if(.testing){
oldpar <- par(mfrow=c(2,2))
tt <- seq(0,1,0.02)
plot(tt,tt,type="n")
for(t in tt) points(t,h(0,1,t))
for(t in tt) points(t,h(0,1,t,p=2))
for(t in tt) points(t,h(0,1,t,p=1/2))
plot(tt,tt,type="n")
for(t in tt) points(t,h(0,1,t,c(0.25,0.75,.1,.9)))
for(t in tt) points(t,h(0,1,t,c(0.25,0.75,.1,.9),p=2))
for(t in tt) points(t,h(0,1,t,c(0.25,0.75,.1,.9),p=1/2))
plot(tt,tt,type="n")
for(t in tt) points(t,h(0,1,t,c(0.25,0.5,.2)),col=2)
for(t in tt) points(t,h(0,1,t,c(0.25,0.5,.2),p=2),col=2)
for(t in tt) points(t,h(0,1,t,c(0.25,0.5,.2),p=1/2),col=2)
par(oldpar)
}

## ------------------------------------------------------------------------
checkX <- function(...){
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
cat("from checkX\n")
print(X)
}

## ----fct tpoints---------------------------------------------------------
#' Move points
#'
#' Move points from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param trace logical. Should movement leave a trace?
#' @param trace.col color of the trace.
#' @param ... additional arguments passed to \code{\link{points}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position.
#' @export
#' @seealso \code{\link[graphics]{points}}
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5)
#' y0 <- c(0,5)
#' x1 <- c(1,10)
#' y1 <- c(1,10)
#' print(tpoints(x0,y0,x1,y1,0.5))
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tpoints(x0,y0,x1,y1,t,trace=TRUE)
#' }
tpoints <-
function(x0, y0, x1=x0, y1=y0, t, when=c(0,1), p=1, trace=FALSE, trace.col="grey",...) {
# print(sys.nframe())
# print(ls(envir=sys.frame(-1)))
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
#
xt <- h(x0, x1, t, when,p=p)
yt <- h(y0, y1, t, when,p=p)
if(trace)
segments(x0,y0,xt,yt ,col=trace.col)
    points(xt, yt,...)
    invisible(list(x=xt,y=yt))
}

## ----makeTransparent-----------------------------------------------------
#' Make Transparent Colors
#'
#' Add transparency parameter (alpha) to colors
#'
#' @param x vector specifying colors or factor.
#'   Colors can be specified as numbers or
#'   character strings, see \code{\link{colors}}.
#'   Factors are also acceptable
#'   in which case the input will be transformed into level numbers.
#' @param alpha numeric, transparency value \code{\link{rgb}}.
#' @return A character vector with elements of 7 or 9 characters, "#"
#'   followed by the red, blue, green and optionally alpha
#'   values in hexadecimal (after rescaling to 0 ... 255).
#'   The optional alpha values range from 0
#'   (fully transparent) to 255 (opaque).
#' @export
#' @seealso \code{\link[grDevices]{rgb}} for setting colors and
#'   \code{\link[grDevices]{colors}} for color names.
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' # Transparent red
#' makeTransparent("red",0.5)
#' # Same thing
#' makeTransparent(2,0.5)
#' # Vector of names
#' makeTransparent(c("red","orange"),0.75)
#' # Vector of color numbers
#' makeTransparent(1:5)
#' # Factor levels can be used as color indicator
#' makeTransparent(factor(c("S","M","H","S")))
#' # Different transparency
#' makeTransparent(1:5,alpha=1:5)
#' #
#' # Define transparent version of base colors
#' #
#' trcols<-makeTransparent(c(1:8,0.1),alpha=0.75)
#' trcols <- data.frame(t(trcols),stringsAsFactors=FALSE)
#' names(trcols) <-
#' paste("tr",c("black","red","green","blue",
#' "cyan","magenta","yellow","grey","white"),sep="")
#' str(trcols)
#' attach(trcols)
#' trred
#' trgreen
#' detach()
#' # Plot colors
#' if(interactive()){
#' par(xpd=TRUE)
#' newplot(stamp=FALSE)
#' points(1:9,1:9,bg=unlist(trcols),col="grey",pch=21,cex=15)
#' text((1:9)+1.1,(1:9)-0.7,names(trcols),adj=0)
#' }
#'
makeTransparent <- function(x, alpha=0.5){
if(is.factor(x)) x <- as.numeric(x)
if(max(alpha,na.rm=TRUE) > 1) alpha <- alpha/max(alpha,na.rm=TRUE)
y <- rbind(col2rgb(x)/255,alpha=alpha)
apply(y,2,function(x) rgb(x[1],x[2],x[3],x[4]))
}

## ----fct trgb------------------------------------------------------------
#' Interpolate Colors
#'
#' This function interpolates the color between start and end color.
#'
#' @param x0 vector specifying start colors or factor.
#'   Colors can be specified as numbers or
#'   character strings, see \code{\link{colors}}.
#'   Factors are also acceptable
#'   in which case the input will be transformed into level numbers.
#' @param x1 vector specifying end colors or factor.
#'   Colors can be specified as numbers or
#'   character strings, see \code{\link{colors}}.
#'   Factors are also acceptable
#'   in which case the input will be transformed into level numbers.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param alpha0,alpha1 numeric, transparency value
#'   \code{\link[grDevices]{rgb}}.
#' @param ... additional arguments passed to \code{\link[grDevices]{rgb}}.
#' @return A character vector with elements of 7 or 9 characters, "#"
#'   followed by the red, blue, green and optionally alpha
#'   values in hexadecimal (after rescaling to 0 ... 255).
#'   The optional alpha values range from 0
#'   (fully transparent) to 255 (opaque).
#' @export
#' @seealso \code{\link[grDevices]{rgb}} for setting colors and
#'   \code{\link[grDevices]{colors}} for color names.
#'
#'   See \code{link{makeTransparent}} to add transparency to the colors.
#'
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive()){
#' animator('
#' newplot()
#' points(5,5,col=trgb("red","blue",t,alpha1=1),pch=16,cex=tcex(80,160,t))',
#' life=1)
#' }
#
trgb <- function(x0, x1=x0, t, when=c(0,1), p =1, alpha0=1, alpha1=1,...){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    if(is.factor(x0)) x0 <- as.numeric(x0)
    if(is.factor(x1)) x1 <- as.numeric(x1)
    if(is.character(x0)) x0 <- col2rgb(x0)
    if(is.character(x1)) x1 <- col2rgb(x1)
    xt <- h(x0/255,x1/255,t,when,p=p)
    alpha <- h(alpha0,alpha1,t,when,p=p)
#    cat(x1,t,when,"\n")
#    print(str(x1))
    invisible(rgb(xt[1],xt[2],xt[3],alpha,...))
}
if(interactive()){
animator('
newplot()
points(5,5,col=trgb("red","blue",t,alpha1=1),pch=16,cex=tcex(80,160,t))
',life=1)
}

## ----fct tlines----------------------------------------------------------
#' Move Lines
#'
#' Move lines from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... additional arguments passed to \code{\link{lines}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position
#' @export
#' @seealso \code{\link[graphics]{lines}}
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5,10)
#' y0 <- c(0,10,0)
#' x1 <- c(10,5,0)
#' y1 <- c(10,0,10)
#' newplot()
#' pos <- tlines(x0,y0,x1,y1,0.25)
#' points(pos)
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tlines(x0,y0,x1,y1,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
tlines <-
function(x0, y0, x1, y1, t, p=1, when, ...) {
#function(x0, y0, x1, y1, t0=0, t1=t0, t, when,dt=.dt,...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
#
#    for(t in seq(t0,t1,dt)) {
#    if(!add) newplot(axes=FALSE)
    xt <- h(x0, x1, t,when,p=p)
    yt <- h(y0, y1, t,when,p=p)
    lines(xt, yt ,...)
    invisible(list(x=xt,y=yt))
}

## ----fct dsegments-------------------------------------------------------
#' Draw Segments
#'
#' Draw segments from \eqn{(x_0,y_0)} to \eqn{(x_1,y_1)}. The effect is
#' like starting from a point and draw a line.
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... additional arguments passed to \code{\link{segments}}.
#' @return List with numerical components \code{x} and \code{y} giving
#'   current position.
#' @export
#' @seealso \code{\link[graphics]{segments}}, \code{\link{tsegments}}
#'   for moving segments and \code{\link{darrows}} for drawing arrows.
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5,10)
#' y0 <- c(0,10,0)
#' x1 <- c(10,5,0)
#' y1 <- c(10,0,10)
#' newplot()
#' pos <- dsegments(x0,y0,x1,y1,0.25)
#' points(pos)
#' ## Lines from 0 to 1
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' dsegments(x0,y0,x1,y1,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
#' ## Reverse: from 1 to 0
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' dsegments(x1,y1,x0,y0,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
dsegments <-
function(x0, y0, x1, y1, t, when, p=1,  ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt <- h(x0, x1, t,when,p)
    yt <- h(y0, y1, t,when,p)
#
    segments(x0, y0, xt, yt,...)
    invisible(list(x=xt,y=yt))
}

## ----fct tsegments-------------------------------------------------------
#' Move Segments
#'
#' Change segment defining positions and plot subsequent segments.
#' Lines are "floating" to final positions.
#'
#' @param x0,x1 numeric vectors, start segments defining x coordinates.
#' @param y0,y1 numeric vector, start segments defining y coordinates.
#' @param x2,x3 numeric vector, end segments defining x coordinates.
#' @param y2,y3 numeric vector, end segments defining y coordinates.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param fixed numeric, which location is fixed:
#'   start (0, default) - draw from \code{(x0,y0)} to \code{(x1,y1)},
#'   or end (1) - reverse.
#' @param ... additional arguments passed to \code{\link{segments}}.
#' @return Numerical matrix with columns defining the current segments.
#' @export
#' @seealso \code{\link[graphics]{segments}}, \code{\link{dsegments}}
#'   for drawing segments and \code{\link{tarrows}} for moving arrows.
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,4,7)
#' y0 <- c(0,7,0)
#' x1 <- c(4,0,7)
#' y1 <- c(2,7,5)
#' x2 <- x0+1
#' y2 <- y0+2
#' x3 <- x1+1
#' y3 <- y1+2
#' newplot()
#' arrows(x0,y0,x2,y2,lty=2)
#' arrows(x1,y1,x3,y3,lty=2)
#' segments(x0,y0,x1,y1,col=2)
#' pos <- tsegments(x0,y0,x1,y1,x2,y2,x3,y3,0.75,lwd=3)
#' segments(x2,y2,x3,y3,col=4)
#' pos
#' points(pos$start)
#' points(pos$end)
#' d <- 0.2
#' points(x0-d,y0+d,pch="0")
#' points(x1+d,y1+d,pch="1")
#' points(x2-d,y2+d,pch="2")
#' points(x3+d,y3+d,pch="3")
tsegments <-
function(x0, y0, x1, y1, x2=x0, y2=y0, x3=x1, y3=y1, t, when, p=1, fixed=0, ...) {
X <- cbind(x0, y0, x1, y1, x2, y2, x3, y3)
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt0 <- h(X[,1], X[,5], t,when,p)
    yt0 <- h(X[,2], X[,6], t,when,p)
    xt1 <- h(X[,3], X[,7], t,when,p)
    yt1 <- h(X[,4], X[,8], t,when,p)
#
    segments(xt0, yt0, xt1, yt1,...)
    invisible(list(start=list(x=xt0,y=yt0),
    end=list(x=xt1,y=yt1)))
}

## ----fct tsegments2------------------------------------------------------
thline <-
function(x0, y0, x1, y1, t, when, fixed=1, ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    switch(fixed,
    segments(x0,h(y0, y1, t,when),x1, h(y0, y1, t,when),...),
    segments(h(x0, x1, t, when), h(y0, y1, t, when),x1,y1,...))
}

## ----fct darrows---------------------------------------------------------
#' Draw Arrows
#'
#' Draw arrows from start to end location. The effect is
#' like starting from a point and draw an arrow on the screen.
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param length length of the edges of the arrow head (in inches).
#' @param ... additional arguments passed to \code{\link{arrows}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position.
#' @export
#' @seealso \code{\link[graphics]{arrows}}, \code{\link{tarrows}}
#'   for moving arrows and \code{\link{dsegments}} for drawing segments.

#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5,10)
#' y0 <- c(0,10,0)
#' x1 <- c(10,5,0)
#' y1 <- c(10,0,10)
#' newplot()
#' pos <- darrows(x0,y0,x1,y1,0.25)
#' points(pos)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' #############
#' if(interactive()){
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' darrows(x0,y0,x1,y1,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
#' }
#' #############
#' if(interactive()){
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' darrows(x1,y1,x0,y0,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
#' }
#
darrows <-
function(x0, y0, x1, y1, t, when,p=1, length=0.125, ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt <- h(x0, x1, t,when,p)
    yt <- h(y0, y1, t,when,p)
    arrows(x0,y0,xt,yt,length=length,...)
    invisible(list(x=xt,y=yt))
}

## ----fct tarrows---------------------------------------------------------
#' Move Arrows
#'
#' Move arrows from start to end location
#'
#' @param x0,x1 numeric vectors, start arrow defining x coordinates.
#' @param y0,y1 numeric vector, start arrow defining y coordinates.
#' @param x2,x3 numeric vector, end arrow defining x coordinates.
#' @param y2,y3 numeric vector, end arrow defining y coordinates.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param length length of the edges of the arrow head (in inches).
#' @param ... additional arguments passed to \code{\link{arrows}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position
#' @export
#' @seealso \code{\link[graphics]{arrows}}, \code{\link{darrows}}
#'   for drawing arrows and \code{\link{tsegments}} for moving segments.
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,4,7)
#' y0 <- c(0,7,0)
#' x1 <- c(4,0,7)
#' y1 <- c(2,7,5)
#' x2 <- x0+1
#' y2 <- y0+2
#' x3 <- x1+1
#' y3 <- y1+2
#' newplot()
#' arrows(x0,y0,x2,y2,lty=2)
#' arrows(x1,y1,x3,y3,lty=2)
#' arrows(x0,y0,x1,y1,col=2)
#' pos <- tarrows(x0,y0,x1,y1,x2,y2,x3,y3,0.75,lwd=3)
#' arrows(x2,y2,x3,y3,col=4)
#' pos
#' points(pos$start)
#' points(pos$end)
#' d <- 0.2
#' points(x0-d,y0+d,pch="0")
#' points(x1+d,y1+d,pch="1")
#' points(x2-d,y2+d,pch="2")
#' points(x3+d,y3+d,pch="3")
tarrows <-
function(x0, y0, x1, y1, x2=x0, y2=y0, x3=x1, y3=y1, t, when, p=1, length=0.125, ...) {
X <- cbind(x0, y0, x1, y1, x2, y2, x3, y3)
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt0 <- h(X[,1], X[,5], t,when,p)
    yt0 <- h(X[,2], X[,6], t,when,p)
    xt1 <- h(X[,3], X[,7], t,when,p)
    yt1 <- h(X[,4], X[,8], t,when,p)
#
    arrows(xt0, yt0, xt1, yt1,...)
    invisible(list(start=list(x=xt0,y=yt0),
    end=list(x=xt1,y=yt1)))
}

## ----fct tpolygon--------------------------------------------------------
#' Move Polygon
#'
#' Move polygon from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... additional arguments passed to \code{\link{polygon}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position
#' @export
#' @seealso \code{\link[graphics]{polygon}}
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5)
#' y0 <- c(0,5)
#' x1 <- c(1,10)
#' y1 <- c(1,10)
#' print(tpolygon(x0,y0,x1,y1,0.5))
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tpolygon(x0,y0,x1,y1,t)
#' }

tpolygon <-
function(x0, y0, x1, y1, t,when,p=1,...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt <- h(x0, x1, t,when,p)
    yt <- h(y0, y1, t,when,p)
    polygon(xt, yt,...)
    invisible(list(x=xt,y=yt))
}

## ----fct trect-----------------------------------------------------------
#' Morf Rectangle
#'
#' Morf one rectangle to another
#'
#' @param  xleft0   a vector (or scalar) of left x positions.
#' @param  ybottom0 a vector (or scalar) of bottom y positions.
#' @param  xright0  a vector (or scalar) of right x positions.
#' @param  ytop0    a vector (or scalar) of top y positions.
#' @param  xleft1   a vector (or scalar) of left x positions.
#' @param  ybottom1 a vector (or scalar) of bottom y positions.
#' @param  xright1  a vector (or scalar) of right x positions.
#' @param  ytop1    a vector (or scalar) of top y positions.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... additional arguments passed to \code{\link{rect}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position
#' @export
#' @seealso \code{\link[graphics]{rect}} and
#'   \code{\link{tpolygon}} for moving a polygon.
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' xleft0   <- 1
#' ybottom0 <- 1
#' xright0  <- 5
#' ytop0    <- 5
#' xleft1   <- 3
#' ybottom1 <- 3
#' xright1  <- 6
#' ytop1    <- 9
#' newplot()
#' rect( xleft0, ybottom0, xright0, ytop0,lty=2,border=2)
#' rect( xleft1, ybottom1, xright1, ytop1,lty=2,border=4)
#' arrows(
#' c(xleft0,xright0),c(ybottom0,ytop0),
#' c(xleft1,xright1),c(ybottom1,ytop1),lty=2)
#' ## Intermediate rectangle
#' pos <- trect(xleft0, ybottom0, xright0, ytop0,
#'                 xleft1, ybottom1, xright1, ytop1, t= 0.75)
#' str(pos)
#' #############
trect <- function(xleft0, ybottom0, xright0, ytop0,
                 xleft1, ybottom1, xright1, ytop1, t, p=1, when, ...){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
xleftt <- h(xleft0,xleft1,t, when,p)
ybottomt <- h(ybottom0,ybottom1,t,when,p)
xrightt <- h(xright0,xright1,t,when,p)
ytopt <- h(ytop0,ytop1,t,when,p)
rect(xleftt, ybottomt,
xrightt,ytopt,...)
invisible(list(xleft=xleftt,ybottom=ybottomt,
xright=xrightt,ytop=ytopt))
}

## ----fct tcex------------------------------------------------------------
#' Interpolate Symbol and Text Size
#'
#' This function interpolates the symbol size
#' between the start and end size.
#'
#' @param cex0 numeric vector specifying start symbol size,
#'   see \code{\link[graphics]{par} cex}.
#' @param cex1 numeric vector specifying end symbol size,
#'   see \code{\link[graphics]{par} cex}.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param area logical, if TRUE (default) sizes will be treated as
#'   area. If FALSE, sizes will be considered by diameter.
#' @return A numeric vector giving the amount by which plotting text
#'   and symbols should be magnified relative to the
#'   default \code{cex} value.
#' @export
#' @seealso \code{\link[graphics]{par}} for
#'   setting symbol sizes (\code{cex}).
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive()){
#' animator('
#' newplot()
#' points(3:7,3:7,col=1:5,
#' pch=16,cex=tcex(0,20,t),life=0.5)
#' ')
#' }
#' if(interactive()){
#' # Doubling the symbol sizes
#' newplot()
#' text(2,8,"By edge:\n cex = c(8, 16)")
#' points(2,5,cex=16,pch=0)
#' points(2-0.75*par("cin")[2]*4,
#' 5-0.75*par("cin")[2]*4,cex=8,pch=15, col="grey")
#' text(8,8,"By size:\n cex = sqrt(c(4, 16, 64))")
#' points(8,5,cex=sqrt(64*4),pch=0)
#' points(8,5,cex=sqrt(16*4),pch=15, col="grey")
#' points(8,5,cex=sqrt(4*4),pch=0)
#' abline(v=c(2,8),h=5,col="red")
#' }
#
# homotopy change of cex
# set size po maintain proportional areas
tcex <- function(cex0=1, cex1=1, t, when, p, area=TRUE){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    if(area) cext <- sqrt(h(cex0,cex1,t,when)) else
    cext <- h(cex0,cex1,t,when,p)
    invisible(cext)
}


## ----fct ttext-----------------------------------------------------------
#' Move Text
#'
#' Move text from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param text character vector, tect to display.
#' @param ... additional arguments passed to \code{\link{text}}.
#' @return List with numerical components \code{x} and \code{y} with
#'   current position.
#' @export
#' @seealso \code{\link[graphics]{text}}
#' @keywords graphics dynamic aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5)
#' y0 <- c(0,5)
#' x1 <- c(1,10)
#' y1 <- c(1,10)
#' print(tpoints(x0,y0,x1,y1,0.5))
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' ttext(x0,y0,x1,y1,t,text=paste("Test text",t))
#' }
ttext <- function(x0, y0, x1=x0, y1=y0, t, when, p, text="",...){# print(sys.nframe())
# print(ls(envir=sys.frame(-1)))
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
#
xt <- h(x0, x1, t, when,p=p)
yt <- h(y0, y1, t, when,p=p)
    text(xt, yt, text, ...)
    invisible(list(x=xt,y=yt))
}
#
ttext <- function(x0, y0, x1=x0, y1=y0, t, when, text="",...){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    text(h(x0,x1,t,when),h(y0,y1,t,when),text,...)
}

## ----fct tmatrix---------------------------------------------------------
#' Interpolate Matrix
#'
#' Interpolates matrix elements between start and end value.
#'
#' @param x0 numerical matrix, start matrix
#' @param x1 numerical matrix, end matrix
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return Intermediate matrix (numerical).
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- matrix(c(1,2,3,4),2,2)
#' x1 <- matrix(c(11,12,13,14),2,2)
#' print(x0)
#' print(x1)
#' print(h(x0,x1,t=0.3))
#
# homotopy change of matrix
# set size po maintain proportional areas
# Warning: swapped arguments x0 and x1
tmatrix <- function(x0, x1=diag(nrow(x0)), t, when, p = 1){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    invisible(h(x0,x1,t,when, p))
}

## ----fct tBoxCox---------------------------------------------------------
#' Box-Cox Transformation with Interpolated Parameter \eqn{lambda}
#'
#' Transform data using the Box-Cox transformation with
#' parameter \eqn{lambda} interpolated between start and end value.
#'
#' @param x numerical vector.
#' @param lambda0 numeric, starting value of parameter lambda.
#' @param lambda1 numeric, end value of parameter lambda.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return numerical vector, Box-Cox transformed
#'   values for current interpolated value of lambda.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive()) {
#'  x <- runif(250,1,3)
#'  for (t in seq(0,1,length=100)){
#'  newplot(xlim=c(0,10),ylim=c(0,1),axes=TRUE,asp=NA)
#'  xt <- tBoxCox(x,-1,3,t)
#'  rug(xt)
#'  lines(density(xt))
#'  }
#' }
# homotopy change of parameter lambda in boxcox transform
tBoxCox <- function(x,lambda0,lambda1=lambda0, t, when ,p){
BoxCox <- function(x,lambda=1){
if(lambda!=0) return((x^lambda-1) / lambda) else return(log(x))
}
if(missing(t)) t <- get("t",envir=sys.frame(-1))
   invisible(BoxCox(x,h(lambda0,lambda1,t,when=when)))
}
#
if(interactive()) {
 x <- runif(250,1,3)
 for (t in seq(0,1,length=100)){
 newplot(xlim=c(0,10),ylim=c(0,1),axes=TRUE,asp=NA)
 xt <- tBoxCox(x,-1,3,t)
 rug(xt)
 lines(density(xt))
 }
}


## ------------------------------------------------------------------------
test <- function(x) {
xx <- parse(text=x)
as.character(attr(xx, "wholeSrcref"))
}
if(.testing) test({print(1)
print("a+b")
})

## ------------------------------------------------------------------------
if(.testing){
for(i in 1:1){
t0 <- Sys.time()
tLife <- 1.1
t <- 0
i <- 0
newplot()
while(t<=1) {
t=as.numeric((Sys.time()-t0))/tLife
newplot()
#text(1,1,(Sys.time()-t0))
#text(1,2,i)
i <- i+1
tpoints(2, 2, 9,5, t,pch=16,cex=2,trace=TRUE)
#Sys.sleep(.00)
}
print(i)
}
}

## ----fct animator--------------------------------------------------------
#' Plot Block of Animated Commands.
#'
#' Make and plot animated sequence of figures.
#'
#' @aliases as.animator is.animator
#' @param block,x character, expression or
#'   object of class \code{animator} containing  graphical timed commands.
#' @param life numerical, duration of animation.
#' @param fps numerical, frames per second.
#' @param pause numerical, length of the pause between plotted frames.
#' @param verbose logical, if TRUE print animation characteristics.
#' @return Function \code{animator} plots the animation and
#'   returns an object of class \code{animator}.
#'
#'  \code{as.animator} attempts to add the class
#'  \code{animator} to the argument \code{x}.
#'
#'  \code{is.animator} returns \code{TRUE} if \code{x}
#'  is an \code{animator} object and \code{FALSE} otherwise.
#'
#' @seealso \code{\link{as.animator}}, \code{\link{is.animator}}.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive())
#' animator("newplot();tpoints(2,2,5,8,cex=2,pch=16)",life=2,verbose=TRUE)
#' # block
#' animator({
#'   newplot()
#'   abline(h=c(1,8),v=c(1,8))
#'  tpoints(1,1,8,8,pch=16,cex=2)
#' })
#' t <- 0
#' as.animator('{
#'   newplot()
#'   abline(h=c(1,8),v=c(1,8))
#'   tpoints(1,1,8,8,pch=16,cex=2)
#' }')
#
animator <- function(block, life=1,fps=25,pause=0.5,verbose=FALSE){
#block <- deparse(substitute(block))
if(is.na(pause)) pause=0.1
t0 <- Sys.time()
t <- 0
life <- life
life1 <- 1/life
pause <- pause/fps
ts <- c(0,seq(0,1,length=(life*fps)),1)
for(t in ts){
    if (dev.interactive()) dev.hold()
#repeat{
#while((Sys.time()-t0)<=life) {
    if(is.expression(block))
        { eval(block)}
    if(is.character(block))
        { eval(parse(text=block))}
    if (dev.interactive()) {
        dev.flush()
        if(!is.na(pause)) Sys.sleep(pause)
    }
#    t <- min(1,as.numeric((Sys.time()-t0))*life1)
#    t <- as.numeric((Sys.time()-t0))*life1

#    if((t>1)) break
}
Life <- Sys.time()-t0
if(verbose) cat(
"\nTime  :",Life,
"\nFrames:",length(ts),
"\nF/s   :",round(length(ts)/as.numeric(Life)),
"\ndt    :",ts[3]-ts[2],"\n")
#attr(block,"class") <- "animator"
#attr(block,"life") <- life
invisible(as.animator(block,life))
}
#' @rdname animator
#' @export
as.animator <- function(x,life=1){
t <- 0
if(!is.animator(x)) {
 if(!is.character(x)) x <- deparse(substitute(x))
 class(x) <- "animator"
 attr(x,"life") <- life
 }
 invisible(x)
 }
## Tests for object class
#' @rdname animator
#' @export
is.animator <- function(x){
 return(class(x) == "animator")
 }
##

## ----fct plot.animator---------------------------------------------------
#' Plot Method for Class \code{animator}.
#'
#' Performs and plots the animation of an object.
#'
#' @param x character, expression (block) or
#'   object of class \code{animator} containing  graphical timed commands.
#' @param life numerical, duration of animation.
#' @param ... additional arguments passed to function \code{animator}.
#' @return Object of class \code{animator}.
#' @seealso \code{\link{as.animator}}, \code{\link{is.animator}}.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x <- as.animator(
#' "newplot();tpoints(2,2,5,8,cex=2,pch=16)", life=2)
#' print(x)
#' if(interactive()) plot(x)
#' ## Equivalent
#' x <- as.animator(expression({
#' newplot()
#' tpoints(2,2,5,8,cex=2,pch=16)
#' }),
#' life=2)
#' print(x)
#'if(interactive()) plot(x)
#
plot.animator <- function(x,life,...) {
if(missing(life)) life <- attr(x,"life")
animator(x,life=life,...)
}

## ------------------------------------------------------------------------
#' Test if \code{knitr} is Active
#'
#' Are you \code{knitr}?
#'
#' @return value \code{TRUE} if \code{knitr} engine is active,
#'   \code{FALSE} otherwise
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @export
#' @examples
#' \dontrun{
#' is_knitr()
#' }
is_knitr <- function() {
"params.src" %in% names(getChunkopts())
}


## ----getChunkopts--------------------------------------------------------
#' Get Chunk Options.
#'
#' Get \code{Sweave} or \code{knitr} programme chunk options.
#'
#' @param what character, name of the options to extract.
#'   If missing, all options will be returned.
#' @return \code{Sweave} or \code{knitr} chunk option with name
#'   as declared by argument \code{what}.
#'   If \code{what} is missing, a list with all chunk options.
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @export
#' @examples

#' \dontrun{
#' getChunkopts()
#' }
getChunkopts<-function(what){
    in.sweave <<-FALSE
    if ((n.parents <- length(sys.parents())) > 3) {
        for (i in seq_len(n.parents) - 1) {
#       cat(1,ls(envir = sys.frame(i)),"\n\n")
            if ("chunkopts" %in% ls(envir = sys.frame(i))) {
                chunkopts = get("chunkopts", envir = sys.frame(i))
                if (all(c("prefix.string", "label") %in% names(chunkopts))) {
                  img.name = paste(chunkopts$prefix.string, chunkopts$label,sep = "-")
#                 ani.options(img.fmt = file.path(outdir, paste(img.name,
#                    "%d.", file.ext, sep = "")))
                  in.sweave <<- TRUE
                  if(missing(what)) return(chunkopts) else return(chunkopts[what])
                  break
                }
            }
        }
    }
    if(missing(what)) return(opts_current$get()) else
    return(opts_current$get()[what])
}
#if(.testing) getChunkopts("label")
#if(.testing) str(getChunkopts())

## ----includeLatex--------------------------------------------------------
#' Include Animated Graphics.
#'
#' Provides a line to embed a stack of graphic frames
#' into PDF file
#' as animated graphics. It is an interface for LaTeX command
#' \code{\\animategraphics} from LaTeX package \code{animate}.
#'
#' @param title character, obsolete.
#' @param file character,  the leftmost part of the file name that
#'   is common to all members of the sequence. If \code{file} is
#'   equal to \code{NA}, \code{file} will be constructed from the
#'   code graphics prefix and chunk label. See Note.
#' @param scale numeric, scaling factor, See Note.
#' @param poster character or numeric,
#'   ['first' | <num> | 'last' | 'none']
#'   Specifies which frame to display and print if
#'   the animation is not activated. The first frame is shown by default.
#'   Thus 'poster' or 'poster=first' need not be explicitly set.
#'   A frame number <num> may as well be given; <num> is zero-based,
#'   that is, the first frame has number '0'. See Note.
#' @param every numeric. Build animation from every 'every'th frame only.
#'   Skipped frames are discarded and not embedded into the document.
#'   See Note.
#' @param fps numeric,  the animation frame rate (frames per second).
#'   Assigning values less than zero results in an error.
#'   Default is 25. See Note.
#' @param first numeric, first frame to use in animation.
#'   Special case is default character value "",
#'   equivalent to 1. See Note.
#' @param last numeric, last frame to use in animation.
#'   Special case is default character value "",
#'   equivalent to last, unknown frame number. See Note.
#' @param vspace character, vertical space (LaTeX style)
#'    for graph positioning. Default is "0pt", no additional space.
#' @param other character, other parameters separated by coma and
#'   passed to LaTeX package \code{animate} See Note.
#' @return prints and invisibly returns the
#'   LaTeX \code{\\animategraphics} command.
# @import knitr
#' @note Arguments (except \code{title} and
#'   \code{vspace}) are used to communicate
#'   with LaTeX package \emph{animate}.
#'   For details see documentation for the LaTeX package
#'   \href{https://www.ctan.org/pkg/animate}{animate}.
#'   Be aware that this is not the same as R package
#'   \code{\link[animation]{animation}}.
#'
#'   When used with Sweave or knitr, the argument \code{file}
#'   need not to be set and will be conveniently constructed from the
#'   graphical prefix and chunk label.
#'
#'   Argument \code{vspace} can be useful for raising the
#'   animation image on slides (e.g. \code{beamer}).
#' @seealso \code{\link{animator}},
#'   LaTeX package \code{animate} (\url{https://www.ctan.org/pkg/animate}),
#'   R package \code{animation}
#'   (\url{https://cran.r-project.org/web/packages/animation)}.
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @export
#' @examples
#' \dontrun{
#' includeLatex("Test animation")
#' includeLatex("Test animation",scale=0.5,poster="last",other="loop")
#' # To include from specific file with stacked frames ( e. g. 0 .. 99 )
#' # Useful to include animations that were prepared before and can
#' # be reused in another file
#' includeLatex(file="./figs/PreparedBefore",first=10,last=50)
#' }
includeLatex <- function(title="",file=NA,scale=0.5,poster="first",every=1,fps=25,first="",last="",
vspace="0pt",other="controls"){
# Old way, maybe needed for Sweave ?
# file <- paste(getChunkopts()[c("prefix.string","label")],collapse="-")
# New way, works for knitr
if(fps < 0 ) stop ("Argument fps should be nonnegative")
if(is.na(file)) {
if(is_knitr()) {
file <- fig_chunk(
label = opts_current$get()$label, ext = "")
} else {
file <- paste(getChunkopts()[c("prefix.string","label")],collapse="-")
}
}
#print(file)
#print(getChunkopts())
#cat("\n\\begin{frame}[fragile] ","\n")
#cat("\\frametitle{",title,"} ","\n")
#cat("\\begin{center} ","\n")
cat(" \\vspace{",vspace,"} ","\n",sep="")
cmd <- paste("\\animategraphics[scale=",scale,
",poster=",poster,
",every=",every,
",",other,
"]{",fps,
"}{",file,
"}{",first,
"}{",last,
"} ","\n",sep="")
cat(cmd)
invisible(cmd)
#cat("\\end{center} ","\n\n\n")
#cat("\\end{frame} ","\n\n")
}
.testing=TRUE
#if(.testing) includeLatex("poskusni izpis")
#if(.testing) includeLatex("poskusni izpis",other="autoplay")
#(chunkName <- opts_current$get()$params.src)
#includeLatex("Test animation")

