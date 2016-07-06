## ----echo=FALSE----------------------------------------------------------
###################################################
##                                               ##
## (c) Andrej Blejec (andrej.blejec@nib.si) new     ##
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
#' results that change in a linear succession
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
#' This parameter can be considered as fraction of time
#' duration of the animation.
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
#' Coordinate arguments
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
#' @param xlim see \code{\link{plot}}
#' @param ylim see \code{\link{plot}}
#' @param ann see \code{\link{plot}}
#' @param axes see \code{\link{plot}}
#' @param type see \code{\link{plot}}
#' @param asp aspect ratio, default y/x = 1 see \code{\link{plot.new}}
#' @param stamp logical, should time be visible on plots.
#' @param ... see \code{\link{plot}}
#' @return NULL
#' @export
#' @import graphics
#' @import grDevices
#' @seealso \code{\link{plot}}
#' @keywords  dynamic, hplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' newplot()
#' points(0.5,0.5)
#'
newplot <-
function(xlim=c(0,10),ylim=c(0,10),ann=FALSE,axes=FALSE,type="n",asp=1,
stamp=FALSE,...){
plot(0,0,xlim=xlim,ylim=ylim,ann=ann,axes=axes,type=type,asp=asp,...)

if(stamp) text(par("usr")[2]*0.8,par("usr")[4]*1.05,paste("t =",
round(get("t",envir=sys.frame(-1)),2)),adj=0,xpd=TRUE)
}

## ------------------------------------------------------------------------
#' Homotopy Function
#'
#' Interpolates a position between start and end value(s).
#' Homotopy controled by a homotopy parameter \code{t} and power parameter \code{p} is used for interpolation.
#'
#' @param x0 numeric vector of start values.
#' @param x1 numeric vector of end values.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return interpolated value (see Note)
#' @export
#' @note Returned coordinates are determined using the homotopy function
#'   \deqn{x_t=x_0  (1-t^p)+x_1 t^p,  t\in[0,1]}.
#' @keywords dynamic
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' h(1, 2, 0)
#' h(1, 2, 1)
#' h(1, 2, .5)
#' h(1,c(2,3),.5)
#'
h <-
function(x0, x1, t, when, p=1) {
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
#' @param ... other parameters passed to \code{\link{points}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{points}}
#' @keywords dynamic, aplot
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

## ----tcols---------------------------------------------------------------
#' Make Transparent Colors
#'
#' Add transparency parameter (alpha) to colors
#'
#' @param x vector specifying colors or factor.
#' Colors can be specified as numbers or
#' character strings, see \code{\link{colors}}.
#' Factors are also acceptable
#' in which case the input will be transformed into level numbers.
#' @param alpha numeric, transparency value \code{\link{rgb}}.
#' @return A character vector with elements of 7 or 9 characters, "#"
#' followed by the red, blue, green and optionally alpha
#' values in hexadecimal (after rescaling to 0 ... 255).
#' The optional alpha values range from 0
#' (fully transparent) to 255 (opaque).
#' @export
#' @seealso \code{\link[grDevices]{rgb}} for setting colors and
#'  \code{\link[grDevices]{colors}} for color names.
#' @keywords dynamic, aplot
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
#' @param x0 numeric or character vector specifying start color,
#' see \code{\link[grDevices]{colors}}.
#' @param x1 numeric or character vector specifying end color,
#' see \code{\link[grDevices]{colors}}.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param alpha0,alpha1 numeric, transparency value
#' \code{\link[grDevices]{rgb}}.
#' @param ... additional parameters passed to \code{\link[grDevices]{rgb}}.
#' @return A character vector with elements of 7 or 9 characters, "#"
#' followed by the red, blue, green and optionally alpha
#' values in hexadecimal (after rescaling to 0 ... 255).
#' The optional alpha values range from 0
#' (fully transparent) to 255 (opaque).
#' @export
#' @seealso \code{\link[grDevices]{rgb}} for setting colors and
#'  \code{\link[grDevices]{colors}} for color names.
#' @keywords dynamic, aplot
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive()){
#' animator('
#' newplot()
#' points(5,5,col=trgb("yellow","blue",t,alpha1=1),
#' pch=16,cex=tcex(0,20,t),life=0.5)
#' ')
#' }
#
trgb <- function(x0, x1=x0, t, when=c(0,1), p =1, alpha0=1, alpha1=1,...){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
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
points(5,5,col=trgb("yellow","blue",t,alpha1=1),pch=16,cex=tcex(0,20,t),life=0.5)
')
}

## ----fct tlines----------------------------------------------------------
#' Move lines
#'
#' Move lines from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... other parameters passed to \code{\link{lines}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{lines}}
#' @keywords graphics
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
#' Draw segments from (x0,y0) to (x1,y1). The effect is
#' like starting from a point and draw a line.
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param ... other parameters passed to \code{\link{segments}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{segments}}
#' @keywords graphics
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
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param fixed numeric, which location is fixed:
#' start (0, default) - draw from \code{(x0,y0)} to \code{(x1,y1)},
#' or end (1) - reverse.
#' @param ... other parameters passed to \code{\link{segments}}.
#' @return Numerical matrix with colums defining the current segments.
#' @export
#' @seealso \code{\link[graphics]{segments}}
#' @keywords graphics
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,4,7)
#' y0 <- c(0,7,0)
#' x1 <- c(4,0,7)
#' y1 <- c(4,7,5)
#' x2 <- c(0,4,7)+2
#' y2 <- c(0,7,0)-2
#' x3 <- c(4,0,7)+2
#' y3 <- c(4,7,5)-2
#' newplot()
#' arrows(x0,y0,x1,y1,lty=2)
#' arrows(x2,y2,x3,y3,lty=2)
#' pos <- tsegments(x0,y0,x1,y1,x2,y2,x3,y3,0.75)
#' pos
#' points(pos$start)
#' points(pos$end)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' points(x2,y2,pch="2")
#' points(x3,y3,pch="3")
tsegments <-
function(x0, y0, x1, y1, x2=x0, y2=y0, x3=x1, y3=y1, t, when, p=1, fixed=0, ...) {
X <- cbind(x0, y0, x1, y1, x2, y2, x3, y3)
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    xt0 <- h(X[,1], X[,3], t,when,p)
    yt0 <- h(X[,2], X[,4], t,when,p)
    xt1 <- h(X[,5], X[,7], t,when,p)
    yt1 <- h(X[,6], X[,8], t,when,p)
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
#' Draw arrows from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param length length of the edges of the arrow head (in inches).
#' @param ... other parameters passed to \code{\link{arrows}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{arrows}}
#' @keywords graphics
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
#' Draw Arrows
#'
#' Draw arrows from start to end location
#'
#' @inheritParams coordParams
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @param fixed numeric, which location is fixed:
#' start (0, default) - draw from \code{(x0,y0)} to \code{(x1,y1)},
#' or end (1) - reverse.
#' @param length length of the edges of the arrow head (in inches).
#' @param ... other parameters passed to \code{\link{arrows}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{arrows}}
#' @keywords graphics
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' x0 <- c(0,5,10)
#' y0 <- c(0,10,0)
#' x1 <- c(10,5,0)
#' y1 <- c(10,0,10)
#' print(tsegments(x0,y0,x1,y1,0.5))
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tsegments(x0,y0,x1,y1,t)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tsegments(x0,y0,x1,y1,t,fixed=1)
#' points(x0,y0,pch="0")
#' points(x1,y1,pch="1")
#' }
tarrows <-
function(x0, y0, x1, y1, t, when,p=1, fixed=1, length=0.125, ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    if(fixed) t <- 1-t
    xt <- h(x0, x1, t,when,p)
    yt <- h(y0, y1, t,when,p)
    switch(fixed,
    arrows(x0,y0,xt,yt,length=length,...),
    arrows(xt,yt,x1,y1,length=length,...))
    invisible(list(x=xt,y=yt))
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
#' @param ... other parameters passed to \code{\link{polygon}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{polygon}}
#' @keywords dynamic, aplot
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
#' @param ... other parameters passed to \code{\link{rect}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{rect}}
#' @keywords dynamic, aplot
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
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tpolygon(x0,y0,x1,y1,t)
#' }
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
#' @param x0 numeric vector specifying start symbol size,
#' see \code{\link[graphics]{par} cex}.
#' @param x1 numeric vector specifying end symbol size,
#' see \code{\link[graphics]{par} cex}.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @params area logical, if TRUE (default) sizes will be treated as
#' area. If FALSE, sizes will be considered by diameter.
#' @return A numeric vector giving the amount by which plotting text
#' and symbols should be magnified relative to the default \code{cex} value.
#' @export
#' @seealso \code{\link[graphics]{par}} for setting symbol sizes.
#' @keywords dynamic, aplot
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
#' @param ... other parameters passed to \code{\link{text}}.
#' @return List with numerical components \code{x} and \code{y} with
#' current position
#' @export
#' @seealso \code{\link[graphics]{text}}
#' @keywords dynamic, aplot
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
#' @params X0 numerical matrix, start matrix
#' @params X1 numerical matrix, end matrix
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return Intermediate matrix (numerical).
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' X0 <- matrix(c(1,2,3,4),2,2)
#' X1 <- matrix(c(11,12,13,14),2,2)
#' print(X0)
#' print(X1)
#' print(h(X0,X1,t=0.3))
#
# homotopy change of matrix
# set size po maintain proportional areas
# Warning: swapped arguments X0 and X1
tmatrix <- function(X0, X1=diag(nrow(X0)), t, when, p){
if(missing(t)) t <- get("t",envir=sys.frame(-1))
    invisible(h(X0,X1,t,when, p))
}

## ----fct tBoxCox---------------------------------------------------------
#' Interpolate Box-Cox Transformation Parameter \code{lambda}
#'
#' Interpolates Box-Cox transformation parameter \code{lambda}.
#'
#' @params x numerical vector.
#' @params lambda0 numeric, starting value of parameter lambda.
#' @params lambda1 numeric, end value of parameter lambda.
#' @inheritParams tParams
#' @inheritParams whenParams
#' @inheritParams pParams
#' @return numerical vector, Box-Cox transformed
#' values for current interpolated value of lambda.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive())
#' x <- runif(250,1,3)
#' animator("newplot(xlim=c(0,max(x)^2),ylim=c(0,1),axes=TRUE,asp=NA);
#' xt <- tBoxCox(x,-1,3,t); rug(xt);
#' lines(density(xt,add=TRUE))",life=5,verbose=TRUE)
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
 animator("newplot(xlim=c(0,max(x)^2),ylim=c(0,1),axes=TRUE,asp=NA);
 xt <- tBoxCox(x,-1,3,t); rug(xt);
 lines(density(xt,add=TRUE))",life=5,verbose=TRUE)
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

## ----fct animator0-------------------------------------------------------
animator0 <- function(block, dt =.dt){
if(is.expression(block))
for(t in seq(0,1,dt) ){ eval(block)}
if(is.character(block))
for(t in seq(0,1,dt) ){ eval(parse(text=block))}
attr(block,"class") <- "animator"
invisible(block)
}

## ----animator1-----------------------------------------------------------
animator1 <- function(block, dt =.dt , life=1,fps=25,verbose=FALSE){
t0 <- Sys.time()
t <- 0
i <- 0
life <- life+0.2
while((Sys.time()-t0)<=life) {
    if(is.expression(block))
        { eval(block)}
    if(is.character(block))
        { eval(parse(text=block))}
    i <- i+1
    if(!is.na(fps)) Sys.sleep(1/fps)
    t <- min(1,as.numeric((Sys.time()-t0))/life)
}
Life <- Sys.time()-t0
if(verbose) cat("Time  :",Life,"\nFrames:",i,"\nF/s   :",round(i/as.numeric(Life)),"\n")
#attr(block,"class") <- "animator"
#attr(block,"life") <- life
invisible(as.animator(block,life))
}


## ----fct animator--------------------------------------------------------
#' Plot Animated Block of Commands.
#'
#' Main function that plots animated sequence of figures.
#'
#' @params block character, expression (block) or 
#' object of class \code{animator} containing  graphical timed commands.
#' @params life numerical, duration of animation.
#' @params fps numerical, frames per second.
#' @params pause numerical, length of the pause between plotted frames.
#' @params verbose logical, if TRUE print animation characteristics.
#' @return object of class \code{animator}.
#' @seealso \code{\link{as.animator}}, \code{\link{is.animator}}.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' if(interactive())
#' animator("newplot();tpoints(2,2,5,8,cex=2,pch=16)",life=2,verbose=TRUE)
#
animator <- function(block, life=1,fps=25,pause=0.5,verbose=FALSE){
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
"\nF/s   :",round(i/as.numeric(Life)),
"\ndt    :",ts[3]-ts[2],"\n")
#attr(block,"class") <- "animator"
#attr(block,"life") <- life
invisible(as.animator(block,life))
}

## ----fct plot.animator---------------------------------------------------
#' Plot Method for Class \code{animator}..
#'
#' Performs and plots the animation of an object.
#'
#' @params x character, expression (block) or
#' object of class \code{animator} containing  graphical timed commands.
#' @params life numerical, duration of animation.
#' @params ... additional arguments passed to function \code{animator}.
#' @return object of class \code{animator}.
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

## ----fct as.animator-----------------------------------------------------
#' Objects of class \code{animator}.
#'
#' Creates or tests for objects of  class \code{animator}.
#'
#' @params x character, expression (block) or
#' object of class \code{animator} containing  graphical timed commands.
#' @params life numerical, duration of animation.
#' @return \code{as.animator} attempts to add the class 
#' \code{animator} to the argument \x}.
#'
#' \code{is.animator} returns \code{TRUE} if \code{x}  
#' is an \code{animator} object and \code{FALSE} otherwise.
#'
#' @seealso \code{\link{animator}}.
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
as.animator <- function(x,life=1){
 class(x) <- "animator"
 attr(x,"life") <- life
 return(x)
 }
## Tests for object class
is.animator <- function(x){
 return(class(x) == "animator")
 }
##

## ----getChunkopts--------------------------------------------------------
getChunkopts<-function(what){
    if ((n.parents <- length(sys.parents())) >= 3) {
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
    return(NULL)
}
if(.testing) getChunkopts("label")

## ----includeLatex--------------------------------------------------------
includeLatex <- function(title="",scale=0.5,poster="last",every=1,fps=25,from="",to="",
vspace="0pt",other=",controls"){
file <- paste(getChunkopts()[c("prefix.string","label")],collapse="-")
#cat("\n\\begin{frame}[fragile] ","\n")
#cat("\\frametitle{",title,"} ","\n")
#cat("\\begin{center} ","\n")
cat(" \\vspace{",vspace,"} ","\n",sep="")
cat("\\animategraphics[scale=",scale,
",poster=",poster,
",every=",every,
other,
"]{",fps,
"}{",file,
"}{",from,
"}{",to,
"} ","\n",sep="")
#cat("\\end{center} ","\n\n\n")
#cat("\\end{frame} ","\n\n")
}
if(.testing) includeLatex("poskusni izpis")
if(.testing) includeLatex("poskusni izpis",other=",autoplay")

