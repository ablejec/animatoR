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

## ----common--------------------------------------------------------------
#' Common arguments
#' 
#' The core arguments for animatoR plotting functions
#'
#' @param x0 numeric vector, start x coordinates.
#' @param y0 numeric vector, start y coordinates.
#' @param x1 numeric vector, end x coordinates.
#' @param y1 numeric vector, end y coordinates.
#' @param t numeric, homotopy parameter, limited between 0 and 1. 
#' This parameter can be considered as fraction of time 
#' duration of the animation.
#' @param when  numeric vector. This parameter controls
#' the times of: entrance, exit, start of movement and, end of movement.
#' @param p numeric, homotopy power parameter. Defaults to 1.
#' @name commonParams
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
#' @param ... see \code{\link{plot}}
#' @return NULL
#' @export
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
#' Interpolates a position between start and end coordinate.
#' Homotopy controled by a homotopy parameter \code{t} and power parameter \code{p} is used for interpolation.
#'
#' @param x0 numeric vector of start coordinates
#' @param x1 numeric vector of end coordinates
#' @param t numeric, homotopy parameter (between 0 and 1)
#' @param p numeric, power parameter (default=1)
#' @return interpolated value (see note)
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


## ----fct tpoints---------------------------------------------------------
#' Move points
#'
#' Move points from start to end location
#'
#' @inheritParams commonParams
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
function(x0, y0, x1=x0, y1=y0, t, when=c(0,1), trace=FALSE, trace.col="grey",...) {
# print(sys.nframe())
# print(ls(envir=sys.frame(-1)))
if(missing(t)) t <- get("t",envir=sys.frame(-1))
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
#
xt <- h(x0, x1, t, when)
yt <- h(y0, y1, t, when)
if(trace)
segments(x0,y0,xt,yt ,col=trace.col)
    points(xt, yt,...)
    invisible(list(x=xt,y=yt))
}

## ----tcols---------------------------------------------------------------
makeTransparent <- function(x, alpha=0){
if(is.character(x)) x <- col2rgb(x)/255
rgb(x[1],x[2],x[3],alpha)
}
tcols<-apply(col2rgb(c(1:8,0.1))/255,2,makeTransparent,alpha=0.75)
tcolnames <- paste("t",c("black","red","green","blue","cyan","magenta","yellow","grey","white"),sep="")
for(i in 1:length(tcols)) assign(tcolnames[i],tcols[i])

## ----fct trgb------------------------------------------------------------
#
trgb <- function(x0, x1=x0, t, when=c(0,1), alpha0=1, alpha1=1,...){
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    if(is.character(x0)) x0 <- col2rgb(x0)
    if(is.character(x1)) x1 <- col2rgb(x1)
    x1 <- h(x0/255,x1/255,t,when)
    alpha <- h(alpha0,alpha1,t,when)
#    cat(x1,t,when,"\n")
#    print(str(x1))
    invisible(rgb(x1[1],x1[2],x1[3],alpha,...))
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
#' @inheritParams commonParams
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
#' print(tlines(x0,y0,x1,y1,0.5))
#' #############
#' par(mfrow=c(2,2))
#' for( t in seq(0,1,1/3)) {
#' newplot()
#' tlines(x0,y0,x1,y1,t)
#' }
tlines <-
function(x0, y0, x1, y1, t, when, ...) {
#function(x0, y0, x1, y1, t0=0, t1=t0, t, when,dt=.dt,...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
#
if(missing(t)) t <- get("t",envir=sys.frame(-1))
#    for(t in seq(t0,t1,dt)) {
#    if(!add) newplot(axes=FALSE)
    xt <- h(x0, x1, t,when)
    yt <- h(y0, y1, t,when)
    lines(xt, yt ,...)
    invisible(list(x=xt,y=yt))
}

## ----fct tsegments-------------------------------------------------------
#' Draw segments
#'
#' Draw lines from start to end location
#'
#' @inheritParams commonParams
#' @param fixed numeric, which location is fixed: 
#' start (0, default) - draw from \code{(x0,y0)} to \code{(x1,y1)},
#' or end (1) - reverse.
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
tsegments <-
function(x0, y0, x1, y1, t, when, fixed=0, ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
#
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    if(fixed) t <- 1-t
    xt <- h(x0, x1, t,when)
    yt <- h(y0, y1, t,when)
#
    switch(fixed+1,
    segments(x0, y0, xt, yt,...),
    segments(xt, yt, x1, y1,...))
    invisible(list(x=xt,y=yt))
}

## ----fct tsegments2------------------------------------------------------
thline <-
function(x0, y0, x1, y1, t, when, fixed=1, ...) {
X <- cbind(x0,y0,x1,y1)
x0 <- X[,1]
y0 <- X[,2]
x1 <- X[,3]
y1 <- X[,4]
#
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    switch(fixed,
    segments(x0,h(y0, y1, t,when),x1, h(y0, y1, t,when),...),
    segments(h(x0, x1, t, when), h(y0, y1, t, when),x1,y1,...))
}

## ----fct tarrows---------------------------------------------------------
tarrows <-
function(x0, y0, x1, y1, t, when, fixed=1, length=0.125, ...) {
    X <- cbind(x0,y0,x1,y1)
    x0 <- X[,1]
    y0 <- X[,2]
    x1 <- X[,3]
    y1 <- X[,4]
    #
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    switch(fixed,
    arrows(x0,y0,h(x0, x1, t,when), h(y0, y1, t,when),length=length,...),
    arrows(h(x0, x1, t,when), h(y0, y1, t,when),x1,y1,length=length,...))

}

## ----fct tpolygon--------------------------------------------------------
tpolygon <-
function(x0, y0, x1, y1, t,when,...) {
    X <- cbind(x0,y0,x1,y1)
    x0 <- X[,1]
    y0 <- X[,2]
    x1 <- X[,3]
    y1 <- X[,4]
    #
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    polygon(h(x0, x1, t,when), h(y0, y1, t,when),...)

}

## ----fct trect-----------------------------------------------------------
trect <- function(xleft0, ybottom0, xright0, ytop0,
                 xleft1, ybottom1, xright1, ytop1, t, when, ...){
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
rect(h(xleft0,xleft1,t, when), h(ybottom0,ybottom1,t,when),
h(xright0,xright1,t,when),h(ytop0,ytop1,t,when),...)
}

## ----fct tcex------------------------------------------------------------
# homotopy change of cex
# set size po maintain proportional areas
tcex <- function(cex0=1, cex1=1, t, when, ...){
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    sqrt(h(cex0,cex1,t,when))
}

## ----fct ttext-----------------------------------------------------------
#
ttext <- function(x0, y0, x1=x0, y1=y0, t, when, text="",...){

    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    text(h(x0,x1,t,when),h(y0,y1,t,when),text,...)
}

## ----fct tmatrix---------------------------------------------------------
# homotopy change of matrix
# set size po maintain proportional areas
tmatrix <- function(X1, X0=diag(nrow(X1)), t, when, ...){
    if(missing(t)) t <- get("t",envir=sys.frame(-1))
    h(X0,X1,t,when)
}

## ----fct tBoxCox---------------------------------------------------------
BoxCox <- function(x,lambda=1,base=exp(1)){
if(lambda!=0) return((x^lambda-1) / lambda) else return(log(x,base))
}
# homotopy change of parameter lambda in boxcox transform
tBoxCox <- function(x,lambda0,lambda1=lambda0, t, when ,base=exp(1), ...){
   if(missing(t)) t <- get("t",envir=sys.frame(-1))
   BoxCox(x,h(lambda0,lambda1,t,when=when),base=base)
}
#
if(interactive()) animator("newplot();plot(tBoxCox(x,-5,0,t,base=2))",life=5,verbose=TRUE)


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
animator <- function(block, life=1,fps=25,pause=0.5,verbose=FALSE){
if(is.na(pause)) pause=0.1
t0 <- Sys.time()
t <- 0
i <- 0
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
    i <- i+1
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
"\nFrames:",i,
"\nF/s   :",round(i/as.numeric(Life)),
"\ndt    :",ts[3]-ts[2],"\n")
#attr(block,"class") <- "animator"
#attr(block,"life") <- life
invisible(as.animator(block,life))
}

## ----fct plot.animator---------------------------------------------------
###
## plot method for class \code{animator}.
##
plot.animator <- function(x,life,...) {
if(missing(life)) life <- attr(x,"life")
animator(x,life=life,...)
}

## ----fct as.animator-----------------------------------------------------
## preveri attribute
as.animator <- function(x,life=1){
 class(x) <- "animator"
 attr(x,"life") <- life
 return(x)
 }
##

## ----fct is.animator-----------------------------------------------------
## preveri attribute
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

