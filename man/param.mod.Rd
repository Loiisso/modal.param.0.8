\name{param.mod}
\alias{param.mod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main iterface function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
param.mod(formula, data, max.iter = 500, conv.limits = 10^(-9), bw = "nrd", bw.mode.ders.estim = NULL, bw.mode.est = NULL, bw.precise.h = F, scale = 2, start.rlm.control = list(), mode.ders.estim.control = list(), mode.est.control = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{max.iter}{
%%     ~~Describe \code{max.iter} here~~
}
  \item{conv.limits}{
%%     ~~Describe \code{conv.limits} here~~
}
  \item{bw}{
%%     ~~Describe \code{bw} here~~
}
  \item{bw.mode.ders.estim}{
%%     ~~Describe \code{bw.mode.ders.estim} here~~
}
  \item{bw.mode.est}{
%%     ~~Describe \code{bw.mode.est} here~~
}
  \item{bw.precise.h}{
%%     ~~Describe \code{bw.precise.h} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{start.rlm.control}{
%%     ~~Describe \code{start.rlm.control} here~~
}
  \item{mode.ders.estim.control}{
%%     ~~Describe \code{mode.ders.estim.control} here~~
}
  \item{mode.est.control}{
%%     ~~Describe \code{mode.est.control} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (formula, data, max.iter = 500, conv.limits = 10^(-9), 
    bw = "nrd", bw.mode.ders.estim = NULL, bw.mode.est = NULL, 
    bw.precise.h = F, scale = 2, start.rlm.control = list(), 
    mode.ders.estim.control = list(), mode.est.control = list(), 
    ...) 
{
    if ((!is.null(bw)) & is.null(bw.mode.ders.estim)) {
        mode.est.control$bw <- bw
    }
    else if (!is.null(bw.mode.ders.estim)) {
        mode.est.control$bw <- bw.mode.est
    }
    if ((!is.null(bw)) & is.null(bw.mode.ders.estim)) {
        mode.ders.estim.control$bw <- bw
    }
    else if (!is.null(bw.mode.ders.estim)) {
        mode.ders.estim.control$bw <- bw.mode.ders.estim
    }
    data <- get_all_vars(formula = formula, data = data)
    rlm.reg <- do.call("rlm", c(list(formula = formula, data = data, 
        x.ret = T, y.ret = T), start.rlm.control))
    i.residuals <- rlm.reg$residuals
    mode.est.control$y <- i.residuals
    normal.mode <- do.call(mode.est, mode.est.control)
    mode.ders.estim.control$y <- i.residuals
    mode.ders.estim.control$mode <- normal.mode
    mode.ders <- do.call(mode.ders.estim, mode.ders.estim.control)
    h <- resid.bw(model.matrix = rlm.reg$x, mode.der0 = mode.ders$mode.der0, 
        mode.der3 = mode.ders$mode.der3)
    h <- h/scale
    warning("Experiment here!: bandwidth is divided by scale - optimal for SJ and botev estimations , \n          making estimates more precise BUT this is only theoretical, works only on belief,\n          DO NOT OVERDO - 2 times is quite enough")
    density.at.resids <- dnorm(x = i.residuals, 0, sd = h)
    best.fit <- modal.estim(formula = formula, data = data, start.density = density.at.resids, 
        start.h = h, max.iter = max.iter, conv.limits = conv.limits, 
        mode.est.control = mode.est.control, mode.ders.estim.control = mode.ders.estim.control, 
        ...)
    return(best.fit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
