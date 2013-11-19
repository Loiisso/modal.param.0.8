\name{mode.ders.estim}
\alias{mode.ders.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
estimate derivatives for kernel density in a specific point (in mode, here)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mode.ders.estim(y, mode, ks.lib = F, kern.Smooth.lib = F, default.method = T, bw = "nrd")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{ks.lib}{
%%     ~~Describe \code{ks.lib} here~~
}
  \item{kern.Smooth.lib}{
%%     ~~Describe \code{kern.Smooth.lib} here~~
}
  \item{default.method}{
%%     ~~Describe \code{default.method} here~~
}
  \item{bw}{
%%     ~~Describe \code{bw} here~~
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
function (y, mode, ks.lib = F, kern.Smooth.lib = F, default.method = T, 
    bw = "nrd") 
{
    n.obs <- length(y)
    if (bw == "default") {
        bw <- 1.06 * sd(y) * n.obs^(-1/5)
    }
    if (bw == "nrd") {
        bw <- bw.nrd(y)
    }
    if (bw == "botev") {
        bw <- kde.botev(y)$bw
    }
    if (bw == "SJ") {
        bw <- bw.SJ(y)
    }
    if (bw == "nrd0") {
        bw <- bw.nrd0(y)
    }
    bw.der0 <- bw
    bw.der3 <- bw * (n.obs^(1/5 - 1/11))
    warning("Experiment- derivative order is estimated wih non-canon function")
    if (ks.lib == T) {
        mode.der0 <- kdde(x = y, h = bw.der0, deriv.order = 0, 
            eval.points = mode)$estimate
        mode.der3 <- kdde(x = y, h = bw.der3, deriv.order = 3, 
            eval.points = mode)$estimate
    }
    if (kern.Smooth.lib == T) {
        dens.der0 <- bkfe(x = y, bandwidth = bw.der0, drv = 0)
        dens.der3 <- bkfe(x = y, bandwidth = bw.der3, drv = 3)
    }
    if (default.method == T) {
        mode.der0 <- c()
        for (i in 1:length(mode)) {
            temp <- (mode[i] - y)/bw.der0
            mode.der0[i] <- (1/n.obs/bw.der0/sqrt(x = 2 * pi) * 
                sum(exp(-temp^2/2)))
        }
        mode.der3 <- c()
        for (i in 1:length(mode)) {
            temp <- (mode[i] - y)/bw.der3
            mode.der3[i] <- (1/n.obs/bw.der3^4/sqrt(x = 2 * pi) * 
                sum((3 * temp - temp^3) * exp(-temp^2/2)))
        }
    }
    Return.list <- list(bw.start = bw, mode.der0 = mode.der0, 
        mode.der3 = mode.der3)
    return(Return.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
