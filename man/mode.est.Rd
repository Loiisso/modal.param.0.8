\name{mode.est}
\alias{mode.est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
estimation of mode of specific density(or just vector)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mode.est(y, ini = T, acc = T, num.ini = 20, bw = "nrd")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{ini}{
%%     ~~Describe \code{ini} here~~
}
  \item{acc}{
%%     ~~Describe \code{acc} here~~
}
  \item{num.ini}{
%%     ~~Describe \code{num.ini} here~~
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
function (y, ini = T, acc = T, num.ini = 20, bw = "nrd") 
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
    if (acc == T) {
        acc <- 10^(-9)
    }
    if (ini == T) {
        num.ini <- num.ini
        temp <- sort(y)
        which.obs <- c(1:num.ini) * floor(n.obs/num.ini)
        ini <- temp[which.obs]
    }
    mode <- c()
    mode.obj <- c()
    pot.mode <- c()
    for (i in 1:num.ini) {
        pot.mode <- ini[i]
        dif <- 1
        obj <- -1
        while (dif > acc) {
            old.obj <- obj
            weights <- dnorm(x = y, mean = pot.mode, sd = bw)
            obj <- sum(weights)
            dif <- obj - old.obj
            pot.mode <- sum(weights * y)/sum(weights)
        }
        mode[i] <- pot.mode
        mode.obj[i] <- obj
    }
    mode.result <- cbind(mode.obj, mode)
    which.best <- which.max(mode.result[, "mode.obj"])
    result.mode <- mode.result[, "mode"][which.best]
    result.obj <- mode.result[, "mode.obj"][which.best]
    result <- data.frame(mode = result.mode, obj = result.obj)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
