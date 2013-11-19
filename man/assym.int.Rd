\name{assym.int}
\alias{assym.int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate assymetric confidence intervals
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
assym.int(x, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (x, alpha) 
{
    x <- sort(x)
    dens.x <- kde(x, eval.points = x)$estimate
    which.max <- which(dens.x == max(dens.x))
    n <- length(x)
    diff <- round(n * (alpha))
    k1 <- round(which.max/2)
    warning("Not sure where to start...")
    k2 <- k1 + diff
    for (i in 1:length(x)) {
        check.1 <- 0
        check.2 <- 0
        dens.k1 <- kde(x = x, eval.points = x[k1])$estimate
        dens.k2 <- kde(x = x, eval.points = x[k2])$estimate
        dens.k1plus <- kde(x = x, eval.points = x[k1 + 1])$estimate
        dens.k2plus <- kde(x = x, eval.points = x[k2 + 1])$estimate
        dens.k1minus <- kde(x = x, eval.points = x[k1 - 1])$estimate
        dens.k2minus <- kde(x = x, eval.points = x[k2 - 1])$estimate
        if (dens.k1 < dens.k2 & dens.k1plus < dens.k2plus) {
            k1 <- k1 + 1
            k2 <- k2 + 1
            check.1 <- 1
        }
        if (dens.k1 > dens.k2 & dens.k1minus > dens.k2minus) {
            k1 <- k1 - 1
            k2 <- k2 - 1
            check.2 <- 1
        }
        if (check.1 == 0 & check.2 == 0) {
            break
        }
    }
    return(c(k1, k2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
