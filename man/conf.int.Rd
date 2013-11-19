\name{conf.int}
\alias{conf.int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate confidence interval for specific probability
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
conf.int(resid, y, alpha, yhat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resid}{
%%     ~~Describe \code{resid} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{yhat}{
%%     ~~Describe \code{yhat} here~~
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
function (resid, y, alpha, yhat) 
{
    which.conf <- assym.int(x = resid, alpha = alpha)
    resid.minus <- sort(resid)[which.conf[1]]
    resid.plus <- sort(resid)[which.conf[2]]
    y.minus <- y + rep(resid.minus, length(y))
    y.plus <- y + rep(resid.plus, length(y))
    int.coverage <- length(which(yhat < y.plus & yhat > y.minus))/length(y)
    conf.mod <- resid.plus - resid.minus
    return(list(coverage = int.coverage, interval = conf.mod))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
