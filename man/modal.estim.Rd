\name{modal.estim}
\alias{modal.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A workhorse function - hosts cycle of iterations of glm calculations and mode/mode derivatives recalculations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
modal.estim(formula, data, start.density, start.h, conv.limits, precise.h = F, max.iter = 500, mode.est.control = list(), mode.ders.estim.control = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{start.density}{
%%     ~~Describe \code{start.density} here~~
}
  \item{start.h}{
%%     ~~Describe \code{start.h} here~~
}
  \item{conv.limits}{
%%     ~~Describe \code{conv.limits} here~~
}
  \item{precise.h}{
%%     ~~Describe \code{precise.h} here~~
}
  \item{max.iter}{
%%     ~~Describe \code{max.iter} here~~
}
  \item{mode.est.control}{
%%     ~~Describe \code{mode.est.control} here~~
}
  \item{mode.ders.estim.control}{
%%     ~~Describe \code{mode.ders.estim.control} here~~
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
function (formula, data, start.density, start.h, conv.limits, 
    precise.h = F, max.iter = 500, mode.est.control = list(), 
    mode.ders.estim.control = list(), ...) 
{
    Q.modal.iter <- c()
    fits <- list()
    bw.vec <- c()
    density.at.resids <- start.density
    h <- start.h
    for (i in 1:max.iter) {
        print(i)
        weighted.fit <- do.call("glm", args = list(formula = formula, 
            data = na.omit(data.frame(data, density.at.resids)), 
            weights = density.at.resids, ...))
        i.residuals <- weighted.fit$residuals
        Q.modal.iter[i] <- sum(dnorm(x = i.residuals, 0, sd = start.h))
        if (i > 1) {
            if (Q.modal.iter[i] - Q.modal.iter[i - 1] < conv.limits) {
                print("Converged")
                break
            }
        }
        density.at.resids <- dnorm(x = i.residuals, 0, sd = h)
    }
    best.fit <- weighted.fit
    best.fit$bw <- h
    return(best.fit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
