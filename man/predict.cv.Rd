\name{predict.cv}
\alias{predict.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
predict confidence intervals for tests
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.cv(formula, data, n.tests, optim.call, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{n.tests}{
%%     ~~Describe \code{n.tests} here~~
}
  \item{optim.call}{
%%     ~~Describe \code{optim.call} here~~
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
function (formula, data, n.tests, optim.call, ...) 
{
    sample <- sample(1:nrow(data), size = nrow(data))
    length.test <- round(nrow(data)/n.tests)
    test.vec <- c()
    for (j in 1:n.tests) {
        test.vec <- c(test.vec, rep(j, length.test))
    }
    add.smth <- c()
    if (nrow(data) - length.test * n.tests > 0) {
        add.smth <- rep(1:n.tests, nrow(data) - length.test * 
            n.tests)
    }
    test.frame <- data.frame(which.obs = sample, test.group = c(test.vec, 
        add.smth)[1:length(sample)])
    y.cross <- double(nrow(data))
    for (i in 1:n.tests) {
        which.test <- test.frame$which.obs[test.frame$test.group == 
            i]
        print(paste("cross", i))
        result <- do.call(optim.call, list(formula = formula, 
            data = data[-which.test, ], ...))
        y.cross[which.test] <- predict(object = result, newdata = data[which.test, 
            ])
    }
    return(y.cross)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
