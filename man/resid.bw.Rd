\name{resid.bw}
\alias{resid.bw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of bandwidth for modal.estim() iterative procedure.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resid.bw(model.matrix, const = T, mode.der0, mode.der3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.matrix}{
%%     ~~Describe \code{model.matrix} here~~
}
  \item{const}{
%%     ~~Describe \code{const} here~~
}
  \item{mode.der0}{
%%     ~~Describe \code{mode.der0} here~~
}
  \item{mode.der3}{
%%     ~~Describe \code{mode.der3} here~~
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
function (model.matrix, const = T, mode.der0, mode.der3) 
{
    n.obs <- nrow(model.matrix)
    X = model.matrix
    L <- t(X) \%*\% X/n.obs * mode.der0
    K <- apply(X = X, MARGIN = 2, FUN = mean) * mode.der3
    compl.calc <- (6/4/pi^(1/2)/n.obs/(t(K) \%*\% ginv(L) \%*\% K))
    h <- compl.calc^(1/7)
    return(h)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
