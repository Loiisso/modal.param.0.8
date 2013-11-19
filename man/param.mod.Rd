\name{param.mod}
\alias{param.mod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main iterface function for parametric modal regression
}
\description{
Estimates glm() object for modal regression estimation through iterative reweighting of observations
}
\usage{
param.mod(formula, data, max.iter = 500, conv.limits = 10^(-9),  bw = "nrd", bw.mode.ders.estim = NULL, bw.mode.est = NULL, bw.precise.h = F, scale = 2,  start.rlm.control = list(), mode.ders.estim.control = list(), mode.est.control = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{- a formula object}
  \item{data}{- a data.frame object, containing neccesary variables}
  \item{max.iter}{-maximum number of iterations in iterative least squares}
  \item{conv.limits}{difference in EM maximization results after which 
  the iterative process have converged}
  \item{bw}{-choice of bandwidth function (see further in bw() R function) 
  for density estimation procedures in  mode.ders.estim(), modal.estim() functions.
  Possible values are “botev”,”nrd”,”default”, “SJ”,”nrd0”. Has a higher priority over “bw.mode.ders.estim” and “bw.mode.est” values.}
  \item{bw.mode.ders.estim}{- specific bandwidth choice for mode.ders.estim() functions}
  \item{bw.mode.est}{specific bandwidth choice for mode.est() function}
  \item{bw.precise.h}{should bandwidth choice be recalculated inside each iteration? 
  (Not implemented, slows down calculation, increases bias for now).}
  \item{scale}{- a dividor for “start.h” in modal.estim() function. 
  Recommened value is 2, - gives minimum bias on monte-carlo simulation. 
  A hack, actually. Once the source of bias shall be removed, this argument shall be deleted.}
  \item{start.rlm.control}{- a list object of arguments for rlm() function that gives initial density of residuals}
  \item{mode.ders.estim.control}{- a list object of arguments for mode.ders.estim() function}
  \item{mode.est.control}{- a list object of arguments for mode.est() function}
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

data(PatentsHGH)
input.frame <- PatentsHGH
summary(input.frame)
formula <- pat ~ year + ardsic + scisect + logk + sumpat + logr
data <- input.frame
lm <- lm(formula = formula, data = data)
rlm <- rlm(formula = formula, data = data)
modal <- param.mod(formula = formula, data = data, bw = "SJ", scale = 1)
# Always do this before summary function for param.mod!
modal$weights <- 1  

summary(modal)
coefficients(modal)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
