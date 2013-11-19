\name{modal.param.0.8-package}
\alias{modal.param.0.8-package}
\alias{modal.param.0.8}
\docType{package}
\title{
parametric modal regression based on Weixin Yao and Longhai Li (2011)
}
\description{
Estimation of conditional mode in parametric paradigma. Based on article and source MATLAB code by Weixin Yao and Longhai Li (2011)
}
\details{
\tabular{ll}{
Package: \tab modal.param.0.8\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-11-18\cr
License: \tab What license is it under?\cr
}
param.mod() is the primary interface. At minimum it requies "formula" and "data" arguments
}
\author{
Artem Puzanov

Maintainer: Artem Puzanov <loiisso@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\references{
Weixin Yao and Longhai Li (2011)
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ modal, regression, parametric, botev }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
example:
require(Ecdat)
data(PatentsHGH)

input.frame <- PatentsHGH
summary(input.frame)
formula <- pat ~ year + ardsic + scisect + logk + sumpat + logr
data <- input.frame
lm <- lm(formula = formula, data = data)
rlm <- rlm(formula = formula, data = data)
modal <- param.mod(formula = formula, data = data, bw = "SJ", scale = 1)
modal$weights <- 1 #Always do this to get correct errors.

summary(modal)
summary(lm)
summary(rlm)
coefficients(modal)
coefficients(lm)
coefficients(rlm)
}
