\name{kde.botev}
\alias{kde.botev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculates botev bandwidth and kernel denisty
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kde.botev(data, n, MIN, MAX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{MIN}{
%%     ~~Describe \code{MIN} here~~
}
  \item{MAX}{
%%     ~~Describe \code{MAX} here~~
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
function (data, n, MIN, MAX) 
{
    nargin = length(as.list(match.call())) - 1
    if (nargin < 2) 
        n = 2^14
    n = 2^ceiling(log2(n))
    if (nargin < 4) {
        minimum = min(data)
        maximum = max(data)
        Range = maximum - minimum
        MIN = minimum - Range/10
        MAX = maximum + Range/10
    }
    R = MAX - MIN
    dx = R/n
    xmesh = MIN + seq(0, R, dx)
    N = length(data)
    w = hist(data, xmesh, plot = FALSE)
    initial_data = (w$counts)/N
    initial_data = initial_data/sum(initial_data)
    dct1d <- function(data) {
        n = length(data)
        weight = c(1, 2 * exp(-(0+1i) * (1:(n - 1)) * pi/(2 * 
            n)))
        data = c(data[seq(1, n - 1, 2)], data[seq(n, 2, -2)])
        data = Re(weight * fft(data))
        data
    }
    a = dct1d(initial_data)
    I = (1:(n - 1))^2
    a2 = (a[2:n]/2)^2
    fixed_point <- function(t, N, I, a2) {
        l = 7
        f = 2 * (pi^(2 * l)) * sum((I^l) * a2 * exp(-I * (pi^2) * 
            t))
        for (s in (l - 1):2) {
            K0 = prod(seq(1, 2 * s - 1, 2))/sqrt(2 * pi)
            const = (1 + (1/2)^(s + 1/2))/3
            time = (2 * const * K0/N/f)^(2/(3 + 2 * s))
            f = 2 * pi^(2 * s) * sum(I^s * a2 * exp(-I * pi^2 * 
                time))
        }
        out = t - (2 * N * sqrt(pi) * f)^(-2/5)
    }
    t_star = tryCatch(uniroot(fixed_point, c(0, 0.1), N = N, 
        I = I, a2 = a2, tol = 10^(-14))$root, error = function(e) 0.28 * 
        N^(-2/5))
    a_t = a * exp(-(0:(n - 1))^2 * pi^2 * t_star/2)
    idct1d <- function(data) {
        n = length(data)
        weights = n * exp((0+1i) * (0:(n - 1)) * pi/(2 * n))
        data = Re(fft(weights * data, inverse = TRUE))/n
        out = rep(0, n)
        out[seq(1, n, 2)] = data[1:(n/2)]
        out[seq(2, n, 2)] = data[n:(n/2 + 1)]
        out
    }
    density = idct1d(a_t)/R
    bandwidth = sqrt(t_star) * R
    xmesh = seq(MIN, MAX, R/(n - 1))
    result.dens <- data.frame(xmesh, density)
    result <- list(result.dens, bw = bandwidth)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
