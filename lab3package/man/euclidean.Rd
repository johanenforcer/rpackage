\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Euclidean algorithm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function calculcates the gcd between two specified numbers.
}
\usage{
euclidean(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
The first number to assign the euclidean algorithm.
}
  \item{b}{
%%     ~~Describe \code{b} here~~
The second number to assign the euclidean algorithm.
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
https://en.wikipedia.org/wiki/Euclidean_algorithm
}
\author{
%%  ~~who you are~~
A random dude
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
function (a, b) 
{
    while (b != 0) {
        r <- a\%\%b
        a <- b
        b <- r
    }
    return(a)
  }
}

\return{
%%  ~~What the function returns~~
The function returns the gcd between two numbers ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
