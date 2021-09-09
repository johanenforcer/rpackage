\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function computes the minimum distances between a specific node to all other nodes in the graph.
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
A valid graph formatted as a dataframe with three specific columns, v1, v2, w, corresponding to the from node, to node and the associated weight between said nodes. 
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
The specific node to start traversing from which will be the basis for computing minimum distances to all other nodes in graph. 
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
https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm  
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

\return{
%%  ~~What the function returns~~
The function returns a vector with all minimum distances between the init_node and
all other nodes in the graph including the init_node itself.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    stopifnot(is.data.frame(graph), identical(names(graph), c("v1", 
        "v2", "w")))
    stopifnot(is.numeric(init_node), init_node \%in\% graph[, 1])
    total_nodes <- length(unique(graph[["v1"]]))
    unvisited_list <- as.vector(unique(graph[["v1"]]))
    visited_list <- list()
    scores = rep(Inf, total_nodes)
    names(scores) <- c(1:length(scores))
    scores[init_node] = 0
    current_node = init_node
    while (length(unvisited_list) > 0) {
        unvisited_list <- unvisited_list[unvisited_list != current_node]
        visited_list <- c(visited_list, current_node)
        df1 <- graph[graph$v1 == current_node, ]
        s1 <- df1[["v2"]]
        weights <- df1[["w"]]
        for (i in 1:length(s1)) {
            if (s1[i] \%in\% unvisited_list) {
                sum <- (scores[current_node] + weights[i])
                if (sum < scores[s1[i]]) {
                  scores[s1[i]] <- sum
                }
            }
        }
        if (length(visited_list) == total_nodes) {
            break
        }
        copy <- scores
        for (node in visited_list) {
            copy <- copy[-which(names(copy) == node)]
        }
        minval <- copy[which.min(copy)]
        current_node <- names(which(copy == minval))
    }
    return(as.vector(scores))
  }
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
