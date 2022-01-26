\name{BellmanFord}
\alias{BellmanFord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bellman Ford algorithm}
\description{ Finding the shortest path after n-1 iterations where n is the number of vertices. It returns the distance and the vertices of the shortest path.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BellmanFord(graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ represents the coordinates of the vertices 2D between 0 and 1 as well as edges with their weight

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
graph=graph_square(12,4)
BellmanFord(graph)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
