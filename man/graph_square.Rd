\name{graph_square}
\alias{graph_square}
\title{Construction of a graph on a square [0,1] * [0,1]}
\description{graph_square returns the coordinates of the vertices 2D between 0 and 1 as well as the edges with their weight}
\usage{
graph_square(n,nbPaths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
     number of vertices
}
  \item{nbPaths}{
     number of possible paths from the start to the end
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{n }{Description of 'number of vertices'}
%%  \item{nbPaths }{Description of 'number of possible paths from the start to the end'}
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
graph_square(n=100,nbPaths=20)
graph_square(n=1000,nbPaths=50) 
graph_square(20,5)
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
