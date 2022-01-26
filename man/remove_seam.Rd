\name{remove_seam}
\alias{remove_seam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove the shortest image}
\description{Return the image after removing the shortest path. We apply Dijkstra or Bellman Ford algorithms to find the shortest path and after that we remove it.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
remove_seam(image,algorithm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{ a given image

}
  \item{algorithm}{ Dijkstra ou BellmanFord

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
remove_seam(image,Dijkstra)
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
