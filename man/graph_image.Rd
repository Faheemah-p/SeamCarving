\name{graph_image}
\alias{graph_image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construction of the graph corresponding to an image}
\description{graph_image returns length, width and intensity of an image where intensity corresponds to the three main colors: red, green and blue. A pixel is a node, and from each node start three arcs, going respectively towards the pixel at the bottom left, at the bottom, and at the bottom right of the node considered. For the edge nodes, there are two arcs going towards the pixel at the bottom and at the bottom left or at the bottom right respectively of the node considered.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
graph_image(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{an image is considered as a graph
%%     ~~Describe \code{x} here~~
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
  graph_image(image)
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
