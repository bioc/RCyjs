% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCyjs-class.R
\name{fitSelection,RCyjs-method}
\alias{fitSelection,RCyjs-method}
\alias{fitSelection}
\title{fitSelection}
\usage{
\S4method{fitSelection}{RCyjs}(obj, padding = 30)
}
\arguments{
\item{obj}{an RCyjs instance}

\item{padding}{numeric, in pixels}
}
\value{
no return value
}
\description{
\code{fitSelection} zoom in to include only currently selected nodes
}
\examples{
if(interactive()){
   rcy <- RCyjs(title="rcyjs demo", graph=simpleDemoGraph())
   selectNodes(rcy, "A")
   fitSelection(rcy, padding=100)
   }
}
