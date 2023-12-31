% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCyjs-class.R
\name{setZoom,RCyjs-method}
\alias{setZoom,RCyjs-method}
\alias{setZoom}
\title{setZoom}
\usage{
\S4method{setZoom}{RCyjs}(obj, newValue)
}
\arguments{
\item{obj}{an RCyjs instance}

\item{newValue}{numeric, typically be 0.1 (zoomed way out, nodes are small) and 10 (zoomed way in, nodes are large)}
}
\value{
no return value
}
\description{
\code{setZoom} zoom in or out
}
\examples{
if(interactive()){
   rcy <- RCyjs(title="rcyjs demo", graph=simpleDemoGraph())
   setZoom(rcy, 0.2)
   Sys.sleep(1)
   setZoom(rcy, 5)
   }

}
