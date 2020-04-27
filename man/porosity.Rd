\name{porosity}
\alias{porosity}
\title{
Compute the porosity of raster zones (patches)
}

\description{
This function scans an input binary raster to assess zones and then computes the porosity of each unique zone.
}

\usage{
porosity(IN=data$rst, 
            PLOT=TRUE,
            NEIGH=4)
}

\arguments{
  \item{IN}{
  	This is a binary raster input object. This raster will be clumped (i.e., like Esri's ArcGIS 'regiongroup' functionality) to produce zones of contiguous pixels. Each contiguous zone will be assessed for its porosity, or deviation from a theoretically maximally compact shape.
}
  \item{PLOT}{
  	This is a Boolean flag that controls whether plots will be provided as output during processing.
}
  \item{NEIGH}{
  	This is an integer, either 4 or 8 that controls whether a Rook's case or Queen's case neighbourhood contiguity is used to assess the zones.
}
}

\value{
The returned object is a dataframe with one row for each zone, and columns: Zone, N, Jact, Jmax, and Porosity. Here \code{Zone} is the unique ID for a specific raster zone of contiguous cells, \code{N} is the nuber of cells comprising that zone, \code{Jact} stores the number of actual internal edge-edge joins within the zone, \code{Jmax} is the theoretical maximum number of edge-edge joins based on \code{N}, and \code{Porosity} is the computed porosity value.
}

\references{
Remmel, T.K. 2018. An incremental and philosophically different approach to measuring raster patch porosity. Sustainability 10:3413.
}

\author{
Tarmo K. Remmel
}

\note{
For zones with only 1 cell, there will be no internal joins possible; thus, Jmax and Jact will both be 0 (zero) and hence the Porosity will appear as NaN in the output dataframe. This function works fully in R, unlike the version used for publishing the paper referred to below that used a combination of python scripts written for ArcGIS that pass results to to R. This version is much easier to implement and use generically.
}

\seealso{
See also the package '\code{raster}'.
}

\examples{
porosity(IN=data$rst, PLOT=TRUE, NEIGH=4)
}

\keyword{ manip }
