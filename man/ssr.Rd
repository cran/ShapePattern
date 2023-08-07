\name{ssr}
\alias{ssr}
\title{
Decompose a planar shape (polygon) to produce area, perimeter, and number of parts spectra
}

\description{
Given a single- or multi-part polygon (imported from a shapefile), ssr decomposes the shape by iteratively shrinking it by a specified distance until it becomes extinct. At each iteration of shrinking, the area, perimeter, and number of parts forming the resultant polygon are stored. Graphic plots and maps along with a tabular result are returned. This version works fully within R and no longer requires RSAGA GIS installed, making it more streamlined and faster.
}

\usage{
ssr(DIST = 25, shp = NULL, colours = c("LightGreen", "Tan"), PLOT = TRUE)
}

\arguments{
  \item{DIST}{
The specified distance (in meters) by which to incrementally shrink the shape internally.
}
  \item{shp}{
The shape (polygon) to be processed by ssr. This must be a single shape (although it can be multi-part and contain holes). This object needs to be imported by readOGR and saved as an object prior to calling ssr. 
}
  \item{colours}{
An option to provide a vector of two colours that will be used alternately when creating graphic outputs. This is implemented only if PLOT = TRUE.
}
  \item{PLOT}{
This is a Boolean (TRUE | FALSE) flag that governs whether graphic output is produced and presented at the conclusion of the decomposition. When ssr is called in batch mode (with batchssr), it is advised that this be set to FALSE as it dramatically slows processing and will continue to overplot itself.
}
}

\value{
If PLOT = TRUE, the function returns to the display maps of the decomposed polygon shape along with plots of the area, perimeter, and number of parts spectra. The function by default returns the tabular data from which the spectra can be plotted at any time.
}

\references{
Remmel, T.K. 2015. ShrinkShape2: a FOSS toolbox for computing rotation-invariant shape spectra for characterizing and comparing polygons. The Canadian Geographer 59(4):532-547.
}

\author{
Tarmo K. Remmel
}

\note{
The shapefile must already be imported and have only one unique shape (although it may be multi-part and contain holes). The projection must be rectangular with units in meters (not decimal degrees or other angular unit)
}

\seealso{
None.
}

\examples{
# ALTERNATE DEMO DATASETS
# shpfileobj <- terra::vect(data$p4no)
# shpfileobj <- terra::vect(data$p4is)

# PATH TO DEMO SHAPEFILE
f <- system.file("ex/lux.shp", package="terra")
# IMPORT SHAPEFILE TO SpatVect FORMAT WITH terra PACKAGE
v <- terra::vect(f)
# REPROJECT TO A VALID PROEJCTION
shpfileobj <- terra::project(v, "EPSG:2150")
# CALL ssr ON FIRST OF THE POLYGONS WITHIN THE SpatVect
out <- ssr(DIST=500, shp=shpfileobj[1], colours=c("LightGreen", "Tan"), PLOT=TRUE)
print(out)
}

\keyword{ manip }
