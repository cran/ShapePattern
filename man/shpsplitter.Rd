\name{shpsplitter}
\alias{shpsplitter}
\alias{sp}
\alias{rdgal}

\title{
Shapefile splitting function to produce a unique shapefile for each unique shape identified by GRIDCODE
}

\description{
While shapefiles can contain multiple polygons, ShrinkShape (ssr) can only process a single shape at a time and thus shapefiles with multiple polygon features need to be split into independent shapefiles prior to processing them with ShrinkShape. This function splits a shapefile into multiple shapefiles by unique values in the GRIDCODE attribute.
}

\usage{
shpsplitter(SHAPEFILE = "f09uqresids64", CODE = "f09_R64_")
}

\arguments{
  \item{SHAPEFILE}{
The name of the shapefile without ".shp" that will be subjected to splitting.
}
  \item{CODE}{
This parameter allows all of the output shapefiles to have this same filename start (to help group like files).
}
}

\details{
The output filenames (shapefiles) will all begin with the string provided by CODE and then the unique GRIDCODE value will be appended to this to keep the lineage of the data clean. Select a CODE that makes sense.
}

\value{
Outputs from this function are shapefiles, one for each unique GRIDCODE attribute in the input SHAPEFILE. The shapefiles are written to the workspace on the computer in preparation for processing with ssr (individually) or sequentially with batchssr.
}

\references{
Remmel, T.K. 2016. Classifying boreal wildfires by clustering ShrinkShape spectra of internal unburned vegetation patches. US-IALE 2016 Annual Meeting: Landscape Change, 3-7 April, Asheville, North Carolina, USA.
}

\author{
Tarmo K. Remmel
}

\note{
In furture releases, the attribute for splitting will be made a parameter, but for now it must be GRIDCODE. This tool requires libraries sp and rgdal.
}

\seealso{
See Also as \code{\link{ssr}}, \code{\link{batchssr}}
}

\examples{
\dontrun{
# The functionality is provided by the library rgdal, and specifically readOGR.
# Depending on the number of unique GRIDCODE values in the shapefile, this function
# has the potential to produce a large number of output files.

shpsplitter(SHAPEFILE="f09uqresids64", CODE="f09_R64_")
}
}

\keyword{ manip }
