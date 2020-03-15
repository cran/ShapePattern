\name{data}
\alias{data}
\docType{data}
\title{
This is a generic data environment that provides some demo data and control functionality.
}

\description{
This is a generic data environment that provides some demo data and control functionality. for the operation of functions within this package. Two imported shapefiles are provided. One is the perimeter of a pond, the second is the same pond but with the internal islands (holes) included. There are demo images for comparing class patterns, a matrix of Whittle correction factors, and some additional interal objects that hold settings for the functions to operate properly.

}

\usage{data("data")}

\format{
The lake outlines (\code{p4is} and \code{p4no}) are in imported shapefile format; they were imported using \code{rgdal:readOGR}. The demo images for class-level pattern comparison (\code{demoimage1} and \code{demoimage2}) are matrix objects. DIFF50 is a matrix of Whittle correction factors (which should not be modified unless the user is certain that this needs to be done). The current factors are designed for 64x64 cell landscape subsets; corrections for other sizes can be constructed with \code{build.lut}; this can take a very long time!
}

\details{
Details of the individual objects are best obtained using \code{str()}.
}

\source{
These pond perimeters were digitized in-house by Connie Ko at York University; other materials were produced by Tarmo K. Remmel.
}

\references{
There are no references for these data elements.
}

\examples{
data(data)
plot(data$p4no)
plot(data$p4is)
print(data$DIFF50)
image(data$demoimage1)
image(data$demoimage2)
}

\keyword{datasets}
