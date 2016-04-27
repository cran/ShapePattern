\name{data}
\alias{data}
\docType{data}
\title{
Two imported shapefiles are provided. One is the perimeter of a pond, the second is the same pond but with the internal islands (holes) included.
}

\description{
These two shapefile imports can be used to test the functionality of the ShrinkShape function and to become familiar with its use. The two versions of the shapefile illustrate the handling of islands (holes) in the data.
}

\usage{data("data")}

\format{
These are in imported shapefile format. They were imported using rgdal:readOGR.
}

\details{
These are simple single-shape shapefiles.
}

\source{
These pond perimeters were digitized in-house by Connie Ko at York University.
}

\references{
There are no references for these data elements.
}

\examples{
data(data)
plot(data$p4no)
plot(data$p4is)
}

\keyword{datasets}
