\name{patternbits}
\alias{patternbits}
\title{
Decompose a binary landscape (grid) to count its pattern elements
}

\description{
Given an input binary (0,1) pattern as a .csv file or matrix object, this function shifts that pattern in each of the 4 cardinal directions by one location (cell) and then counts the frequency for each of the 32 hyper-local neighbourhood configurations (\code{CODE}). The frequencies are provided for each \code{CODE} along with their \code{PROBABILITY}. Finally an object that maps the \code{CODE} into a matrix that can be drawn as a map is also provided as part of the ouput list object.
}

\usage{
patternbits(IMGCSV="~/Desktop/Book2.csv", 
            OBJ=FALSE, OBJname=matrix(c(1,1,0,0,0,1,1,1,0), nrow=3, ncol=3), 
            OBJtxt="NAMEhere",  
            DRAW=TRUE, 
            VERBOSE=FALSE, 
            TORUS=TRUE, 
            rho=0, 
            proportion=0.5)
}

\arguments{
  \item{IMGCSV}{
This is a .csv (comma separated values) textfile format of an input binary grid. It is read into a matrix object and needs to have an equal number of rows and columns. No header is permitted.}
  \item{OBJ}{
This is a Boolean flag that determines whether the input grid is available as on object (TRUE) or whether it will be read from a .csv file (FALSE).}
  \item{OBJname}{
If the input data is an object, the name of that object is provided here. If \code{OBJ} is FALSE, whatever is entered here is ignored.
}
  \item{OBJtxt}{
A character string that can be used to describe the input data or to encode a batch number, simulation identifier or other code that will be included in the output list object for later reference.
}
  \item{DRAW}{
This is a Boolean flag that is used to turn the drawing function on (TRUE) or off (FALSE). It can be useful to see the intermediate shifting layers used by the algorithm. This is often used to learn how the function works, rather than in production mode.
}
  \item{VERBOSE}{
This is a Boolean flag that is used to control the display of additional state feeback during operation (TRUE), or to suppress it (FALSE). Generally, this is used in the default FALSE mode, but since it was useful during development, it has been left in for now).
}
  \item{TORUS}{
This is a Boolean flag that determines whether the data is considered to be toroidal (continuous) by having the edges wrap around to the opposite side (TRUE), or not (FALSE). Currently both modes behave as if it is TRUE. The enhancement for FALSE is coming soon.
}
  \item{rho}{
A numeric element that serves the purpose of passing this pattern parameter to the function, allowng it to be added to the output list object as reference. It is not actually used to control processing. This value describes the degree of spatial autocorrelation in the input object.
}
  \item{proportion}{
A numeric element that serves the purpose of passing this pattern parameter to the function, allowng it to be added to the output list object as reference. It is not actually used to control processing. This value describes the proportion of black to white cells in the input object.
}
}

\value{
The output is a list object that contains \code{PROCESSINGDATE} (when the code was run), \code{IMAGE} (the name of the image, if supplied), \code{NCELLS} (the total number of cells in the grid), \code{TORUS} (Boolean indicator of whether processing as a torus or not), \code{RHO} (spatial autocorrelation value of grid), \code{PROPORTION} (proportion value of grid categories), \code{RESULTS} (output data.frame with all frequencies and probabilities), and \code{JOINT} (the output matrix of all pattern element codes). Note that C = centre, R = right, A = above, L = left, and B = below.
}

\references{
None currently.
}

\author{
Tarmo K. Remmel
}

\note{
TBA
}

\seealso{
See Also \code{\link{CARsimu}}.
}

\examples{
elements <- patternbits(OBJ=TRUE, OBJtxt="Demo")
str(elements)
}

\keyword{ manip }
