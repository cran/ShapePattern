\name{ShapePattern-package}
\alias{ShapePattern-package}
\alias{ShapePattern}
\docType{package}
\title{
\packageTitle{ShapePattern}
}

\description{
\packageDescription{ShapePattern}
}

\details{

The DESCRIPTION file:
\packageDESCRIPTION{ShapePattern}
\packageIndices{ShapePattern}
This package combines the legacy functionality of package \code{PatternClass} and package \code{ShapePattern} and sets the environment for continuing to contribute to shape and pattern analyses. Eventually, package \code{PatternClass} will be archived (please point to this package to maintain functionality). Functions are provided to perform (1) ShrinkShape, a spatial decomposition by iterative shrinking of planar shapes (ploygons); (2) PatternClass, a tool for computing and visualizing statistical differences between class-level landscape maps; (3) Patternbits, a means for producing probability distributions for the most elemental hyper-local pattern elements in a binary landscape map and using Kullback-Leibler divergence to assess similarity and differnece between two such distributions; (4) CAR 2D simulation of stationary and isotropic landscape patterns, and (5) a tool for computing raster zone (patch) porosity.
}

\author{
\packageAuthor{ShapePattern}

Maintainer: \packageMaintainer{ShapePattern}
}

\references{
1. Polygon shape analysis:

Remmel, T.K. 2015. ShrinkShape2: a FOSS toolbox for computing rotation-invariant shape spectra for characterizing and comparing polygons. The Canadian Geographer 59(4):532-547.


2. Comparing binary landscapes:

Remmel, T.K. and M.-J. Fortin. 2016. What constitutes a significant difference in landscape pattern? (using R). In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. http://sarahgergel.net/lel/learning-landscape-ecology/

Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and F. Csillag. 2003. When are two landscape pattern indices significantly different? Journal of Geographical Systems 5(4):331-351.


3. For computing pattern elements:

Remmel, T.K. 2020. Distributions of hyper-local configuration elements to characterize, compare, and assess landscape-level spatial patterns. Entropy 22(4):420.


4. Simulating Landscapes:

Remmel, T.K. and F. Csillag. When are two landscape pattern indices significantly different? Journal of Geographical Systems 5(4):331-351.


5. For computing raster patch porosity:

Remmel, T.K. 2018. An incremental and philosophically different approach to measuring raster patch porosity. Sustainability 10:3413.
}

\keyword{ package }
\keyword{ manip }

\seealso{
It would be prudent to also observe the \code{landscapemetrics} package.
}

\examples{
# Specific examples are provided on the individual function manual pages.
# This package provides functions for different areas of analysis:
#
# 1. ShrinkShape
# ssr()
#
# 2. PatternClass (legacy package moved here)
# The order of function calls should be: singlemap() > singleplotter()
# or
# singlemap(), singlemap() > doubleplotter()
#
# 3. Patternbits
# patternbits() and KLPQ()
#
# 4. Conditional AutoRegressive 2D pattern simulator
# CARsimu()
}
