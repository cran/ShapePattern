
# ShapePattern 3.1.0
* Improvements
    * Minor code updates in ssr() to accommodate changes to package: terra (thank you Robert Hijmans for the help).
* Bugfixes
    * Corresponding minor updates to manual pages and code.
    
# ShapePattern 3.0.1
* Improvements
    * Minor error trapping in ssr() that now provides meaningful feedback.
* Bugfixes
    * Corresponding minor updates to manual pages and code.

# ShapePattern 3.0.0
* Improvements
    * Adaptations for retired rgeos and rgdal packages.
    * Transfer reliance to package: terra.
* Bugfixes
    * Minor updates to manual pages and code.
    * Removed some old commented code to clean scripts.
* Retire
    * Retired batchssr() that are not necessary given the simplicity with which ssr() can be called in a loop.
    * Retired shpsplitter() as it was a niche tool used infrequently and it forces the data to be brought into ssr() clean, rather than rely on the tool to attempt to handle exceptions.

# ShapePattern 2.2.0
* Improvements
    * Removal of as.vector() calls to data.frames as per new CRAN requirements.
* Bugfixes
    * Minor updates to manual pages and code.
    * Removed some old commented code.
    * Moved some console feedback to warning() calls.
    * Fixed some index references that used integers to column names.

# ShapePattern 2.1.0
* Improvements
    * Inclusion of 'porosity()' function that provides the functionality of Remmel (2018), fully implemented in R and not relying on ArcGIS or Python.
    * Manual page included for 'porosity()'.
    * Added a small test raster, 'rst', in the 'data' supplied with this package.
    * New reference (Remmel 2018) added.
    * Changed earlier incorrect reference to Remmel (2018) to Remmel (2015).
    * Keeping pace with updated build of 'raster' package requiring R version 3.6.2
* Bugfixes
    * Minor updates to manual pages and code.

# ShapePattern 2.0.4
* Improvements
    * 'patternbits()' function to handle toroidal and non-toroidal grids is now working.
    * 'patternbits()' function now also handles grids whose number of rows and columns may differ.
    * Manual pages updated.
    * New reference (Remmel 2020) added.
    * Included a NEWS page to list updates.
* Bugfixes
    * Minor clean-up of code.

# ShapePattern 2.0.3
* Bugfixes
    * Fixed manual and description pages that provided outdated information.

# ShapePattern 2.0.2
* Improvements
    * Major bringing together of the origianl ShapePattern and PatternClass packages under this umbrella.
    * Included 'patternbits()' function, though some functionality is not fully operational.

# ShapePattern 1.0.1
* Improvements
    * Updates to manual pages.
* Bugfixes
    * Fix examples and clear warnings.

# ShapePattern 1.0
* First submission to CRAN.
