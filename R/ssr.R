ssr <- function(DIST=25, shp=NULL, colours=c("LightGreen","Tan"), PLOT=TRUE) {

  #==================================================
  #
  #  NAME:       ssr()
  #  AUTHOR:     Tarmo K. Remmel
  #  DATE:       22 August 2023
  #  NOTES:      Perform ShrinkShape fully in R with terra
  #  NEEDS:      A shapefile (polygons), properly built
  #              library: terra
  #
  #  NOTES:      Shapefile can have only a single shape
  #              Shapes with holes allowed
  #              Projection units must be m, not dd or other angular unit
  #              Shapefile must be imported already (this tool does not do that)
  #
  #              No dependance on rgeos and rgdal.
  #  
  #              Call ShrinkShape
  #              shpfileobj <- terra::vect(data@p4no)
  #              out <- ssr(DIST=25, shp=shpfileobj)
  #
  # SHAPEFILE MUST NOT BE 'DIRTY'. BE SURE TO SAVE PROPERLY CLEANED AND COMPLETE 
  # SHAPEFILES THAT HAVE CRS INFORMATION (PROJECTION DATA) ATTACHED
  # FULLY IMPLEMENTED IN R (NO SAGA REQUIRED) AND INCLUDES A PLOT OF THE SHRINKING
  # NOTE THAT PLOTTING GREATLY SLOWS THE ALGORITHM         
  #==================================================

  # VERIFY THAT THE shp ARGUMENT IS NOT NULL, IF IT IS, PROVIDE USEFUL FEEDBACK
  if(!is.null(shp)) {
  
    # ESTIMATE THE NUMBER OF ITERATIONS REQUIRED BASED ON SHAPEFILE BOUNDING BOX COORDINATES
    noiter <- ceiling(max(abs(terra::ext(shp)[1] - terra::ext(shp)[2]), abs(terra::ext(shp)[3] - terra::ext(shp)[4])) / DIST) + 1

    # SET PLOTTING OPTIONS
    if(PLOT) {
      plot.new()
      par(mfrow=c(2,3), pty="s")
      fill <- rep(colours, noiter)
      terra::plot(shp, col=fill[1], ext=terra::ext(shp))
      title("Original Polygon")
    
      terra::plot(shp, ext=terra::ext(shp), col=fill[1], add=FALSE, axes=TRUE)
      for (a in 1:noiter) {
        if(any(is.na(terra::geom(terra::buffer(shp, (-1 * a * DIST) )))) == FALSE) {
          terra::plot(terra::buffer(shp, (-1*a*DIST)), col=fill[a+1], ext=terra::ext(shp), add=TRUE, axes=FALSE)
        } # END IF
        par(new=TRUE)
      } # END FOR
      title("Shrinking Phases")
     } # END IF
    
     # NOW BUILD VECTORS FOR ACCUMULATING AREA, PERIMETER, AND PARTS
     area <- rep(0, noiter)
     perim <- rep(0, noiter)
     parts <- rep(0, noiter)

     # NOW ACCUMULATE AREA, PERIMETER AND PARTS FOR FULL SHAPE
     area[1] <- terra::expanse(shp)
     perim[1] <- terra::perim(shp)
     parts[1] <- max(terra::geom(shp)[,2])
  
     # PERFORM SHRINKING AND ACCUMULATE AREA, PERIMETER, AND PARTS FOR SHRUNKEN SHAPE
     for (a in 2:noiter) {
       # PROCESS BUFFERING ONLY IF IT DOES NOT CAUSE THE SHAPE TO DISAPPEAR
       if(any(is.na(terra::geom(terra::buffer(shp, (-1 * a * DIST) )))) == FALSE) {
         temp <- terra::buffer(shp, (-1*a*DIST) )
         area[a] <- terra::expanse(temp)
         perim[a] <- terra::perim(temp)
         parts[a] <- max(terra::geom(temp)[,2])
       } # END IF
     } # END FOR

     # BIND CUMULATIVE SHRINKING DISTANCE, AREA, PERIMETER, AND PARTS INTO MATRIX
     # NEED TO ADJUST THE NEXT LINE TO PROPERLY ADJUST THE CUMULATIVE SHRINKING DISTANCE
     tab <- as.data.frame(cbind(area, perim, parts))
     # TRIM tab TO REMOVE EXCESS ZEROS
     cutrow <- min(which(tab[,2] == 0))
     tab <- tab[1:cutrow,]
     # ADD THE CUMULATIVE SHRINKING DISTANCE ATTRIBUTE COLUMN
     tab <- cbind(1:dim(tab)[1], tab)
     # ADD THE SHRINKING PHASE ATTRIBUTE COLUMN
     tab <- cbind(seq(0,noiter*DIST,DIST)[1:dim(tab)[1]], tab)
     # MAKE THE COLUMN NAMES NICE
     names(tab) <- c("CumShrink", "Phase", "Area", "Perimeter", "NumParts")

     if(PLOT) {
       # PLOT AREA, PERIMETER, AND NUMBER OF PARTS RESULTS
       par(new=FALSE)
       plot(tab$Area/10000, type="o", xlab="Shrinking Phase", ylab="Area (ha)")
       title("Area")
       par(new=FALSE)
       plot(tab$Perimeter, type="o", xlab="Shrinking Phase", ylab="Perimeter Length (m)")
       title("Perimeter")
       par(new=FALSE)
       plot(tab$NumParts, type="o", xlab="Shrinking Phase", ylab="Number of Parts")
       title("Parts")
     } # END IF

     # SEND RESULTS TABLE BACK AS FUNCTION RETURN
     return(tab)
     
   } # END IF
   else {
     cat("The argument 'shp' cannot be NULL. Provide a valid dataset. See the Examples in help(ssr).\n")
   } # END: else
 } # END FUNCTION: ssr
