porosity <- function(IN=data$rst, PLOT=TRUE, NEIGH=4) {

  #-----------------------------------------------------------------------------------
  #
  #  FUNCTION:     porosity()
  #  DATE:         26 April, 2020
  #  AUTHOR:       Tarmo K. Remmel
  #  CALLS:         
  #  DESCRIPTION:  Computes the porosity of individual raster clusters.
  #
  #  ARGUMENTS:     IN:			Binary Raster	(A binary [0,1] raster to be assessed)
  #					PLOT:		TRUE | FALSE 	(Should plotting be provided on screen)
  #					NEIGH:		4 | 8 			(Number of neighbours to use for determining 
  #												 zone clusters. 4 = Rook's case, 8 = Queen's case )
  #
  #  DETAILS:		This function assesses for each contiguous zone (clump, or cluster, or patch)
  #					in an input raster, the porosity of that zone. That is, it compares the
  #					actual number of edge to edge joins and compares that to the theoretically 
  #					maximum number of joins possible given a set number of cells comprising that
  #					zone. The porosity quantifies the compactness of the shape based on a 
  #					theoretically maximally compact spatial structure.  
  #
  #  NEEDS:			This function requires the package 'raster'
  #
  #  NOTES:			For zones with only 1 cell, there will be no internal joins possible;
  #					thus, Jmax and Jact will both be 0 (zero) and hence the Porosity will
  #					appear as NaN in the output dataframe. This function works fully in R,
  #					unlike the version used for publishing the paper referred to below that 
  #					used a combination of python scripts written for ArcGIS that pass results to 
  #					to R. This version is much easier to implement and use generically.
  #
  #  VALUE:        The returned object is a dataframe with one row for each zone,
  #					and columns: Zone, N, Jact, Jmax, and Porosity.
  #
  #  REFERENCES:	Remmel, T.K. 2018. An incremental and philosophically different 
  #					approach to measuring raster patch porosity. Sustainability 10:3413.
  #					[DOI:10.3390/su10103413].
  #
  #  USAGE:        por <- porosity(IN=data$rst, PLOT=TRUE, NEIGH=4, TEST=FALSE)
  #
  #-----------------------------------------------------------------------------------

  # SAVE GRAPHIC AND WARNING PARAMETERS AND RESTATE THEM ON EXIT
  # THERE ARE KNOWN RASTER EXTENT WARNINGS THAT WILL BE THROWN; THESE ARE NOT ERRORS,
  # BUT EXPECTED DUE TO RASTER SIZE DIFFERENCES MANUFACTURED BY THE PROCESSING. THESE ARE
  # COMPLETELY NORMAL.
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn=oldw))
  
  if(PLOT) {
    # START A NEW PLOT ENVIRONMENT
    plot.new()
  } # END IF: PLOT
  
  if(PLOT) {
    par(mfrow=c(3,4), pty="s")
    plot(IN, main="Original")
  } # END IF: PLOT
  
  # ADD A ROW OF CELLS (0s) AROUND THE OUTSIDE TO AVOID EDGE EFFECTS
  imgpad <- extend(x=IN, y=1, value=0)
  if(PLOT) {
    plot(imgpad, main="imgpad")
  } # END IF: PLOT
  
  # STORE THE SPATIAL RESOLUTION OF THE RASER FOR SIMPLICITY LATER
  spres <- res(imgpad)[1]

  # PERFORM SHIFTS
  north <- shift(imgpad, dy=spres)
  south <- shift(imgpad, dy=-spres) 
  west <- shift(imgpad, dx=-spres)
  east <- shift(imgpad, dx=spres)

  if(PLOT) {
    plot(north, main="North")
    plot(south, main="South")
    plot(west, main="West")
    plot(east, main="East")
  } # END IF: PLOT

  # COMPUTE JOINS IN EACH OF THE 4 CARDINAL DIRECTIONS
  neighnorth <- imgpad * north 
  neighsouth <- imgpad * south
  neighwest <- imgpad * west
  neigheast <- imgpad * east

  if(PLOT) {
    plot(neighnorth)
    plot(neighsouth)
    plot(neighwest)
    plot(neigheast)
  } # END IF: PLOT

  # COMPUTE THE TOTAL NUMBER OF JOINS IN THE 4 DIRECTIONS
  out <- neighnorth + neighsouth + neighwest + neigheast
  
  if(PLOT) {
    plot(out, main="Out")
  } # END IF: PLOT

  # SUM CONNECTIONS
  tab <- freq(out)
  # REMOVE ZEROS
  tab <- tab[tab[,1] != 0,]
  tot2wayjoins <- sum(tab[,1]*tab[,2])
  tot1wayjoins <- tot2wayjoins/2

  # CREATE ZONES (CONSIDER HAVING A USER PROVIDE A ZONE MAP INSTEAD OF THIS AS AN OPTION)
  zones <- clump(IN, directions=NEIGH)
  # STORE THE NUMBER OF CELLS IN EACH ZONE
  N <- as.data.frame(freq(zones))
  N <- N[!is.na(N[,1]),]
  
  if(PLOT) {
    plot(zones, main="Zones")
  } # END IF: PLOT
  
  # COMPUTE BOTH 2-WAY and 1-WAY JOINS
  joins2way <- zonal(out, zones, fun=sum)
  joins1way <- joins2way[,2]/2

  # BUILD OUTPUT DATAFRAME AND ASSIGN MEANINGFUL ATTRIBUTE NAMES
  joins <- as.data.frame(cbind(N, joins2way[,2]/2, 0, 0))
  names(joins) <- c("Zone", "N", "Jact", "Jmax", "Porosity")
  
  # COMPUTE Jmax VALUES FOR EACH ZONE AND AND TO OUTPUT DATAFRAME
  joins$Jmax <- trunc((4 * joins$N - 4 * sqrt(joins$N)) / 2)

  # COMPUTE Porosity VALUES FOR EACH ZONE AND ADD TO OUTPUT DATAFRAME
  joins$Porosity <- 1 - (joins$Jact / joins$Jmax)

  # RETURN OBJECT
  return(joins)

} # END FUNCTION: porosity
