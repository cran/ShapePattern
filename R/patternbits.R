patternbits <- function(IMGCSV="~/Desktop/Book2.csv", OBJ=TRUE, OBJname=matrix(c(1,1,0,0,0,1,1,1,0), nrow=3, ncol=3), OBJtxt="NAMEhere", DRAW=TRUE, VERBOSE=FALSE, TORUS=TRUE, rho=0, proportion=0.5) {

  #-----------------------------------------------------
  # 
  # FILENAME:	patternbits.r
  # AUTHOR:     Tarmo K. Remmel
  # DATE:       9 December 2019
  # FUNCTION:	patternbits()
  # NEEDS:      Input binary raster, read from .csv
  #				OR an integer (0,1) matrix.
  #             install.packages("raster")
  # CALLED BY:	NA
  # CALLS:      NA
  # RETURNS:    results
  # NOTES:      Based on the idea of felo.tanul in 2002
  #             See file: Neighbours.xlsx for testing
  #             C (CENTRE), R (Right), A (Above),
  #             L (LEFT), B (BELOW); SUCH THAT 0 IS BLACK (DRAWS IN GREY)
  #             AND 1 IS WHITE (DRAWS IN GREEN). CODE IS THE 5-DIGIT
  #             BINARY CODE THAT CHARACTERIZES THE
  #             NEIGHBOURHOOD STRUCTURE FOR 4 ORTHOGONAL
  #             NEIGHBOURS AND THE CENTRE. FREQUNCY IS A
  #             COUNT OF HOW MANY TIMES EACH PATTERN
  #             OCCURS IN THE SUPPLIED IMAGE.
  # 
  #-----------------------------------------------------

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  # READ .CSV FILE TO OBJECT
  if(!OBJ) {
    img <- as.matrix(read.table(file=IMGCSV, sep=",", header=FALSE))
  } else {img <- OBJname}
  
  # PREPARE PLOTTING ENVIRONMENT
  par(mfrow=c(3,3), pty="s")

  # STORE IMAGE EXTENTS
  extimg <- dim(img)
  cat(extimg)


  # PRINT IMAGE MATRIX TO SCREEN (FOR TESTING PURPOSES; CAN REMOVE THIS LATER)
  if(VERBOSE) {
  	print(img)
  }
  
  # PERFORM IMAGE SHIFTING
  shiftN <- rbind(img[2:extimg[1],],img[1,])
  shiftE <- cbind(img[,extimg[2]],img[,1:extimg[1]-1])
  shiftS <- rbind(img[extimg[1],],img[1:extimg[1]-1,])
  shiftW <- cbind(img[,2:extimg[2]],img[,1])

  # PRODUCE RASTER VERSIONS FOR PROPER PLOTTING
  imgrst <- raster(img)
  shiftNrst <- raster(shiftN)
  shiftErst <- raster(shiftE)
  shiftSrst <- raster(shiftS)
  shiftWrst <- raster(shiftW)

  # PLOT RASTERS IF DESIRED
  if(DRAW) {
    plot(imgrst, main="Original")
    plot(shiftNrst, main="Shifted North")
    plot(shiftErst, main="Shifted East")
    plot(shiftSrst, main="Shifted South")
    plot(shiftWrst, main="Shifted West")

  }
  
  # BUILD OUTPUT MATRIX TO HOLD RESULTS
  n <- 5
  results <- expand.grid(replicate(n, 0:1, simplify = FALSE))
  results <- results[,5:1]
  results <- cbind(results,0,0,0)
  names(results) <- c("C","R","A","L","B","CODE","FREQUENCY", "PROBABILITY")
  results$CODE <- as.character(results$CODE)
  results$CODE <- paste(results$C,results$R, results$A, results$L, results$B, sep="")
  results$CODE <- as.integer(results$CODE)

  # NON-INVERTED PORTION
  # CODE SHIFTED DATA BASED ON DIRECTIONAL SHIFT
  shiftNc <- shiftN * 1
  shiftEc <- shiftE * 10
  shiftSc <- shiftS * 100
  shiftWc <- shiftW * 1000
  
  below <- shiftNc * img
  onleft <- shiftEc * img
  above <- shiftSc * img
  onright <- shiftWc * img
  
  # COMBINE THE INFORMATION FROM ALL DIRECTIONS
  joint1 <- below + onleft + above + onright + (img * 10000) 
  joint1[joint1<10000] <- NA  # OK TO HERE
  
  # FOR DEBUGGING PURPOSES
  if(VERBOSE) {
  	print(table(joint1))
  }
  
  # INVERTED PORTION
  # PRODUCE INVERTED IMAGES
  imginv <- img + 1
  imginv[imginv==2] <- 0
  
  # PERFORM IMAGE SHIFTING
  shiftNinv <- rbind(imginv[2:extimg[1],],imginv[1,])
  shiftEinv <- cbind(imginv[,extimg[2]],imginv[,1:extimg[1]-1])
  shiftSinv <- rbind(imginv[extimg[1],],imginv[1:extimg[1]-1,])
  shiftWinv <- cbind(imginv[,2:extimg[2]],imginv[,1])
  
  # CODE SHIFTED DATA BASED ON DIRECTIONAL SHIFT
  shiftNcinv <- shiftNinv * 1
  shiftEcinv <- shiftEinv * 10
  shiftScinv <- shiftSinv * 100
  shiftWcinv <- shiftWinv * 1000

  belowinv <- shiftNcinv * imginv
  onleftinv <- shiftEcinv * imginv
  aboveinv <- shiftScinv * imginv
  onrightinv <- shiftWcinv * imginv

  joint0 <- belowinv + onleftinv + aboveinv + onrightinv + (imginv * 10000)
  joint0[joint0<10000] <- NA


  if(VERBOSE) {
    cat("\n** joint 0**\n")
    print(joint0)
    cat("\n****\n")
  }

  # MAKE ADJUSTMENTS FOR 0-CENTERED COUNTS
  joint2 <- as.character(joint0 * 2)
  joint3 <- gsub("0","1", joint2)
  joint4 <- gsub("2","0", joint3)
  joint4 <- as.matrix(as.integer(joint4))
  dim(joint4) <- extimg #c(5,5)

  if(VERBOSE) {
    cat("\n** joint4 **\n")
    print(joint4)
    cat("\n****\n")
  }

  # WRITE TEMPORARY joint4 TO joint0 FOR CLARITY
  joint0 <- joint4

  # COMBINE THE JOINT TABLES
  joint0[is.na(joint0)] <- joint1[is.na(joint0)]
  joint <- joint0
  
  # IF NOT TORUS, THEN SUBSET THE IMAGE TO NOT INCLUDE THE OUTTER ROW/COL OF IMAGE
  # ADD HERE WHEN READY TO DO SO
  
  
  
  # MAP JOINT TO RESULTS DATA FRAME
  tab <- table(joint)
  ncells <- sum(tab)
  for(row in 1:length(tab)){
    results[results$CODE==as.integer(names(tab[row])),7]<- tab[[row]]
  }
  
  # FILL PROBABILITY COLUMN
  results$PROBABILITY <- results$FREQUENCY/ncells
  
  if(OBJ) {
  	name <- OBJtxt
  } else {
  	name <- IMGCSV
  }
  
  # PROVIDE RETURN OBJECT (AS A LIST WITH PERTINENT INFORMATION INCLUDED)
  results <- list(PROCESSINGDATE=date(), IMAGE=name, NCELLS=ncells, TORUS=TORUS, RHO=rho, PROPORTION=proportion, RESULTS=results, JOINT=joint)
  return(results)

} # END FUNCTION: patternbits
