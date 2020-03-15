KLPQ <- function(P=c(0.36, 0.48, 0.16), Q=c(0.333, 0.333, 0.333), JITTER=0.0) {
			
  #-----------------------------------------------------
  # 
  # FILENAME:	patternbits.r
  # AUTHOR:     Tarmo K. Remmel
  # DATE:       15 January 2020
  # FUNCTION:	KLPQ()
  # NEEDS:      Two vectors of the same length, representing
  #             the same sampling. These are probabilities that
  #             must sum to 1 in each vector.
  # CALLED BY:	NA
  # CALLS:      NA
  # RETURNS:    Numeric value for Kullback-Leibler divergence
  # NOTES:      No etry in either P or Q can be 0. Thus, the 
  #             very small JITTER value is added to the values
  #             to avoid errors. The JITTER value could easily be
  #             set to 0 if it is known that no P or Q entries
  #             are 0. A tiny JITTER value may be 0.000000001.
  # 
  #-----------------------------------------------------

  # ADD A NEGLIGIBLE AMOUNT TO ALL VALUES TO AVOID PROBLEMS WHERE PROBABILITY IS ZERO
  P <- P + JITTER
  Q <- Q + JITTER
  
  # COMPUTE K-L DIVERGENCE AND RETURN THAT VALUE TO THE CALLING ENVIRONMENT
  return(sum(P*log(P/Q)))	

} # END FUNCTION: KLPQ
