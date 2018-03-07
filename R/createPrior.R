createPrior <- function( PSIobject ) {
    # create prior distribution for all participants and all trials
    # with uniform probability distribution
    checkParametersPrior(PSIobject)
    
    trials <- PSIobject$numtrials + 1       # +1 because first distribution is uniform
    thresh <- length(PSIobject$thresholds)
    slopes <- length(PSIobject$slopes)
    errs   <- length(PSIobject$error_rates)
    
    p_prior <- array( 0, dim = c( trials, thresh, slopes, errs ) )  # create prior array
    p_prior[1,,,] = 1 / ( thresh * slopes * errs )                  # fill first prior (first trial) with uniform values
    return( p_prior)
}