createLookUpTable   <- function( PSIobject ) {
    # create p_inhibit_signal look-up-table for PSI object:
    # per parameter combination, error-likelihood for different SSDs
    checkParametersLookup(PSIobject)
    
    thresh  <- length(PSIobject$thresholds)
    slopes  <- length(PSIobject$slopes)
    errs    <- length(PSIobject$error_rates)
    numssds <- length(PSIobject$SSDs)
    
    # array: per thresh x slope x error_rate combination, one psychometric function of lenth numssds
    p_inhibit_signal <- array( 0, dim = c(thresh, slopes, errs, numssds) )
    
    for (shape in 1:thresh) {
        for (scale in 1:slopes) {
            for (err in 1:errs) {
                p_inhibit_signal[shape, scale, err, 1:numssds] = minmaxlogistic( PSIobject$SSDs, midpoint = PSIobject$RTrunmean - PSIobject$thresholds[shape], steepness = PSIobject$slopes[scale], minimum = PSIobject$error_rates[err])
            }
        }
    }
    p_inhibit_signal = 1 - p_inhibit_signal # invert to likelihood-of-correct-response
    return( p_inhibit_signal )
}