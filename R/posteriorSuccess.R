posteriorSuccess <- function( PSIobject ) {
    # compute posteriors, for all possible SSDs, for success and error trials
    thresh  <- length(PSIobject$thresholds)
    slopes  <- length(PSIobject$slopes)
    errs    <- length(PSIobject$error_rates)
    numssds <- length(PSIobject$SSDs)
    
    p_posterior_success <- array( 0, dim = c(thresh, slopes, errs, numssds) )
    
    for (del in 1:length(PSIobject$SSDs)) { # compute posteriors for all possible trials
        p_posterior_success[,,,del] = ( PSIobject$p_prior[PSIobject$Stop_trial,,,] *      PSIobject$p_inhibit_signal[,,,del]  ) / sum(( PSIobject$p_prior[PSIobject$Stop_trial,,,] *      PSIobject$p_inhibit_signal[,,,del]  ))
    }
    
    return( p_posterior_success)
}