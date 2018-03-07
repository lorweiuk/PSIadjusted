pInhibitPerSSD <- function( PSIobject ) {
    # compute likelihood error per ssd, over all parameter combination
    checkParametersPinhibit(PSIobject)
    
    p_inhibit_per_SSD <- rep( 0, times = length( PSIobject$SSDs ) )
    
    for (ix in 1:length(PSIobject$SSDs)) { # compute p-inhibit over all parameters
        p_inhibit_per_SSD[ix] = sum( PSIobject$p_inhibit_signal[,,,ix] * PSIobject$p_prior[ PSIobject$Stop_trial,,, ] )
    }
    
    return( p_inhibit_per_SSD )
}