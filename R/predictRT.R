predictRT <- function( PSIobject) {
    # linearly predict current Go-RT
    checkParametersPredict( PSIobject )
    
    # if enough trials have been passed to make prediction
    if ( PSIobject$Go_trial > PSIobject$minRTmean ) {
        # do linear regression of GO-RTs against trialnumber, then set current GO-RT to next trial in regressionequation
        if ( !("Go_RTs" %in% names(PSIobject)) ) {
            stop("PSIobj must contain element: Go_RTs.")
        }
        
        regstart <- PSIobject$Go_trial - PSIobject$maxRTmean
        regend   <- PSIobject$Go_trial - 1
        if (regstart < 1) { regstart = 1; }
        
        templm = NULL
        templm = lm( PSIobject$Go_RTs[ regstart:regend ] ~ seq( from=regstart, to=regend, by=1 ) )
        
        RTrunmean = templm$coefficients[1]  +  templm$coefficients[2] * (regend + 1) 
    } else {
        RTrunmean = PSIobject$RTrunmeanstart
    }
    return( RTrunmean )
}