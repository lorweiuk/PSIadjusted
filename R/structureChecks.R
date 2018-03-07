
checkParametersPrior <- function( PSIobj ) {
    # check whether parameters are present
    # ...before createPrior
    if ( !("thresholds" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: thresholds.")
    }
    if ( !("slopes" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: slopes.")
    }
    if ( !("error_rates" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: error_rates.")
    }
    if ( !("numtrials" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: numtrials.")
    }
}

checkParametersPredict <- function( PSIobj ) {
    # ... before predictRT
    if ( !("Go_trial" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: Go_trial.")
    }
    if ( !("minRTmean" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: minRTmean.")
    }
    if ( !("maxRTmean" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: maxRTmean.")
    }
    if ( !("RTrunmeanstart" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: RTrunmeanstart.")
    }
}

checkParametersSSDs <- function( PSIobj ) {
    # ... create ssds
    if ( !("RTrunmean" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: RTrunmean.")
    }
    if ( !("SSDresolution" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: SSDresolution.")
    }
}

checkParametersLookup <- function( PSIobj ) {
    # ... create lookup table
    if ( !("SSDs" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: SSDs.")
    }
}

checkParametersPinhibit <- function( PSIobj ) {
    # ... p inhibit per ssd
    if ( !("p_inhibit_signal" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: p_inhibit_signal.")
    }
    if ( !("p_prior" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: p_prior.")
    }
    if ( !("Stop_trial" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: Stop_trial.")
    }
}

checkParametersEntropy <- function( PSIobj ) {
    # ... expected entropy
    if ( !("p_posterior_success" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: p_posterior_success.")
    }
    if ( !("p_posterior_failure" %in% names(PSIobj)) ) {
        stop("PSIobj must contain element: p_posterior_failure.")
    }
}