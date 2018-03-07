\docType{package}
\name{PSIadjusted-package}
\alias{PSIadjusted-package}
\title{
    PSI adjusted method for the Stop-signal task.
}
\description{
    Provides a reaction-time adjusted PSI algorithm for use in the Stop-signal task and related
    behavioral tasks. The PSI method was originally proposed in the field of psychophysics (Kontsevich
    & Tyler, Vision Res, 1999), and was adapted for the Stop-signal task by Livesey & Livesey (PLoS ONE, 2017).
    
    The process of estimating psychometric functions from behavior can require large amounts of trials. In
    order to shorten this process, Kontsevich & Tyler proposed an optimal Bayesian sampling algorithm. This
    alogrithm was applied to estimating the so-called "inhibition function" in the Stop-signal task by Livesey
    & Livesey, with largely constant reaction-times. In order to make this algorithm viable for Stop-signal
    tasks when participants increase their Go reaction-times throughout over time, Weise, Boecker, Gauggel,
    Falkenburger and Drueke proposed an RT-adjusted version of this algorithm, which makes a linear prediction
    about the current Go-RT and estimates the difference between the psychometric function's midpoint (the "critical
    SSD") and the Go-RT.
    
    How to use PSIadjusted:
    # create a PSI object with the necessary information
    PSIadj                 <- list()
    PSIadj$numtrials       <- 50   # number of Stop-trials
    PSIadj$Go_trial        <- 1    # index of current Go-trial
    PSIadj$Stop_trial      <- 1    # index of current Stop-trial
    PSIadj$minRTmean       <- 15   # minimum number of Go-trials before estimating Go-RT; has to be >= 2
    PSIadj$maxRTmean       <- 40   # max number of Go-trials used for estimation
    PSIadj$RTrunmeanstart  <- 400  # mean Go-RT estimate before linear prediction starts (before number of Go-trials >= PSIadj&minRTmean)
    PSIadj$RTrunmean       <- PSIadj$RTrunmeanstart; # running average of GO-RT
    PSIadj$Go_RTs          <- c()
    PSIadj$thresholds      <- seq( from = -100, to = 400, by = 5 ) # SSRTs estimated; method assumes these are ordered from small to large
    PSIadj$SSDresolution   <- 50   # resolution of SSDs offered
    PSIadj$slopes          <- c( 0.003, 0.0052, 0.010, 0.019, 0.029, 0.04 )
    PSIadj$error_rates     <- seq( from = 0, to = 0.3, by = 0.05 )
    
    PSIadj$p_prior <- \code{createPrior( PSIadj )} # create prior distrubtion (uniform)
    
    for (trial in 1:trialnumber) {
        # if enough trials have been passed to make prediction
        PSIadj$RTrunmean <- \code{predictRT( PSIadj )}
    
        # compute new SSDs
        PSIadj$SSDs <- \code{computeSSDs( PSIadj )}
        
        # compute new p_inhibit_signal (because SSDs have changed)
        PSIadj$p_inhibit_signal <- \code{createLookUpTable( PSIadj )} # create p(inhibit)-lookup table
        
        # compute error-likelihood per ssd, over all parameters
        PSIadj$p_inhibit_per_SSD <- \code{pInhibitPerSSD( PSIadj )}
        
        # make space for posteriors
        PSIadj$p_posterior_success    <- \code{posteriorSuccess( PSIadj )}
        PSIadj$p_posterior_failure    <- \code{posteriorFailure( PSIadj )}
        
        # compute expected entropy
        PSIadj$expected_future_entropy <- \code{expectedEntropy( PSIadj )}
        
        # which SSD to use on upcoming Stop-trial
        PSIadj$SSDs_used[PSIadj$Stop_trial] = PSIadj$SSDs[ which(PSIadj$expected_future_entropy == min(PSIadj$expected_future_entropy))[1] ]
        
        if (trial == Stop_trial) {
            # check whether success or error
            PSIadj$Stop_trial = PSIadj$Stop_trial + 1
            
            # save posterior distribution based on behavior
            if (sucess) { # if trial was success, copy posterior_success to p_prior
                PSIadj$p_prior[ PSIadj$Stop_trial,,, ] = PSIadj$p_posterior_success[,,, which(PSIadj$expected_future_entropy==min(PSIadj$expected_future_entropy))[1] ]
            } else if (error) {
                PSIadj$p_prior[ PSIadj$Stop_trial,,, ] = PSIadj$p_posterior_failure[,,, which(PSIadj$expected_future_entropy==min(PSIadj$expected_future_entropy))[1] ]
            }
        } else if (trial == Go_trial) {
            # get participant's Go-RT
            PSIadj$Go_RTs  = c( PSIadj$Go_RTs, GORT )
            
            PSIadj$Go_trial = PSIadj$Go_trial + 1
        }
    }
    
    
}
\references{
    Kontsevich, L.L., Tyler, C.W. (1999). Bayesian adaptive estimation of psychometric slope and threshold.
    \emph{Vision Res. 1999 Aug;39}(16):2729-37.
    Livesey, E. J. & Livesey, D. J. (1999). Validation of a Bayesian Adaptive Estimation Technique in the
    Stop-Signal Task. \emph{PLoS ONE 11}(11)e0165525. \doi{10.1371/journal.pone.0165525}
}
\author{
Lorenz Weise \email{lweise@ukaachen.de}
}
\keyword{package}
