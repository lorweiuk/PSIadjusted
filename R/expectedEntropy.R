expectedEntropy <- function( PSIobject ) {
    # computes first the entropies for each possible trial (SSDs x outcome),
    # then the expected future entropies based on likelihood of error,success
    checkParametersEntropy(PSIobject)
    
    future_entropy          <- matrix( 0, nrow  = length( PSIobject$SSDs), ncol=2 )
    expected_future_entropy <- rep(    0, times = length( PSIobject$SSDs ) )
    
    # and their entropies
    for (del in 1:length(PSIobject$SSDs)) { # compute posteriors entropies
        future_entropy[del,1] = -sum( apply(PSIobject$p_posterior_success[,,,del], MARGIN=1, sum) * (log(apply(PSIobject$p_posterior_success[,,,del],MARGIN=1,sum))));
        future_entropy[del,2] = -sum( apply(PSIobject$p_posterior_failure[,,,del], MARGIN=1, sum) * (log(apply(PSIobject$p_posterior_failure[,,,del],MARGIN=1,sum))));
    }
    expected_future_entropy <- future_entropy[,1] * PSIadj$p_inhibit_per_SSD + future_entropy[,2] * (1 - PSIadj$p_inhibit_per_SSD); # compute expected entropy
    
    return( expected_future_entropy )
}