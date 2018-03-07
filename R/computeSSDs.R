computeSSDs <- function( PSIobject ) {
    # determine new SSDs: subtract SSRTs from mean RT (to get critical SSD per SSRT),
    # then round to PSIadj$SSDresolution, trunc away negative values
    checkParametersSSDs(PSIobject)
    
    SSDs = seq( from = min( PSIobject$thresholds ), to = max( PSIobject$thresholds ), by = PSIobject$SSDresolution ) # as many SSDs as possible SSRTs, but they are determined anew each stop-trial
    SSDs = rev( PSIobject$RTrunmean - SSDs )
    SSDs = round( SSDs / PSIobject$SSDresolution ) * PSIobject$SSDresolution
    SSDs = SSDs[ SSDs >= 0 ]
    if (length(SSDs) == 0) {
        SSDs = c(0)
    }
    return( SSDs )
}