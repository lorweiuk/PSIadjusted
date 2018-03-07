minmaxlogistic <- function(xin, midpoint, steepness, minimum) {
    # computes a logistic function with midpoint
    # and equal minimum and maximum values = minimum
    y = minimum + (1-2*minimum)/(1+exp(-steepness*(xin - midpoint)));
    return(y);
}