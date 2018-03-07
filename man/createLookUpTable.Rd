\name{createLookUpTable}
\alias{createLookUpTable}
\title{
    Creates table of p(inhibit signal) per SSD.
}
\description{
    Creates table of p(inhibit signal) per SSD, per parameter combination (thresholds x slopes x error rates). Psychometric function used in minmaxlogistic.
}
\usage{
    p_inhibit_signal <- createLookUpTable( PSIobject )
}
\arguments{
    \item{PSIobject}{a PSI object containing thresholds, slopes, error_rates, updated SSDs and predicted RT}
}
\value{
returns lookup table
}

        