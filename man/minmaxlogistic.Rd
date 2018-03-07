\name{minmaxlogistic}
\alias{minmaxlogistic}
\title{
    Psychometric function.
}
\description{
    Gives a psychometric function with 3 free parameters (SSD midpoint, slope, error_rate).
}
\usage{
    p_success <- minmaxlogistic(xin, midpoint, steepness, minimum)
}
\arguments{
    \item{xin}{the SSDs over which to compute p(success)}
    \item{midpoint}{the SSD at which p(success) is .5}
    \item{steepness}{slope of the psychometric function}
    \item{minimum}{the minimum and maximum of the psychometric function}
}
\value{
returns psychometric function over SSDs
}

        