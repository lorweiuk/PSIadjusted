\name{predictRT}
\alias{predictRT}
\title{
    Predict upcoming RT.
}
\description{
    Predict upcoming RT based on linear trend.
}
\usage{
    upcoming_RT <- predictRT( PSIobject )
}
\arguments{
    \item{PSIobject}{a PSI object containing Go-trial, minRTmean, maxRTmean, Go_RTs, and RTrunmeanstart}
}
\value{
returns linearly predicted Go-RT
}

        