\name{pInhibitPerSSD}
\alias{pInhibitPerSSD}
\title{
    Compute overall p(Success) per SSD.
}
\description{
    Compute overall p(Success) per SSD given the lookup-table and prior distribution.
}
\usage{
    success_per_SSD <- pInhibitPerSSD( PSIobject )
}
\arguments{
    \item{PSIobject}{a PSI object containing updated SSDs, p(Success)-lookup table and current prior distribution}
}
\value{
returns p(Success) per SSD, over all parameter combinations
}

        