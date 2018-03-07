\name{posteriorFailure}
\alias{posteriorFailure}
\title{
    Compute posterior distribution in case of failure.
}
\description{
    Compute posterior distribution in case of failure (no inhibit).
}
\usage{
    posterior_failure <- posteriorFailure( PSIobject )
}
\arguments{
    \item{PSIobject}{a PSI object containing updated thresholds, slopes, error_rates, updated SSDs, prior distribution and p(Success) lookup table}
}
\value{
returns posterior distribution for error response
}

        