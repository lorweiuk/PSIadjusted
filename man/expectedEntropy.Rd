\name{expectedEntropy}
\alias{expectedEntropy}
\title{
    Computes expected entropy.
}
\description{
    Computes expected entropy over updated SSDs for upcoming trial.
}
\usage{
    entropies <- expectedEntropy( PSIobject )
}
\arguments{
    \item{PSIobject}{a PSI object containing updated SSDs, upcoming posterior distributions (success and failure) and p(Inhibit) per SSD}
}
\value{
returns expected entropy per SSD
}

        