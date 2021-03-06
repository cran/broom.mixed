% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TMB_tidiers.R
\name{tidy.TMB}
\alias{tidy.TMB}
\title{Tidying methods for TMB models}
\usage{
\method{tidy}{TMB}(
  x,
  effects = c("fixed", "random"),
  conf.int = FALSE,
  conf.level = 0.95,
  conf.method = c("wald", "uniroot", "profile"),
  ...
)
}
\arguments{
\item{x}{An object of class \code{TMB} (you may need to use
\code{class(obj) <- "TMB"} on your results from TMB)}

\item{effects}{which effects should be returned?}

\item{conf.int}{whether to include a confidence interval}

\item{conf.level}{confidence level for CI}

\item{conf.method}{method for computing confidence intervals}

\item{...}{additional arguments passed to confint function (tmbroot, tmbprofile)}
}
\description{
Tidying methods for TMB models
}
\examples{
if (require("TMB")) {

    \dontrun{
       runExample("simple",thisR=TRUE)
       class(obj) <- "TMB"
       tidy(obj,conf.int=TRUE,conf.method="wald")
    }
    \dontrun{tidy(obj,conf.int=TRUE,conf.method="uniroot")}
    \dontrun{tidy(obj,conf.int=TRUE,conf.method="profile")}
}
}
