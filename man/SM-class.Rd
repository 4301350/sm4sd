% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_classes.R
\docType{class}
\name{SM-class}
\alias{SM-class}
\title{Class SM --- An virtual S4 class to represent the raw signal model.}
\description{
Class "SM" is a virtual super class and cornstone of specific model.
}
\section{Slots}{

\describe{
  \item{\code{sigb,epsb,epsp}}{Inherited slots from \code{\link{RS}}.}
}
}

\section{Details}{

Available model extensions in this packages are: \itemize{
 \item \code{\link{SM.dlm}}: dynamic linear model, it's a space-state model to estimate the offset;
 \item \code{\link{SM.HL}}: Hilhorst model, it's a deterministic model where the offset is prefixed.
}
}

\section{Extends}{

From class \code{\link{RS}} directly without extra slots.
}

