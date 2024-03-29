% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.cre_mod}
\alias{predict.cre_mod}
\title{Ensemble Rule Compression}
\usage{
\method{predict}{cre_mod}(model, newdata, s = "lambda.min")
}
\arguments{
\item{model}{cre model}

\item{newdata}{test data}

\item{s}{if glmnet is used either "lambda.min" or "lambda.1se". Defaul is "lambda.1se"}
}
\value{
list with conditions and rules
}
\description{
Ensemble Rule Compression
}
