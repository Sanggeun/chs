% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chs_by_age_sex.R
\name{chs_by_age_sex}
\alias{chs_by_age_sex}
\title{stratified result by age and sex.}
\usage{
chs_by_age_sex(
  data_set,
  de_var = "ph_a0500",
  var_name,
  cluster = "JIJUM_CD",
  by_var = c("sex", "age_10"),
  strata = c("BOGUN_CD", "dong_type", "house_type"),
  weight = "wt",
  digits = 1
)
}
\arguments{
\item{data_set}{data set.}

\item{de_var}{target(dependent) variable.}

\item{var_name}{indicator name.}

\item{cluster}{cluster.}

\item{by_var}{strata variable.}

\item{strata}{strata.}

\item{weight}{weight.}

\item{digits}{integer indicating the number of decimal places.#'}
}
\value{
stratifed rates
}
\description{
chs_by_age_sex function will calculate stratified rate of health indicators.
}
