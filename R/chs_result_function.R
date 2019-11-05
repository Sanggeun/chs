#' Calculating crude rate.
#'
#' chs_c_rate function will calculate crude rate of health indicators.
#'
#' @param data data set.
#' @param by_var strata variable.
#' @param de_var target(dependent) variable.
#' @param cluster cluster.
#' @param strata strate.
#' @param index_subset denominator condition by health indicators.
#' @param josa_year the year of data.
#' @param index_year the years in which the target variable was investigated.
#' @param weight weight.
#' @param digits integer indicating the number of decimal places.#'
#' @return crude rate of health indicators
#' @examples
#' @export
#' @importFrom dplyr filter
#' @importFrom survey svydesign
#' @importFrom survey svymean

chs_c_rate <- function(data, by_var = "sex" , de_var = "sm_a0100", cluster = "JIJUM_CD",
                       strata=c("BOGUN_CD","dong_type","house_type"), index_subset = NULL,
                       josa_year , index_year = 2011:2018, var_cat = 1,
                       weight="wt",digits=1) {

  # 지표별 조사연도(조사되지 않은 연도가 있음)
  if (!all(index_year %in% josa_year)) {
    data <- data[data$josa_year %in% josa_year,]
  }

  # 지표별 분모의 조건
  if (!is.null(index_subset)) {
    if(length(index_subset) == 1) {
      data <- data %>%
        filter(data[[index_subset]]==1)
    } else if (length(index_subset == 2)) {
      data <- data %>%
        filter(data[[index_subset[1]]]==1, data[[index_subset[2]]]==1)
    }
  }

  data$target <- data[[de_var]]
  chs_design_1 <- svydesign(id=as.formula(paste0("~",cluster)),
                            strata = as.formula(paste0("~",paste(strata, collapse = "+"))),
                            weights = as.formula(paste0("~",weight)), nest = TRUE ,data=data)
  options(survey.lonely.psu = "adjust")

  #n < 50 : Don't analyze(low reliability)

  if (NROW(data) >= 50 | sum(data[[de_var]] == 1) >= 1 ) {

    if (var_cat == 1) {

      a <- svymean(~factor(target), chs_design_1, target==1, se=T, na.rm=T, deff=T, ci=T, keep.vars=T)
      b <- margin.table(table(data[[de_var]]))
      c <- table(data[[de_var]])

      result <- c(
        by_var = data[[by_var]][1],
        total = b,
        n = c[2],
        crude_rate = formatC(round(coef(a)[2]*100,digits),digits,format = "f"),
        low_CI = formatC(round(confint(a)[2]*100,digits),digits,format = "f"),
        upper_cI = formatC(round(confint(a)[4]*100,digits),digits,format = "f")

      )
    } else if (var_cat == 2) {
      a <- svymean(~target, na.rm = TRUE, chs_design_1)
      b <- margin.table(table(data[[de_var]]))

      result <- c(
        by_var = data[[by_var]][1],
        total = b,
        n = NA,
        crude_rate = formatC(round(coef(a)[1],digits),digits,format = "f"),
        low_CI = formatC(round(confint(a)[1],digits),digits,format = "f"),
        upper_cI = formatC(round(confint(a)[2],digits),digits,format = "f")

      )
    }

  } else {
    result <- c(by_var = data[[by_var]][1],
                total = NROW(data),
                rep(NA,4)
                )
  }
  return(result)
}



#' Calculating crude mean.
#'
#' chs_c_meane function will calculate crude mean.
#'
#' @param data data set.
#' @param by_var strata variable.
#' @param de_var target(dependent) variable.
#' @param cluster cluster.
#' @param strata strate.
#' @param index_subset denominator condition by health indicators.
#' @param josa_year the year of data.
#' @param index_year the years in which the target variable was investigated.
#' @param weight weight.
#' @param digits integer indicating the number of decimal places.#'
#' @return crude rate of health indicators
#' @examples
#' @export
#' @importFrom dplyr filter
#' @importFrom survey svydesign
#' @importFrom survey svymean

chs_c_mean <- function(data, by_var = "sex" , de_var = "sm_a0100", cluster = "JIJUM_CD",
                       strata=c("BOGUN_CD","dong_type","house_type"), index_subset = NULL,
                       josa_year , index_year = 2011:2018, var_cat = 1,
                       weight="wt",digits=1) {

  # 지표별 조사연도(조사되지 않은 연도가 있음)
  if (!all(index_year %in% josa_year)) {
    data <- data[data$josa_year %in% josa_year,]
  }

  # 지표별 분모의 조건
  if (!is.null(index_subset)) {
    if(length(index_subset) == 1) {
      data <- data %>%
        filter(data[[index_subset]]==1)
    } else if (length(index_subset == 2)) {
      data <- data %>%
        filter(data[[index_subset[1]]]==1, data[[index_subset[2]]]==1)
    }
  }

  data$target <- data[[de_var]]
  chs_design_1 <- svydesign(id=as.formula(paste0("~",cluster)),
                            strata = as.formula(paste0("~",paste(strata, collapse = "+"))),
                            weights = as.formula(paste0("~",weight)), nest = TRUE ,data=data)
  options(survey.lonely.psu = "adjust")

  #n < 50 : Don't analyze(low reliability)

  if (NROW(data) >= 50) {

    if (var_cat == 1) {

      a <- svymean(~target, chs_design_1)

      result <- c(
        by_var = data[[by_var]][1],
        a

      )
    } else if (var_cat == 2) {
      a <- svymean(target, chs_design_1)

      result <- c(
        by_var = data[[by_var]][1],
        a
      )
    }

  } else {
    result <- c(by_var = data[[by_var]][1],
    NA, NA)

  }
  return(result)
}


#' Calculating crude rate by the stratification variable.
#'
#' chs_c_rate_by function will calculate crude rate of health indicators by the stratification variable.
#'
#' @param data data set.
#' @param by_var stratification variable.
#' @return crude rate by stratificaton variable.
#' @examples
#' @export
#' @importFrom dplyr filter

chs_c_rate_by <- function(data, by_var, ...) {

  by_var_class <- unique(data[[by_var]])

  result_by_var <- matrix(nrow = length(by_var_class), ncol = 6, byrow = TRUE)

  k <- 0
  for (i in by_var_class) {
    k <- k + 1
    ## 지역사회건강조사 결과
    data_set_tmp <- data %>% filter(data[[by_var]]== i)
    chs_rate <- chs_c_rate(data_set_tmp, by_var = by_var, ...)
    result_by_var[k,] <- chs_rate
  }
  colnames(result_by_var) <- c("By","Total","N","c.rate","LowCI","UpperCI")

  return(result_by_var)
}





