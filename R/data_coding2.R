#' coding chs index.
#'
#' data_coding2 function will code health indicators using raw data of community health survey.
#'
#' @param year the year investigated.
#' @param data_set raw data of community health survey.
#' @return
#' @examples
#' @export
#' @encoding UTF8


data_coding2 <- function(data, year) {

  data$josa_year <- year

  ## 공통영역

  ### 나이
  data$age_10 <- NA
  data$age_10[data$age>=19 & data$age<=29]<-"19-29"
  data$age_10[data$age>=30 & data$age<=39]<-"30-39"
  data$age_10[data$age>=40 & data$age<=49]<-"40-49"
  data$age_10[data$age>=50 & data$age<=59]<-"50-59"
  data$age_10[data$age>=60 & data$age<=69]<-"60-69"
  data$age_10[data$age>=70]<-"70이상"


  data$age30 <- ifelse(data$age >= 30, 1, 0)
  data$age50 <- ifelse(data$age >= 50, 1, 0)
  data$age65 <- ifelse(data$age >= 65, 1, 0)


  ### 성별

  data$gender <- ifelse(data$sex == 1,"남","여")

  data$sex_m <- ifelse(data$sex == 1, 1, 0)
  data$sex_f <- ifelse(data$sex == 2, 1, 0)



  return(data)


}
