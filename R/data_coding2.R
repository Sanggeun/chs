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

  ### 교육
  data$educ <- ifelse(data$sob_01z1 %in% c("1","2") |
                            (data$sob_01z1 == "3" & data$sob_02z1 %in% c("2","3","4")),"1.무학",
                          ifelse((data$sob_01z1 == "3" & data$sob_02z1 == "1") |
                                   (data$sob_01z1 == "4" & data$sob_02z1 %in% c("2","3","4")),"2.초등학교",
                                 ifelse((data$sob_01z1 == "4" & data$sob_02z1 == "1") |
                                          (data$sob_01z1 == "5" & data$sob_02z1 %in% c("2","3","4")), "3.중학교",
                                        ifelse((data$sob_01z1 == "5" & data$sob_02z1 == "1") |
                                                 (data$sob_01z1 %in% c("6", "7") & data$sob_02z1 %in% c("2","3","4")), "4.고등학교",
                                               ifelse((data$sob_01z1 %in% c("6","7") & data$sob_02z1 %in% "1") |
                                                        (data$sob_01z1 =="8" & data$sob_02z1 %in% c("1","2","3","4") ),"5.대학교이상",NA   ) ))))

  ### 직업
  data$job <- ifelse(data$soa_05z1 %in% c("1","2"),"1.전문행정관리",
                         ifelse(data$soa_05z1 == "3", "2.사무직",
                                ifelse(data$soa_05z1 %in% c("4","5"), "3.판매서비스직",
                                       ifelse(data$soa_05z1 == "6", "4.농림어업",
                                              ifelse(data$soa_05z1 %in% c("7","8","9"), "5.기능단순노무직",
                                                     ifelse(data$soa_05z1 %in% c("10","11","12","13"),"6.기타",NA))))))
  ### 소득

  if (year %in% c(2011:2013, 2018)) {
    income_t <- ifelse(data$fma_12z1 == 1 & data$fma_13z1 >= 0 & data$fma_13z1 <= 77776,
                       round(data$fma_13z1/12,1),
                       ifelse(data$fma_12z1 == 2 & data$fma_14z1 >= 0 & data$fma_14z1 <= 77776,
                              round(data$fma_14z1,1),NA))

    data$income <- ifelse(income_t >= 0 & income_t < 50, "1.50만원미만",
                              ifelse(income_t < 100, "2.50-100만원미만",
                                     ifelse(income_t < 200, "3.100-200만원미만",
                                            ifelse(income_t < 300, "4.200-300만원미만",
                                                   ifelse(income_t < 400, "5.300-400만원미만",
                                                          ifelse(income_t < 500, "6.400-500만원미만",
                                                                 ifelse(income_t < 600, "7.500-600만원미만",
                                                                        ifelse(income_t >= 600, "8.600만원이상", NA))))))))

    data$income_2 <- ifelse(income_t >= 0 & income_t < 100, "1.100만원미만",
                                ifelse(income_t < 200, "2.100-200만원미만",
                                       ifelse(income_t < 300, "3.200-300만원미만",
                                              ifelse(income_t < 400, "4.300-400만원미만",
                                                     ifelse(income_t >= 400, "5.400만원이상", NA)))))

    data$income_3 <- ifelse(income_t >= 0 & income_t < 200, "200만원미만",
                                ifelse(income_t < 400, "200-400만원미만",
                                       ifelse(income_t < 600, "400-600만원미만",
                                              ifelse(income_t >= 600, "600만원이상", NA))))
  }

  if (year %in% 2014:2017) {
    data$income <- ifelse(data$fma_24z1 == 1, "1.50만원미만",
                              ifelse(data$fma_24z1 == 2, "2.50-100만원미만",
                                     ifelse(data$fma_24z1 == 3, "3.100-200만원미만",
                                            ifelse(data$fma_24z1 == 4, "4.200-300만원미만",
                                                   ifelse(data$fma_24z1 == 5, "5.300-400만원미만",
                                                          ifelse(data$fma_24z1 == 6, "6.400-500만원미만",
                                                                 ifelse(data$fma_24z1 == 7, "7.500-600만원미만",
                                                                        ifelse(data$fma_24z1 == 8, "8.600만원이상", NA))))))))

    data$income_2 <- ifelse(data$fma_24z1 %in% 1:2, "1.100만원미만",
                                ifelse(data$fma_24z1 == 3, "2.100-200만원미만",
                                       ifelse(data$fma_24z1 == 4, "3.200-300만원미만",
                                              ifelse(data$fma_24z1 == 5, "4.300-400만원미만",
                                                     ifelse(data$fma_24z1 %in% 6:8, "5.400만원이상", NA)))))

    data$income_3 <- ifelse(data$fma_24z1 %in% 1:3, "200만원미만",
                                ifelse(data$fma_24z1 %in% 4:5, "200-400만원미만",
                                       ifelse(data$fma_24z1 %in% 6:7, "400-600만원미만",
                                              ifelse(data$fma_24z1 == 8, "600만원이상", NA))))
  }




  ### 세대
  if (year %in% c(2011,2013)) {
    data$fma_19z1 <- data$fma_19z2 # family type
  }
  data$generation <- ifelse(data$fma_19z1 %in% 1:7, "1세대",
                                ifelse(data$fma_19z1 %in% 8:16, "2세대",
                                       ifelse(data$fma_19z1 %in% 17:19, "3세대",NA)))

  ### 동읍면
  if (year %in% 2011:2018) {
    data$town <- ifelse(data$town_t == 1, "1.동",
                            ifelse(data$town_t == 2, "2.읍면", NA))
  }

  if (year == 2019) {
    data$town <- ifelse(data$DONG_TYPE == 1, "1.동",
                            ifelse(data$DONG_TYPE == 2, "2.읍면", NA))
  }



}
