#' coding chs general index.
#'
#' data_coding function will code health indicators using raw data of community health survey.
#'
#' @param year the year investigated.
#' @param data_set raw data of community health survey.
#' @return
#' @examples
#' @export
#' @encoding UTF8

data_coding_general <- function(data_set, year) {
  
  data_set$examin_year <- year
  
  if(year %in% 2009:2018) {
    data_set$spot_no <- data_set$jijum_cd
    data_set$wt_h <- data_set$wt_house
    data_set$wt_p <- data_set$wt
    data_set$dong_ty_code <- data_set$town_t
  }
  
  
  
  ### 나이
  data_set$age_10 <- NA
  data_set$age_10[data_set$age>=19 & data_set$age<=29]<-"19-29"
  data_set$age_10[data_set$age>=30 & data_set$age<=39]<-"30-39"
  data_set$age_10[data_set$age>=40 & data_set$age<=49]<-"40-49"
  data_set$age_10[data_set$age>=50 & data_set$age<=59]<-"50-59"
  data_set$age_10[data_set$age>=60 & data_set$age<=69]<-"60-69"
  data_set$age_10[data_set$age>=70]<-"70이상"
  
  
  data_set$age30 <- ifelse(data_set$age >= 30, 1, 0)
  data_set$age50 <- ifelse(data_set$age >= 50, 1, 0)
  data_set$age65 <- ifelse(data_set$age >= 65, 1, 0)
  
  
  ### 성별
  
  data_set$gender <- ifelse(data_set$sex == 1,"남","여")
  
  data_set$sex_m <- ifelse(data_set$sex == 1, 1, 0)
  data_set$sex_f <- ifelse(data_set$sex == 2, 1, 0)
  
  ### 교육
  data_set$educ <- ifelse(data_set$sob_01z1 %in% c("1","2") |
                            (data_set$sob_01z1 == "3" & data_set$sob_02z1 %in% c("2","3","4")),"1.무학",
                          ifelse((data_set$sob_01z1 == "3" & data_set$sob_02z1 == "1") |
                                   (data_set$sob_01z1 == "4" & data_set$sob_02z1 %in% c("2","3","4")),"2.초등학교",
                                 ifelse((data_set$sob_01z1 == "4" & data_set$sob_02z1 == "1") |
                                          (data_set$sob_01z1 == "5" & data_set$sob_02z1 %in% c("2","3","4")), "3.중학교",
                                        ifelse((data_set$sob_01z1 == "5" & data_set$sob_02z1 == "1") |
                                                 (data_set$sob_01z1 %in% c("6", "7") & data_set$sob_02z1 %in% c("2","3","4")), "4.고등학교",
                                               ifelse((data_set$sob_01z1 %in% c("6","7") & data_set$sob_02z1 %in% "1") |
                                                        (data_set$sob_01z1 =="8" & data_set$sob_02z1 %in% c("1","2","3","4") ),"5.대학교이상",NA   ) ))))
  
  ### 직업
  if (year %in% 2009:2019) {
  data_set$job <- ifelse(data_set$soa_06z1 %in% c("1","2"),"1.전문행정관리",
                         ifelse(data_set$soa_06z1 == "3", "2.사무직",
                                ifelse(data_set$soa_06z1 %in% c("4","5"), "3.판매서비스직",
                                       ifelse(data_set$soa_06z1 == "6", "4.농림어업",
                                              ifelse(data_set$soa_06z1 %in% c("7","8","9"), "5.기능단순노무직",
                                                     ifelse(data_set$soa_06z1 %in% c("10","11","12","13"),"6.기타",NA))))))
  }
  if (year %in% 2020:2021) {
    data_set$job <- ifelse(data_set$soa_06z2 %in% c("1","2"),"1.전문행정관리",
                    ifelse(data_set$soa_06z2 == "3", "2.사무직",
                    ifelse(data_set$soa_06z2 %in% c("4","5"), "3.판매서비스직",
                    ifelse(data_set$soa_06z2 == "6", "4.농림어업",
                    ifelse(data_set$soa_06z2 %in% c("7","8","9"), "5.기능단순노무직",
                    ifelse(data_set$soa_06z2 %in% c("10") | data_set$soa_01z1 ==2,"6.기타",NA))))))
  }
  ### 소득
  
  if (year %in% c(2009:2013, 2018:2021)) {
    if (year %in %c(2009:2013)) {
      income_t <- ifelse(data_set$fma_12z1 == 1 & data_set$fma_20z1 >= 0 & data_set$fma_20z1 <= 77776,
                         round(data_set$fma_13z1/12,1),
                         ifelse(data_set$fma_12z1 == 2 & data_set$fma_20z1 >= 0 & data_set$fma_20z1 <= 77776,
                                round(data_set$fma_20z1,1),NA))
    }
    if (year %in %c(2018:2021)) {
      income_t <- ifelse(data_set$fma_12z1 == 1 & data_set$fma_13z1 >= 0 & data_set$fma_13z1 <= 77776,
                         round(data_set$fma_13z1/12,1),
                         ifelse(data_set$fma_12z1 == 2 & data_set$fma_14z1 >= 0 & data_set$fma_14z1 <= 77776,
                              round(data_set$fma_14z1,1),NA))
    }
    data_set$income <- ifelse(income_t >= 0 & income_t < 50, "1.50만원미만",
                              ifelse(income_t < 100, "2.50-100만원미만",
                                     ifelse(income_t < 200, "3.100-200만원미만",
                                            ifelse(income_t < 300, "4.200-300만원미만",
                                                   ifelse(income_t < 400, "5.300-400만원미만",
                                                          ifelse(income_t < 500, "6.400-500만원미만",
                                                                 ifelse(income_t < 600, "7.500-600만원미만",
                                                                        ifelse(income_t >= 600, "8.600만원이상", NA))))))))
    
    data_set$income_2 <- ifelse(income_t >= 0 & income_t < 100, "1.100만원미만",
                                ifelse(income_t < 200, "2.100-200만원미만",
                                       ifelse(income_t < 300, "3.200-300만원미만",
                                              ifelse(income_t < 400, "4.300-400만원미만",
                                                     ifelse(income_t >= 400, "5.400만원이상", NA)))))
    
    data_set$income_3 <- ifelse(income_t >= 0 & income_t < 200, "200만원미만",
                                ifelse(income_t < 400, "200-400만원미만",
                                       ifelse(income_t < 600, "400-600만원미만",
                                              ifelse(income_t >= 600, "600만원이상", NA))))
  }
  
  if (year %in% 2014:2017) {
    data_set$income <- ifelse(data_set$fma_24z1 == 1, "1.50만원미만",
                              ifelse(data_set$fma_24z1 == 2, "2.50-100만원미만",
                                     ifelse(data_set$fma_24z1 == 3, "3.100-200만원미만",
                                            ifelse(data_set$fma_24z1 == 4, "4.200-300만원미만",
                                                   ifelse(data_set$fma_24z1 == 5, "5.300-400만원미만",
                                                          ifelse(data_set$fma_24z1 == 6, "6.400-500만원미만",
                                                                 ifelse(data_set$fma_24z1 == 7, "7.500-600만원미만",
                                                                        ifelse(data_set$fma_24z1 == 8, "8.600만원이상", NA))))))))
    
    data_set$income_2 <- ifelse(data_set$fma_24z1 %in% 1:2, "1.100만원미만",
                                ifelse(data_set$fma_24z1 == 3, "2.100-200만원미만",
                                       ifelse(data_set$fma_24z1 == 4, "3.200-300만원미만",
                                              ifelse(data_set$fma_24z1 == 5, "4.300-400만원미만",
                                                     ifelse(data_set$fma_24z1 %in% 6:8, "5.400만원이상", NA)))))
    
    data_set$income_3 <- ifelse(data_set$fma_24z1 %in% 1:3, "200만원미만",
                                ifelse(data_set$fma_24z1 %in% 4:5, "200-400만원미만",
                                       ifelse(data_set$fma_24z1 %in% 6:7, "400-600만원미만",
                                              ifelse(data_set$fma_24z1 == 8, "600만원이상", NA))))
  }
  
  
  
  
  ### 세대
  if (year %in% c(2011,2013)) {
    data_set$fma_19z1 <- data_set$fma_19z2 # family type
  }
  if (year %in% 2009:2019) {
  data_set$generation <- ifelse(data_set$fma_19z1 %in% 1:7, "1세대",
                                ifelse(data_set$fma_19z1 %in% 8:16, "2세대",
                                       ifelse(data_set$fma_19z1 %in% 17:19, "3세대",NA)))
  }
  if (year %in% 2020:2021) {
    data_set$generation <- ifelse(data_set$fma_19z3 %in% 1:3, "1세대",
                                  ifelse(data_set$fma_19z3 %in% 4:6, "2세대",
                                         ifelse(data_set$fma_19z3 %in% 7, "3세대",NA)))
  }
  
  ### 동읍면
  data_set$town <- ifelse(data_set$dong_ty_code == 1, "1.동",
                          ifelse(data_set$dong_ty_code == 2, "2.읍면", NA))
                          
  return(data_set)
}


                          
