#' coding chs index.
#'
#' data_coding_inspec function will code health indicators using raw data of community health survey.
#'
#' @param year the year investigated.
#' @param data_set raw data of community health survey.
#' @return
#' @examples
#' @export
#' @encoding UTF8

data_coding_inspec <- function(data_set, year) {

  data_set$josa_year <- year

  if(year == 2011) {
    data_set$CITY_CD <- data_set$city_cd
    data_set$JIJUM_CD <- data_set$jijum_cd
    data_set$BOGUN_CD <- data_set$bogun_cd
    data_set$dong_p <- data_set$dong
    data_set$sod_02z2 <- data_set$sod_02z1

    data_set$dong_p[data_set$dong_p == "논공읍공단출장소"] <- "논공읍"
    data_set$dong_p[data_set$dong_p == "다사읍서재출장소"] <- "다사읍"
    data_set$dong_p[data_set$dong_p %in% c('대현1동','대현2동')] <- "대현동"
    data_set$dong_p[data_set$dong_p %in% c('동인1.2.4가동','동인3가동')] <- "동인동"

  }

  if(year == 2012) {
    data_set$CITY_CD <- data_set$city_cd
    data_set$JIJUM_CD <- data_set$jijum_cd
    data_set$BOGUN_CD <- data_set$bogun_cd
    data_set$dong_p <- data_set$dong
    data_set$sod_02z2 <- data_set$sod_02z1

    data_set$dong_p[data_set$dong_p == "논공읍공단출장소"] <- "논공읍"
    data_set$dong_p[data_set$dong_p == "다사읍서재출장소"] <- "다사읍"
  }

  if (year == 2013) {
    data_set$sod_02z2 <- data_set$sod_02z1
  }

  if (year %in% 2017:2018) {
    data_set$wt_house <- data_set$wt_h
  }

  if (year == 2019) {
    data_set$dong_p <- data_set$DONG_P
    data_set$wt_house <- data_set$wt_h
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
  data_set$job <- ifelse(data_set$soa_06z1 %in% c("1","2"),"1.전문행정관리",
                         ifelse(data_set$soa_06z1 == "3", "2.사무직",
                                ifelse(data_set$soa_06z1 %in% c("4","5"), "3.판매서비스직",
                                       ifelse(data_set$soa_06z1 == "6", "4.농림어업",
                                              ifelse(data_set$soa_06z1 %in% c("7","8","9"), "5.기능단순노무직",
                                                     ifelse(data_set$soa_06z1 %in% c("10","11","12","13"),"6.기타",NA))))))
  ### 소득

  if (year %in% c(2011:2013, 2018)) {
    income_t <- ifelse(data_set$fma_12z1 == 1 & data_set$fma_13z1 >= 0 & data_set$fma_13z1 <= 77776,
                       round(data_set$fma_13z1/12,1),
                       ifelse(data_set$fma_12z1 == 2 & data_set$fma_14z1 >= 0 & data_set$fma_14z1 <= 77776,
                              round(data_set$fma_14z1,1),NA))

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
  data_set$generation <- ifelse(data_set$fma_19z1 %in% 1:7, "1세대",
                                ifelse(data_set$fma_19z1 %in% 8:16, "2세대",
                                       ifelse(data_set$fma_19z1 %in% 17:19, "3세대",NA)))

  ### 동읍면
  if (year %in% 2011:2018) {
  data_set$town <- ifelse(data_set$dong_type == 1, "1.동",
                          ifelse(data_set$dong_type == 2, "2.읍면", NA))
  }

  if (year == 2019) {
    data_set$town <- ifelse(data_set$DONG_TYPE == 1, "1.동",
                            ifelse(data_set$DONG_TYPE == 2, "2.읍면", NA))
  }

  # 건강행태

  ##  흡연


    ###1. 현재 흡연율

    data_set$sm_a0100 <- NA
    data_set$sm_a0100[data_set$sma_03z2==8] <- NA
    data_set$sm_a0100[data_set$sma_03z2 %in% c(1,2)] <- 1 # current smoker
    data_set$sm_a0100[data_set$sma_03z2 == 3] <- 0
    data_set$sm_a0100[data_set$sma_01z2 == 2] <- 0

    ###2. 평생 흡연율

    if (year %in% 2011:2018) {
    data_set$sm_a0200 <- ifelse(data_set$sma_01z2 == 1, 1,
                                ifelse(data_set$sma_01z2 == 2, 0, NA))

    ### 3. 흡연시작연령
    data_set$sm_a0300 <- ifelse(data_set$sma_01z2 == 1 &
                                  (data_set$sma_02z1>=0 & data_set$sma_02z1<=110) &
                                  (data_set$sma_02z1>=0 & data_set$sma_02z1<=data_set$age),
                                data_set$sma_02z1, NA)


    ### 4. 매일 흡연자의 하루 평균 흡연량

    #### 분모정의(매일 흡연자)
    data_set$sm_a0400 <- ifelse(data_set$sma_01z2 == 1,
                                ifelse(data_set$sma_03z2 == 1, 1,
                                       ifelse(data_set$sma_03z2 %in% c(2,3),0,NA)),
                                ifelse(data_set$sma_01z2 == 2, 0, NA))

    }
    #### 분자정의(흡연량)
    data_set$sm_b0100 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 776,
                                data_set$smb_01z1, NA)

    ### 4-1. 매일 흡연자의 하루 평균 흡연량_10개비 미만
    data_set$sm_b0201 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 9, 1,
                                ifelse(data_set$smb_01z1 >= 10 & data_set$smb_01z1 <= 776, 0, NA))

    ### 4-2. 매일흡연자의 하루평균흡연량_10~19개비
    data_set$sm_b0202 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 9, 0,
                                ifelse(data_set$smb_01z1 >= 10 & data_set$smb_01z1 <= 19, 1,
                                       ifelse(data_set$smb_01z1 >= 20 & data_set$smb_01z1 <= 776, 0, NA)))

    ### 4-3. 매일흡연자의 하루평균흡연량_20~39개비
    data_set$sm_b0203 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 19, 0,
                                ifelse(data_set$smb_01z1 >= 20 & data_set$smb_01z1 <= 39, 1,
                                       ifelse(data_set$smb_01z1 >= 40 & data_set$smb_01z1 <= 776, 0, NA)))

    ### 4-4. 매일흡연자의 하루평균흡연량_40개비 이상
    data_set$sm_b0204 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 39, 0,
                                ifelse(data_set$smb_01z1 >= 40 & data_set$smb_01z1 <= 776, 1, NA))

    ### 5. 금연시도율

    data_set$sm_d0600 <- NA
    data_set$sm_d0600[data_set$smd_02z2==1] <- 1
    data_set$sm_d0600[data_set$smd_02z2 %in% c(2,3)] <- 0

    ### 6. 현재흡연자의 1개월 내 금연계획률
    data_set$sm_d0500 <- ifelse(data_set$smd_01z2 == 1, 1,
                                ifelse(data_set$smd_01z2 %in% 2:4, 0, NA))


    ### 7. 과거흡연자의 금연기간

    if (year %in% 2011:2014) {
      data_set$sm_d2300 <- NA
      data_set$sm_d2601 <- NA
      data_set$sm_d2602 <- NA
      data_set$sm_d2603 <- NA
      data_set$sm_d2604 <- NA
      data_set$sm_d2605 <- NA
      data_set$sm_d2606 <- NA
    }

    if (year %in% 2015:2017) {
      #### 분모정의
      data_set$sm_d2300 <- ifelse(data_set$sma_01z2 == 1,
                                  ifelse(data_set$sma_03z2 == 3, 1,
                                         ifelse(data_set$sma_03z2 %in% c(1,2), 0,NA)),
                                  ifelse(data_set$sma_01z2 == 2, 0, NA))

      #### 7-1. 과거흡연자의 금연기간 1년 미만
      data_set$sm_d2601 <- ifelse(data_set$smb_09z1 == 1, 1,
                                  ifelse(data_set$smb_09z1 %in% 2:6, 0, NA))

      #### 7-2. 과거흡연자의 금연기간_1년~5년 미만
      data_set$sm_d2602 <- ifelse(data_set$smb_09z1 == 2, 1,
                                  ifelse(data_set$smb_09z1 %in% c(1,3:6), 0, NA))

      #### 7-3. 과거흡연자의 금연기간상_5년~10년 미만
      data_set$sm_d2603 <- ifelse(data_set$smb_09z1 == 3, 1,
                                  ifelse(data_set$smb_09z1 %in% c(1,2,4:6), 0, NA))

      #### 7-4. 과거흡연자의 금연기간_10년~15년 미만
      data_set$sm_d2604 <- ifelse(data_set$smb_09z1 == 4, 1,
                                  ifelse(data_set$smb_09z1 %in% c(1:3,5,6), 0, NA))

      #### 7-5. 과거흡연자의 금연기간_15년~20년 미만
      data_set$sm_d2605 <- ifelse(data_set$smb_09z1 == 5, 1,
                                  ifelse(data_set$smb_09z1 %in% c(1:4,6), 0, NA))

      #### 7-6. 과거흡연자의 금연기간_20년 이상
      data_set$sm_d2606 <- ifelse(data_set$smb_09z1 == 6, 1,
                                  ifelse(data_set$smb_09z1 %in% 1:5, 0, NA))


    }

    ### 8. 현재 비흡연자의 가정실내 간접흡연노출률(11,13년도는 설문문항 차이있음)

    ##### 11,13: 가정의 실내에서 다른 사람이 피우는 담배연기를 맡는 시간은 하루 몇 시간 정도입니까?
    ##### 14~ : 1. 본인을 제외한 가족 중 가정의 실내에서 일상적으로 담배를 피우는 분이 있습니까?
    #####     2. 최근 1주일동안 가정의 실내에서 다른 사람이 피우는 담배연기르 맡은 적이 있습니까?

    if (year == 2012) {
      data_set$sm_e0100 <- NA
      data_set$sm_c0700 <- NA
    }

    if (year %in% c(2011, 2013)) {
      ##### 분모정의
      data_set$sm_e0100 <- ifelse(data_set$sma_01z2 == 2, 1,
                                  ifelse(data_set$sma_01z2 == 1,
                                         ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                                                ifelse(data_set$sma_03z2 == 3, 1, NA)),
                                         NA))

      #### 분자정의
      data_set$sm_c0700 <- ifelse(data_set$smc_03z1 ==1, 0,
                                  ifelse(data_set$smc_03z1 %in% c(2,3), 1,NA))
    }

    if (year %in% 2014:2018) {
      ##### 분모정의
      data_set$sm_e0100 <- ifelse(data_set$sma_01z2 == 2, 1,
                                  ifelse(data_set$sma_01z2 == 1,
                                         ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                                                ifelse(data_set$sma_03z2 == 3, 1, NA)),
                                         NA))
      #### 분자정의
      data_set$sm_c0700 <- ifelse(data_set$smc_08z1 == 1,
                                  ifelse(data_set$smc_09z1 == 1, 1,
                                         ifelse(data_set$smc_09z1 == 2, 0, NA)),
                                  ifelse(data_set$smc_08z1 == 2, 0, NA))

    }

    ### 9. 현재 비흡연자의 직장실내간접흡연노출률
    # 분모: 일을 하고 있는 현재 비흡연자(평생 비흡연자, 과거 흡연자) sm_e0200==1

    # 분모
    data_set$sm_e0200 <- NA

    if (year %in% c(2011, 2013)) {
      data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==2] <- 1
      data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==1 & data_set$sma_03z2 %in% c(1,2)] <- 0
      data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==1 & data_set$sma_03z2 == 3] <- 1
    }

    if (year %in% 2014:2018){
      data_set$sm_e0200[data_set$smc_10z1 %in% c(1,2) & data_set$sma_01z2==2] <- 1
      data_set$sm_e0200[data_set$smc_10z1 %in% c(1,2) & data_set$sma_01z2==1 & data_set$sma_03z2 %in% c(1,2)] <- 0
      data_set$sm_e0200[data_set$smc_10z1 %in% c(1,2) & data_set$sma_01z2==1 & data_set$sma_03z2 == 3] <- 1
    }
    data_set$sm_e0200[data_set$soa_06z1 %in% c(11,12,13)] <- NA

    # 분자
    data_set$sm_c0800 <- NA

    if (year %in% c(2011,2013)){
      data_set$sm_c0800[data_set$smc_05z1 == 1] <- 0
      data_set$sm_c0800[data_set$smc_05z1 %in% c(2,3)] <- 1
    }

    if (year %in% 2014:2018) {
      data_set$sm_c0800[data_set$smc_10z1 == 1] <- 1
      data_set$sm_c0800[data_set$smc_10z1 == 2] <- 0
      data_set$sm_c0800[data_set$smc_10z1 == 3] <- NA
    }
    data_set$sm_c0800[data_set$soa_06z1 %in% c(11,12,13)] <- NA


    ### 10. 현재 비흡연자의 공공장소간접흡연노출률

    #### 분자 정의

    if (year == 2012) {
      data_set$sm_c0500 <- NA
    }
    if (year %in% c(2011, 2013:2018)) {
      data_set$sm_c0500 <- ifelse(data_set$smc_07z1 == 1, 1,
                                  ifelse(data_set$smc_07z1 == 2, 0, NA))
    }

    ### 11. 평생 전자담배 사용경험률
    if (year %in% 2011:2013) {
      data_set$sm_a0800 <- NA
    }

    if (year %in% 2014:2018) {
      data_set$sm_a0800 <- ifelse(data_set$sma_08z1 == 1, 1,
                                  ifelse(data_set$sma_08z1 == 2, 0, NA))
    }

    ### 12. 현재 전자담배 사용경험률

    #### 2014, 2015: 현재 전자담배를 피웁니까?
    #### 2016~ : 최근 1달간 전자담배를 피운 적이 있습니까?

    if (year %in% 2011:2013) {
      data_set$sm_a1000 <- NA
    }

    if (year %in% 2014:2015) {
      data_set$sm_a1000 <- ifelse(data_set$sma_08z1 == 1,
                                  ifelse(data_set$sma_09z1 == 1, 1,
                                         ifelse(data_set$sma_09z1 == 2, 0,NA)),
                                  ifelse(data_set$sma_08z1 == 2, 0, NA))
    }

    if (year %in% 2016:2017) {
      data_set$sm_a1000 <- ifelse(data_set$sma_08z1 == 1,
                                  ifelse(data_set$sma_11z1 == 1, 1,
                                         ifelse(data_set$sma_11z1 == 2, 0,NA)),
                                  ifelse(data_set$sma_08z1 == 2, 0, NA))
    }

    ### 13. 연간 금연캠페인 경험률
    if (year %in% c(2014, 2016)) {
      data_set$sm_d0900 <- NA
    }

    if(year %in% c(2011:2013, 2015, 2017)) {
      data_set$sm_d0900 <- ifelse(data_set$smd_06z1 == 1, 1,
                                  ifelse(data_set$smd_06z1 == 2, 0, NA))
    }

    ### 14, 15 연간 금연교육 경험률, 현재 흡연자의 금연교육 경험률

    if (year %in% 2011:2018) {
    data_set$sm_d1000 <- ifelse(data_set$smd_07z1 == 1, 1,
                                ifelse(data_set$smd_07z1 == 2, 2, NA))
    }

    ### 16. 금연구역 인지율
    if(year %in% c(2012, 2014, 2016)) {
      data_set$sm_d1100 <- NA
    }

    if(year %in% c(2011, 2013, 2015, 2017)) {
      data_set$sm_d1100 <- ifelse(data_set$smd_08z1 %in% 1:2, 1,
                                  ifelse(data_set$smd_08z1 == 3, 0, NA))
    }

    ### 17. 현재흡연자의 금연구역 내 흡연 경험률
    if(year %in% c(2012, 2014, 2016)) {
      data_set$sm_d1200 <- NA
      data_set$sm_d1300 <- NA
    }

    if (year %in% c(2011, 2013, 2015, 2017)) {
      #### 분모정의(현재흡연자 + 구체적 금연구역 인지자)
      data_set$sm_d1200 <- ifelse(data_set$sm_a0100 == 1 & data_set$smd_08z1 == 1, 1, 0)

      #### 분자정의
      data_set$sm_d1300 <- ifelse(data_set$smd_09z1 %in% 1:2, 1,
                                  ifelse(data_set$smd_09z1 == 3, 0, NA))
    }



  ### 음주

  #### 월간음주율

  data_set$dr_a0400 <- NA
  data_set$dr_a0400[data_set$dra_01z1 ==2 |data_set$drb_02z1==2 | data_set$drb_01z2 ==1] <- 0
  data_set$dr_a0400[data_set$dra_01z1==1 & data_set$drb_02z1==1 & data_set$drb_01z2 %in% c(2:5)] <- 1

  #### 고위험 음주율

  #성별에 따라 기준이 다름

  data_set$dr_a0500 <- NA
  data_set$dr_a0500[data_set$drb_01z2 %in% c(1:3)] <- 0
  data_set$dr_a0500[data_set$sex=="1" & data_set$drb_01z2 %in% c(4:5) & data_set$drb_03z1 %in% c(1:3)] <- 0
  data_set$dr_a0500[data_set$sex=="1" & data_set$drb_01z2 %in% c(4:5) & data_set$drb_03z1 %in% c(4:5)] <- 1
  data_set$dr_a0500[data_set$sex=="2" & data_set$drb_01z2 %in% c(4:5) & data_set$drb_03z1 %in% c(1:2)] <- 0
  data_set$dr_a0500[data_set$sex=="2" & data_set$drb_01z2 %in% c(4:5) & data_set$drb_03z1 %in% c(3:5)] <- 1



  ### 안전의식

  #### 운전시 안전벨트 착용률

  #분모 sfa_01z1==1

  data_set$sf_a0100 <- NA
  data_set$sf_a0100[data_set$sfa_02z2 %in% c(1:4)] <- 0
  data_set$sf_a0100[data_set$sfa_02z2 == 5] <- 1


  #### 동승차량 앞좌석 안전벨트 착용률

  #분모sf_a0300==1

  # 분모
  data_set$sf_a0300 <- NA

  if (year %in% c(2011, 2012, 2013)) {
    data_set$sf_a0300[data_set$sfa_03z1 ==1] <- 1
    data_set$sf_a0300[data_set$sfa_03z1 ==2] <- 0
  } else if (year %in% c(2014, 2015, 2016, 2018)) {
    data_set$sf_a0300[data_set$sfa_04z3 %in% c(2:5)] <- 1
    data_set$sf_a0300[data_set$sfa_04z3 ==1] <- 0
  }


  # 분자
  data_set$sf_a0400 <- NA
  if (year %in% 2011:2013) {
    data_set$sf_a0400[data_set$sfa_04z2 ==5] <- 1
    data_set$sf_a0400[data_set$sfa_04z2 %in% c(1:4)] <- 0
  } else if (year %in% 2014:2018) {
    data_set$sf_a0400[data_set$sfa_04z3 %in% c(2:4)] <- 0
    data_set$sf_a0400[data_set$sfa_04z3 ==5] <- 1
  }


  #### 음주운전경험률
  #분모 sf_b0400==1


  # 분모
  data_set$sf_b0400 <- NA
  data_set$sf_b0400[data_set$sfa_01z1==1 | data_set$sfa_05z1==1] <- 1
  # 분자
  data_set$sf_b0500<- 0
  data_set$sf_b0500[data_set$sfb_05z2==1 | data_set$sfb_03z2==1] <- 1


  ### 운동 및 신체활동

  data_set$ph_a0500 <- NA
  if (year %in% c(2011:2017, 2019)) {

    #### 중등도이상 신체활동 실천율
    # 격렬한 신체활동
    data_set$pha_05z1 <- as.numeric(data_set$pha_05z1)
    data_set$pha_06z1 <- as.numeric(data_set$pha_06z1)

    data_set$ph_a0100 <- NA
    data_set$ph_a0100[is.na(data_set$pha_06z1) & data_set$pha_05z1 %in% c(0:24)] <-
      data_set$pha_05z1[is.na(data_set$pha_06z1) & data_set$pha_05z1 %in% c(0:24)]*60
    data_set$ph_a0100[data_set$pha_05z1 %in% c(0:24) & data_set$pha_06z1 %in% c(0:60)] <-
      data_set$pha_05z1[data_set$pha_05z1 %in% c(0:24) & data_set$pha_06z1 %in% c(0:60)]*60 + data_set$pha_06z1[data_set$pha_05z1 %in% c(0:24) & data_set$pha_06z1 %in% c(0:60)]
    data_set$ph_a0100[is.na(data_set$pha_05z1) & (data_set$pha_06z1 %in% c(0:60))] <- data_set$pha_06z1[is.na(data_set$pha_05z1) & (data_set$pha_06z1 %in% c(0:60))]

    data_set$ph_a0200 <- NA
    data_set$ph_a0200[data_set$pha_04z1 %in% c(0:2) | (data_set$pha_04z1 %in% c(3:7) & data_set$ph_a0100 <=19)] <- 0
    data_set$ph_a0200[(data_set$pha_04z1 %in% c(3:7) & data_set$ph_a0100 >= 20)] <- 1

    # 중등도 신체활동
    data_set$pha_08z1 <- as.numeric(data_set$pha_08z1)
    data_set$pha_09z1 <- as.numeric(data_set$pha_09z1)

    data_set$ph_a0300 <- NA
    data_set$ph_a0300[is.na(data_set$pha_09z1) & (data_set$pha_08z1 %in% c(0:24))] <-
      data_set$pha_08z1[is.na(data_set$pha_09z1) & (data_set$pha_08z1 %in% c(0:24))]*60
    data_set$ph_a0300[data_set$pha_08z1 %in% c(0:24) & data_set$pha_09z1 %in% c(0:60)] <-
      data_set$pha_08z1[data_set$pha_08z1 %in% c(0:24) & data_set$pha_09z1 %in% c(0:60)]*60 + data_set$pha_09z1[data_set$pha_08z1 %in% c(0:24) & data_set$pha_09z1 %in% c(0:60)]
    data_set$ph_a0300[is.na(data_set$pha_08z1) & (data_set$pha_09z1 %in% c(0:60))] <- data_set$pha_09z1[is.na(data_set$pha_08z1) & (data_set$pha_09z1 %in% c(0:60))]

    data_set$ph_a0400 <- NA
    data_set$ph_a0400[data_set$pha_07z1 %in% c(0:4) | (data_set$pha_04z1 %in% c(5:7) & data_set$ph_a0300 <=29)] <- 0
    data_set$ph_a0400[(data_set$pha_07z1 %in% c(5:7) & data_set$ph_a0300 >= 30)] <- 1

    # 중등도이상 신체활동 실천율

    data_set$ph_a0500 <- NA
    data_set$ph_a0500[data_set$ph_a0200==1 | data_set$ph_a0400==1] <- 1
    data_set$ph_a0500[data_set$ph_a0200==0 & data_set$ph_a0400==0] <- 0

  }

  data_set$pha_aerobic <- NA
  data_set$phc_sedent <- NA
  data_set$pha_work <- NA
  data_set$pha_walk <- NA
  data_set$pha_lei <- NA

  if (year %in% 2018) {
    ## 일과 관련된 고강도 신체활동 시간

    data_set$pha_vig_d <- ifelse(data_set$pha_21z1 == 1 & data_set$pha_22z1 %in% 1:7 &
                                   data_set$pha_23z1 %in% 0:24 & data_set$pha_24z1 %in% 0:60,
                                 (data_set$pha_23z1*60 + data_set$pha_24z1*1), NA)
    data_set$pha_vig <- data_set$pha_22z1*data_set$pha_vig_d

    ## 일과 관련된 중강도 신체활동 시간
    data_set$pha_mod_d <- ifelse(data_set$pha_25z1 == 1 & data_set$pha_26z1 %in% 1:7 &
                                   data_set$pha_27z1 %in% 0:24 & data_set$pha_28z1 %in% 0:60,
                                 (data_set$pha_27z1*60 + data_set$pha_28z1*1), NA)
    data_set$pha_mod <- data_set$pha_26z1*data_set$pha_mod_d

    ## 이동 중 걷기 및 자전거 이용시간
    data_set$pha_walk_d <- ifelse(data_set$pha_29z1 == 1 & data_set$pha_30z1 %in% 1:7 &
                                    data_set$pha_31z1 %in% 0:24 & data_set$pha_32z1 %in% 0:60,
                                  (data_set$pha_31z1*60 + data_set$pha_32z1*1), NA)
    data_set$pha_walk <- data_set$pha_30z1*data_set$pha_walk_d

    ## 여가 관련 고강도 신체활동 시간
    data_set$pha_vig2_d <- ifelse(data_set$pha_33z1 == 1 & data_set$pha_34z1 %in% 1:7 &
                                    data_set$pha_35z1 %in% 0:24 & data_set$pha_36z1 %in% 0:60,
                                  (data_set$pha_35z1*60 + data_set$pha_36z1*1), NA)
    data_set$pha_vig2 <- data_set$pha_34z1*data_set$pha_vig2_d

    ## 여가 관련된 중강도 신체활동 시간
    data_set$pha_mod2_d <- ifelse(data_set$pha_37z1 == 1 & data_set$pha_38z1 %in% 1:7 &
                                    data_set$pha_39z1 %in% 0:24 & data_set$pha_40z1 %in% 0:60,
                                  (data_set$pha_39z1*60 + data_set$pha_40z1*1), NA)
    data_set$pha_mod2 <- data_set$pha_38z1*data_set$pha_mod2_d

    ## 시간 합산
    data_set$pha_vig_t <- ifelse(
      !is.na(data_set$pha_vig) & !is.na(data_set$pha_vig2),
      data_set$pha_vig + data_set$pha_vig2,
      ifelse(!is.na(data_set$pha_vig), data_set$pha_vig,
             ifelse(!is.na(data_set$pha_vig2),data_set$pha_vig2, 0))
    )
    data_set$pha_vig_t2 <- data_set$pha_vig_t*2

    data_set$pha_mod_t <- ifelse(
      #
      !is.na(data_set$pha_mod) & !is.na(data_set$pha_mod2) & !is.na(data_set$pha_walk),
      data_set$pha_mod + data_set$pha_mod2 + data_set$pha_walk,
      #
      ifelse(!is.na(data_set$pha_mod) & !is.na(data_set$pha_mod2),
             data_set$pha_mod + data_set$pha_mod2,
             ifelse(!is.na(data_set$pha_mod) & !is.na(data_set$pha_walk),
                    data_set$pha_mod + data_set$pha_walk,
                    ifelse(!is.na(data_set$pha_mod2) & !is.na(data_set$pha_walk),
                           data_set$pha_mod2 + data_set$pha_walk,
                           ifelse(!is.na(data_set$pha_mod), data_set$pha_mod,
                                  ifelse(!is.na(data_set$pha_mod2), data_set$pha_mod2,
                                         ifelse(!is.na(data_set$pha_walk), data_set$pha_walk, 0)))))))


    data_set$pha_vig_mod_t  <- data_set$pha_vig_t2 + data_set$pha_mod_t

    # 유산소 신체활동 실천율
    data_set$pha_aerobic <- ifelse(
      (data_set$pha_21z1 == 1 & data_set$pha_22z1 %in% 1:7 & !is.na(data_set$pha_vig) |
         data_set$pha_21z1 == 2) &
        (data_set$pha_25z1 == 1 & data_set$pha_26z1 %in% 1:7 & !is.na(data_set$pha_mod) |
           data_set$pha_25z1 == 2) &
        (data_set$pha_29z1 == 1 & data_set$pha_30z1 %in% 1:7 & !is.na(data_set$pha_walk) |
           data_set$pha_29z1 == 2) &
        (data_set$pha_33z1 == 1 & data_set$pha_34z1 %in% 1:7 & !is.na(data_set$pha_vig2) |
           data_set$pha_33z1 == 2) &
        (data_set$pha_37z1 == 1 & data_set$pha_38z1 %in% 1:7 & !is.na(data_set$pha_mod2) |
           data_set$pha_37z1 == 2),
      (data_set$pha_mod_t >= 150 | data_set$pha_vig_t >= 75 | data_set$pha_vig_mod_t >= 150),NA)

    # 일과 관련된 신체활동 시간
    data_set$pha_work <- ifelse(
      !is.na(data_set$pha_vig) & !is.na(data_set$pha_mod),
      data_set$pha_vig*2 + data_set$pha_mod,
      ifelse(!is.na(data_set$pha_vig), data_set$pha_vig*2,
             ifelse(!is.na(data_set$pha_mod), data_set$pha_mod,0)))


    # 여가 관련 신체활동 시간
    data_set$pha_lei <- ifelse(!is.na(data_set$pha_vig2) & !is.na(data_set$pha_mod2),
                               data_set$pha_vig2*2 + data_set$pha_mod2,
                               ifelse(!is.na(data_set$pha_vig2), data_set$pha_vig2*2,
                                      ifelse(!is.na(data_set$pha_mod2), data_set$pha_mod2,0)))

    # 앉아서 보내는 시간

    data_set$phc_sedent <- ifelse(data_set$pha_41z1 %in% 0:24 & data_set$pha_42z1 %in% 0:60,
                                  data_set$pha_41z1 + data_set$pha_42z1/60,NA)

  }






  #### 걷기실천율


  data_set$phb_03z1 <- as.numeric(data_set$phb_03z1)
  data_set$phb_02z1 <- as.numeric(data_set$phb_02z1)

  data_set$ph_b0100 <- NA
  data_set$ph_b0100[is.na(data_set$phb_03z1) & (data_set$phb_02z1 %in% c(0:24))] <-
    data_set$phb_02z1[is.na(data_set$phb_03z1) & (data_set$phb_02z1 %in% c(0:24))]*60
  data_set$ph_b0100[data_set$phb_02z1 %in% c(0:24) & data_set$phb_03z1 %in% c(0:60)] <-
    data_set$phb_02z1[data_set$phb_02z1 %in% c(0:24) & data_set$phb_03z1 %in% c(0:60)]*60 + data_set$phb_03z1[data_set$phb_02z1 %in% c(0:24) & data_set$phb_03z1 %in% c(0:60)]
  data_set$ph_b0100[is.na(data_set$phb_02z1) & (data_set$phb_03z1 %in% c(0:60))] <- data_set$phb_03z1[is.na(data_set$phb_02z1) & (data_set$phb_03z1 %in% c(0:60))]


  data_set$ph_b0200 <- NA
  data_set$ph_b0200[data_set$phb_01z1 %in% c(0:4) | (data_set$phb_01z1 %in% c(5:7) & data_set$ph_b0100 <=29)] <- 0
  data_set$ph_b0200[(data_set$phb_01z1 %in% c(5:7) & data_set$ph_b0100 >= 30)] <- 1

  data_set$ph_a1001 <- NA
  data_set$ph_a1002 <- NA
  data_set$ph_a1003 <- NA
  data_set$ph_a1004 <- NA
  data_set$ph_a1005 <- NA
  data_set$ph_a1006 <- NA
  data_set$ph_a1009 <- NA

  data_set$ph_a1100 <- NA

  data_set$ph_a1201 <- NA
  data_set$ph_a1202 <- NA
  data_set$ph_a1203 <- NA
  data_set$ph_a1204 <- NA
  data_set$ph_a1205 <- NA

  data_set$ph_a1301 <- NA
  data_set$ph_a1302 <- NA
  data_set$ph_a1303 <- NA
  data_set$ph_a1304 <- NA
  data_set$ph_a1305 <- NA

  data_set$ph_c0100 <- NA

  if (year %in% c(2011, 2013, 2015, 2017, 2019)) {
    #### 유연성 운동 실천빈도
    data_set$ph_a1001 <- ifelse(data_set$pha_10z1 == 1, 1,
                                ifelse(data_set$pha_10z1 %in% 2:6, 0, NA))
    data_set$ph_a1002 <- ifelse(data_set$pha_10z1 == 2, 1,
                                ifelse(data_set$pha_10z1 %in% c(1,3:6), 0, NA))
    data_set$ph_a1003 <- ifelse(data_set$pha_10z1 == 3, 1,
                                ifelse(data_set$pha_10z1 %in% c(1,2,4:6), 0, NA))
    data_set$ph_a1004 <- ifelse(data_set$pha_10z1 == 4, 1,
                                ifelse(data_set$pha_10z1 %in% c(1:3,5,6), 0, NA))
    data_set$ph_a1005 <- ifelse(data_set$pha_10z1 == 5, 1,
                                ifelse(data_set$pha_10z1 %in% c(1:4,6), 0, NA))
    data_set$ph_a1006 <- ifelse(data_set$pha_10z1 == 6, 1,
                                ifelse(data_set$pha_10z1 %in% 1:5, 0, NA))

    data_set$ph_a1009 <- ifelse(data_set$pha_10z1 %in% 2:6, 1,
                                ifelse(data_set$pha_10z1 == 1, 0, NA))
   }
    if(year %in% c(2011, 2013, 2015, 2017)) {
    #### 근력운동 실천율
    data_set$ph_a1100 <- ifelse(data_set$pha_11z1 >= 3, 1,
                                ifelse(data_set$pha_11z1 %in% 1:2, 0, NA))


    #### 주중 여가시간에 앉아서 보내는 시간
    data_set$ph_a1201 <- ifelse(data_set$pha_12z1 == 1, 1,
                                ifelse(data_set$pha_12z1 %in% 2:5, 0, NA))
    data_set$ph_a1202 <- ifelse(data_set$pha_12z1 == 2, 1,
                                ifelse(data_set$pha_12z1 %in% c(1, 3:5), 0, NA))
    data_set$ph_a1203 <- ifelse(data_set$pha_12z1 == 3, 1,
                                ifelse(data_set$pha_12z1 %in% c(1:2, 4:5), 0, NA))
    data_set$ph_a1204 <- ifelse(data_set$pha_12z1 == 4, 1,
                                ifelse(data_set$pha_12z1 %in% c(1:3, 5), 0, NA))
    data_set$ph_a1205 <- ifelse(data_set$pha_12z1 == 5, 1,
                                ifelse(data_set$pha_12z1 %in% 1:4, 0, NA))

    #### 주말 여가시간에 앉아서 보내는 시간
    data_set$ph_a1301 <- ifelse(data_set$pha_13z1 == 1, 1,
                                ifelse(data_set$pha_13z1 %in% 2:5, 0, NA))
    data_set$ph_a1302 <- ifelse(data_set$pha_13z1 == 2, 1,
                                ifelse(data_set$pha_13z1 %in% c(1, 3:5), 0, NA))
    data_set$ph_a1303 <- ifelse(data_set$pha_13z1 == 3, 1,
                                ifelse(data_set$pha_13z1 %in% c(1:2, 4:5), 0, NA))
    data_set$ph_a1304 <- ifelse(data_set$pha_13z1 == 4, 1,
                                ifelse(data_set$pha_13z1 %in% c(1:3, 5), 0, NA))
    data_set$ph_a1305 <- ifelse(data_set$pha_13z1 == 5, 1,
                                ifelse(data_set$pha_13z1 %in% 1:4, 0, NA))

  }

  if (year %in% c(2012, 2014, 2016, 2018)) {
    #### 지역내 운동시설 접근
    data_set$ph_c0100 <- ifelse(data_set$phc_01z1 %in% 1:2, 1,
                                ifelse(data_set$phc_01z1 %in% 3:4, 0, NA))
  }

  ### 영양

  #### 저염선호율(type1)


  data_set$nu_b0100 <- NA
  data_set$nu_b0100[data_set$nub_01z1 %in% c(1,2,3)] <- 0
  data_set$nu_b0100[data_set$nub_01z1 %in% c(4,5)] <- 1

  data_set$nu_b0200 <- NA
  data_set$nu_b0200[data_set$nub_02z1 %in% c(1,2,3)] <- 0
  data_set$nu_b0200[data_set$nub_02z1 == 4] <- 1

  data_set$nu_b0300 <- NA
  data_set$nu_b0300[data_set$nub_03z1 %in% c(1,2)] <- 0
  data_set$nu_b0300[data_set$nub_03z1 == 3] <- 1

  data_set$tmp_nutrition <- data_set$nu_b0100 + data_set$nu_b0200 + data_set$nu_b0300

  # 저염선호율(type1)

  data_set$nu_b0400 <- NA
  data_set$nu_b0400[data_set$tmp_nutrition == 1] <- 1
  data_set$nu_b0400[data_set$tmp_nutrition %in% c(0,2,3)] <- 0

  # 저염선호율(type2)

  data_set$nu_b0500 <- NA
  data_set$nu_b0500[data_set$tmp_nutrition == 2] <- 1
  data_set$nu_b0500[data_set$tmp_nutrition %in% c(0,1,3)] <- 0

  # 저염선호율(type3)

  data_set$nu_b0600 <- NA
  data_set$nu_b0600[data_set$tmp_nutrition == 3] <- 1
  data_set$nu_b0600[data_set$tmp_nutrition %in% c(0,1,2)] <- 0



  #분모: josa_year %in% c(2014,2015,2016)
  data_set$nu_c0200 <- NA
  data_set$nu_c0300 <- NA
  data_set$nu_c0400 <- NA
  if (year %in% 2014:2018) {
    # 영양표시 인지율

    data_set$nu_c0200[data_set$nuc_02z1 == 1] <- 1
    data_set$nu_c0200[data_set$nuc_02z1 == 2] <- 0

    # 영양표시 독해율

    data_set$nu_c0300[data_set$nu_c0200 == 1 & data_set$nuc_01z2 == 1] <- 1
    data_set$nu_c0300[data_set$nu_c0200 == 1 & data_set$nuc_01z2 == 2] <- 0
    data_set$nu_c0300[data_set$nu_c0200 == 0] <- 0

    # 영양표시 활용률

    data_set$nu_c0400[data_set$nu_c0200 == 1 & data_set$nu_c0300 == 1 & data_set$nuc_03z1 == 1] <- 1
    data_set$nu_c0400[data_set$nu_c0200 == 1 & data_set$nu_c0300 == 1 & data_set$nuc_03z1 == 2] <- 0

  }
  #### 5일 이상 아침식사 실천율


  data_set$nu_a0204 <- NA
  data_set$nu_a0204[data_set$nua_01z1 %in% c(5,6,7)] <- 1
  data_set$nu_a0204[data_set$nua_01z1 %in% c(0,1,2,3,4)] <- 0





  ### 비만 및 체중조절


  # BMI
  data_set$ob_a0100 <- NA
  normal <- as.numeric(data_set$oba_02z1) >= 50 & as.numeric(data_set$oba_02z1) <= 210 &
    as.numeric(data_set$oba_03z1) >= 20 & as.numeric(data_set$oba_03z1) <= 180
  data_set$ob_a0100[normal] <- as.numeric(data_set$oba_03z1[normal]) / (as.numeric(data_set$oba_02z1[normal])^2*0.0001)

  data_set$ob_a0101 <- NA
  normal_2 <- !is.na(data_set$ob_a0100) & data_set$ob_a0100 >= 10 & data_set$ob_a0100 < 50
  data_set$ob_a0101[normal_2] <- data_set$ob_a0100[normal_2]



  #### 비만율

  # 저체중

  data_set$ob_a0201 <- NA
  data_set$ob_a0201 <- ifelse(data_set$ob_a0101 >= 0 & data_set$ob_a0101 < 10,NA,
                              ifelse(data_set$ob_a0101 >= 10 & data_set$ob_a0101 <18.5, 1,
                                     ifelse(data_set$ob_a0101 >= 18.5 & data_set$ob_a0101 <50, 0,
                                            ifelse(is.na(data_set$ob_a0101), NA, NA))))
  # 정상체중

  data_set$ob_a0202 <- NA
  data_set$ob_a0202 <- ifelse(data_set$ob_a0101 >= 0 & data_set$ob_a0101 < 10,NA,
                              ifelse(data_set$ob_a0101 >= 10 & data_set$ob_a0101 <18.5, 0,
                                     ifelse(data_set$ob_a0101 >= 18.5 & data_set$ob_a0101 <25, 1,
                                            ifelse(data_set$ob_a0101 >= 25 & data_set$ob_a0101 <50, 0,
                                                   ifelse(is.na(data_set$ob_a0101), NA, NA)))))

  # 비만
  data_set$ob_a0203 <- NA
  data_set$ob_a0203 <- ifelse(data_set$ob_a0101 >= 0 & data_set$ob_a0101 < 10,NA,
                              ifelse(data_set$ob_a0101 >= 10 & data_set$ob_a0101 <18.5, 0,
                                     ifelse(data_set$ob_a0101 >= 18.5 & data_set$ob_a0101 <25, 0,
                                            ifelse(data_set$ob_a0101 >= 25 & data_set$ob_a0101 <50, 1,
                                                   ifelse(is.na(data_set$ob_a0101), NA, NA)))))


  #### 주관적 비만인지율

  data_set$ob_a0300 <- NA

  data_set$ob_a0300[data_set$oba_01z1 %in% c(1,2,3)] <- 0
  data_set$ob_a0300[data_set$oba_01z1 %in% c(4,5)] <- 1


  #### 체중조절 시도율
  data_set$ob_b0100 <- NA

  data_set$ob_b0100[data_set$obb_01z1 %in% c(1,2)] <- 0
  data_set$ob_b0100[data_set$obb_01z1 %in% c(3,4)] <- 1

  ### 구강건강

  #### 저작불편호소율
  #분모: age>= 65

  data_set$or_b0100 <- NA
  data_set$or_b0100[data_set$orb_01z1 %in% c(1,2)] <- 1
  data_set$or_b0100[data_set$orb_01z1 %in% c(3,4,5)] <- 0

  #### 구강검진 수진율
  data_set$or_e0500 <- NA
  data_set$or_e0500[data_set$ore_05z1==1] <-1
  data_set$or_e0500[data_set$ore_05z1==2] <-0

  #### 점심식사후 칫솔질 실천율
  #분모: ord_01d2 %in% c(1,2)

  data_set$or_d0050 <- ifelse(data_set$ord_01d2 %in% c(1,2), 1, 0)

  data_set$or_d0100 <- NA
  data_set$or_d0100[data_set$ord_01d2==1] <-1
  data_set$or_d0100[data_set$ord_01d2==2] <-0

  ### 정신건강

  #### 스트레스 인지율

  data_set$mt_a0100 <- NA
  data_set$mt_a0100[data_set$mta_01z1 %in% c(1,2)] <- 1
  data_set$mt_a0100[data_set$mta_01z1 %in% c(3,4)] <- 0



  #### 우울감 경험률

  data_set$mt_b0100 <- NA
  data_set$mt_b0100[data_set$mtb_01z1 == 1] <- 1
  data_set$mt_b0100[data_set$mtb_01z1 == 2] <- 0


  #### 우울증상 유병률

  data_set$tmp_mtb_07a1 <- ifelse(data_set$mtb_07a1 == 1, 0,
                           ifelse(data_set$mtb_07a1 == 2, 1,
                           ifelse(data_set$mtb_07a1 == 3, 2,
                           ifelse(data_set$mtb_07a1 == 4, 3, NA))))

  data_set$tmp_mtb_07b1 <- ifelse(data_set$mtb_07b1 == 1, 0,
                                  ifelse(data_set$mtb_07b1 == 2, 1,
                                         ifelse(data_set$mtb_07b1 == 3, 2,
                                                ifelse(data_set$mtb_07b1 == 4, 3, NA))))

  data_set$tmp_mtb_07c1 <- ifelse(data_set$mtb_07c1 == 1, 0,
                                  ifelse(data_set$mtb_07c1 == 2, 1,
                                         ifelse(data_set$mtb_07c1 == 3, 2,
                                                ifelse(data_set$mtb_07c1 == 4, 3, NA))))

  data_set$tmp_mtb_07d1 <- ifelse(data_set$mtb_07d1 == 1, 0,
                                  ifelse(data_set$mtb_07d1 == 2, 1,
                                         ifelse(data_set$mtb_07d1 == 3, 2,
                                                ifelse(data_set$mtb_07d1 == 4, 3, NA))))

  data_set$tmp_mtb_07e1 <- ifelse(data_set$mtb_07e1 == 1, 0,
                                  ifelse(data_set$mtb_07e1 == 2, 1,
                                         ifelse(data_set$mtb_07e1 == 3, 2,
                                                ifelse(data_set$mtb_07e1 == 4, 3, NA))))

  data_set$tmp_mtb_07f1 <- ifelse(data_set$mtb_07f1 == 1, 0,
                                  ifelse(data_set$mtb_07f1 == 2, 1,
                                         ifelse(data_set$mtb_07f1 == 3, 2,
                                                ifelse(data_set$mtb_07f1 == 4, 3, NA))))

  data_set$tmp_mtb_07g1 <- ifelse(data_set$mtb_07g1 == 1, 0,
                                  ifelse(data_set$mtb_07g1 == 2, 1,
                                         ifelse(data_set$mtb_07g1 == 3, 2,
                                                ifelse(data_set$mtb_07g1 == 4, 3, NA))))

  data_set$tmp_mtb_07h1 <- ifelse(data_set$mtb_07h1 == 1, 0,
                                  ifelse(data_set$mtb_07h1 == 2, 1,
                                         ifelse(data_set$mtb_07h1 == 3, 2,
                                                ifelse(data_set$mtb_07h1 == 4, 3, NA))))

  data_set$tmp_mtb_07i1 <- ifelse(data_set$mtb_07i1 == 1, 0,
                                  ifelse(data_set$mtb_07i1 == 2, 1,
                                         ifelse(data_set$mtb_07i1 == 3, 2,
                                                ifelse(data_set$mtb_07i1 == 4, 3, NA))))

  data_set$tmp_mt_b0500 <- data_set$tmp_mtb07a1 + data_set$tmp_mtb07b1 +
    data_set$tmp_mtb07c1 + data_set$tmp_mtb07d1 + data_set$tmp_mtb07e1 +
    data_set$tmp_mtb07f1 + data_set$tmp_mtb07g1 + data_set$tmp_mtb07h1 + data_set$tmp_mtb07i1

  data_set$mt_b0500 <- ifelse(data_set$tmp_mt_b0500 >= 0 & data_set$tmp_mt_b0500 <= 9, 0,
                        ifelse(data_set$tmp_mt_b0500 >= 10 & data_set$tmp_mt_b0500 <= 27, 1, NA))

  #### 자살생각률

  if (year == 2019) {
    data_set$mt_d0100 <- ifelse(data_set$mtd_01z1 == 1, 1,
                                ifelse(data_set$mtd_01z1 == 2, 0, 1))
  }


  ### 예방접종

  #### 인플루엔자 예방접종률

  data_set$sc_a0100 <- NA
  data_set$sc_a0100[data_set$sca_01z1==1] <- 1
  data_set$sc_a0100[data_set$sca_01z1==2] <- 0



  ### 이환

  #### 혈압인지율


  if(year %in% c(2011, 2013:2019)) {

    data_set$il_a1900 <- ifelse(data_set$hya_19z1 == 1, 1,
                                ifelse(data_set$hya_19z1 == 2, 0, NA))

  }

  if (year == 2012) {
    data_set$il_a1900 <- 9
  }

  #### 혈당인지율
  if(year %in% c(2011, 2013:2019)) {

    data_set$il_b1900 <- ifelse(data_set$dia_19z1 == 1, 1,
                                ifelse(data_set$dia_19z1 == 2, 0, NA))
  }

  if (year == 2012) {
    data_set$il_b1900 <- 9
  }


  #### 고혈압 의사진단 경험률(30세이상)
  #분모: age>= 30

  data_set$il_a0200 <- NA
  data_set$il_a0200[data_set$hya_04z1==1] <- 1
  data_set$il_a0200[data_set$hya_04z1==2] <- 0


  #### 고혈압 약물치료율
  #분모: age>=30 & il_a0200==1
  data_set$il_a0500 <- NA
  data_set$il_a0500[data_set$hya_06z1==1 & data_set$hya_14a1==1 & data_set$hya_14b1>=20 & data_set$hya_14b1<=31] <- 1
  data_set$il_a0500[(data_set$hya_06z1==1 & data_set$hya_14a1==1 & data_set$hya_14b1>=0 & data_set$hya_14b1<=19) |
                      (data_set$hya_06z1==1 & data_set$hya_14a1==2)| data_set$hya_06z1==2] <- 0


  #### 당뇨병 의사진단경험률(30세이상)
  #분모: age>= 30

  data_set$il_b0200 <- NA
  data_set$il_b0200[data_set$dia_04z1==1] <- 1
  data_set$il_b0200[data_set$dia_04z1==2] <- 0


  #### 당뇨병 치료율
  #분모: age>=30 & il_b0200==1

  data_set$il_b0600 <- NA
  data_set$il_b0600[data_set$dia_06z1==1 & (data_set$dia_13a1 ==1|data_set$dia_13b1==1)] <- 1
  data_set$il_b0600[(data_set$dia_06z1==1 & (data_set$dia_13a1 ==2 & data_set$dia_13b1==2)) |
                      data_set$dia_06z1==2] <- 0


  #### 당뇨병 안질환 합병증검사 수진율
  #분모: age >=30 & il_b0200==1

  data_set$il_b0900 <- NA
  data_set$il_b0900[data_set$dia_14z1==1] <- 1
  data_set$il_b0900[data_set$dia_14z1==2] <- 0


  #### 당뇨병 신장질환 합병증검사 수진율
  #분모: age >=30 & il_b0200==1

  data_set$il_b1000 <- NA
  data_set$il_b1000[data_set$dia_15z1==1] <- 1
  data_set$il_b1000[data_set$dia_15z1==2] <- 0


  #### 이상지질혈증 평생 의사진단 경험률
  #분모: age >= 30

  data_set$il_r0100 <- NA
  data_set$il_r0100[data_set$dla_01z1==1] <- 1
  data_set$il_r0100[data_set$dla_01z1==2] <- 0



  #### 관절염 의사진단 경험률(50세이상)
  #age >= 50

  data_set$il_g0100 <- NA
  data_set$il_g0100[data_set$ara_20z1==1] <- 1
  data_set$il_g0100[data_set$ara_20z1==2] <- 0


  ### 의료이용

  #### 필요의료서비스 미치료율
  #12년부터~(11년 제외) josa_year %in% c(2012,2013,2014,2015,2016)
  if ( year == 2011) {
    data_set$sr_a0100 <- 9
  }

  if (year %in% 2012:2018) {
    data_set$sr_a0100 <- NA
    data_set$sr_a0100[data_set$sra_01z1==1] <- 1
    data_set$sr_a0100[data_set$sra_01z1==2] <- 0
  }

  ### 활동제한 및 삶의 질

  #### 양호한 주관적 건강 인지율

  data_set$ql_a0100 <- NA
  data_set$ql_a0100[data_set$qoa_01z1 %in% c(1,2)] <- 1
  data_set$ql_a0100[data_set$qoa_01z1 %in% c(3:5)] <- 0


  #### 삶의 질(EQ-5D)


  ## 운동능력
  data_set$ql_c0100 <- NA
  data_set$ql_c0100[data_set$qoc_01z1 %in% c(1,3)] <- 0
  data_set$ql_c0100[data_set$qoc_01z1 == 2] <- 1

  data_set$ql_c0200 <- NA
  data_set$ql_c0200[data_set$qoc_01z1 %in% c(1,2)] <- 0
  data_set$ql_c0200[data_set$qoc_01z1 == 3] <- 1

  ## 자기관리
  data_set$ql_c0300 <- NA
  data_set$ql_c0300[data_set$qoc_02z1 %in% c(1,3)] <- 0
  data_set$ql_c0300[data_set$qoc_02z1 == 2] <- 1

  data_set$ql_c0400 <- NA
  data_set$ql_c0400[data_set$qoc_02z1 %in% c(1,2)] <- 0
  data_set$ql_c0400[data_set$qoc_02z1 == 3] <- 1

  ## 일상활동
  data_set$ql_c0500 <- NA
  data_set$ql_c0500[data_set$qoc_03z1 %in% c(1,3)] <- 0
  data_set$ql_c0500[data_set$qoc_03z1 == 2] <- 1

  data_set$ql_c0600 <- NA
  data_set$ql_c0600[data_set$qoc_03z1 %in% c(1,2)] <- 0
  data_set$ql_c0600[data_set$qoc_03z1 == 3] <- 1

  ## 통증 불편
  data_set$ql_c0700 <- NA
  data_set$ql_c0700[data_set$qoc_04z1 %in% c(1,3)] <- 0
  data_set$ql_c0700[data_set$qoc_04z1 == 2] <- 1

  data_set$ql_c0800 <- NA
  data_set$ql_c0800[data_set$qoc_04z1 %in% c(1,2)] <- 0
  data_set$ql_c0800[data_set$qoc_04z1 == 3] <- 1

  ## 불안 우울
  data_set$ql_c0900 <- NA
  data_set$ql_c0900[data_set$qoc_05z1 %in% c(1,3)] <- 0
  data_set$ql_c0900[data_set$qoc_05z1 == 2] <- 1

  data_set$ql_c1000 <- NA
  data_set$ql_c1000[data_set$qoc_05z1 %in% c(1,2)] <- 0
  data_set$ql_c1000[data_set$qoc_05z1 == 3] <- 1

  ## EQ-5D
  data_set$ql_c1100 <- NA
  data_set$ql_c1100[data_set$qoc_01z1  %in% c(1:3) & data_set$qoc_02z1 %in% c(1:3) & data_set$qoc_03z1 %in% c(1:3) &
                      data_set$qoc_04z1 %in% c(1:3) & data_set$qoc_05z1 %in% c(1:3)] <- 0
  data_set$ql_c1100[data_set$ql_c0200 == 1 |data_set$ql_c0400 ==1 | data_set$ql_c0600==1| data_set$ql_c0800==1 | data_set$ql_c1000==1]<- 1

  data_set$ql_c1200 <- 1- (0.05 + 0.096*data_set$ql_c0100 + 0.418*data_set$ql_c0200 + 0.046*data_set$ql_c0300 + 0.136*data_set$ql_c0400 +
                             0.051*data_set$ql_c0500 + 0.208*data_set$ql_c0600 + 0.037*data_set$ql_c0700 + 0.151*data_set$ql_c0800 +
                             0.043*data_set$ql_c0900 + 0.158*data_set$ql_c1000 + 0.05*data_set$ql_c1100)
  data_set$ql_c1200[data_set$qoc_01z1==1 & data_set$qoc_02z1==1 & data_set$qoc_03z1==1 & data_set$qoc_04z1==1 & data_set$qoc_05z1==1] <- 1


  #### EQ-VAS

  data_set$ql_c1300 <- NA
  data_set$ql_c1300[data_set$qoc_06z1 %in% c(0:100)] <- as.numeric(data_set$qoc_06z1[data_set$qoc_06z1 %in% c(0:100)])



  ### 보건기관 이용

  #### 보건기관 이용률
  data_set$ct_a0100 <- NA

  if (year %in% 2011:2013) {
    data_set$ct_a0100[data_set$hma_01z1 %in% c(2:4)] <- 1
    data_set$ct_a0100[data_set$hma_01z1 == 1] <- 0

  } else if (year %in% 2014:2017) {
    data_set$ct_a0100[data_set$hma_01z2 == 1] <- 1
    data_set$ct_a0100[data_set$hma_01z2 == 2] <- 0

  }

  data_set$en_a0101 <- NA
  data_set$en_a0102 <- NA
  data_set$en_a0103 <- NA
  data_set$en_a0104 <- NA
  data_set$en_a0105 <- NA
  data_set$en_a0106 <- NA
  data_set$en_a0107 <- NA

  data_set$en_b0101 <- NA
  data_set$en_b0102 <- NA
  data_set$en_b0103 <- NA
  data_set$en_b0104 <- NA
  data_set$en_b0105 <- NA
  data_set$en_b0106 <- NA

  data_set$en_b0201 <- NA
  data_set$en_b0202 <- NA
  data_set$en_b0203 <- NA
  data_set$en_b0204 <- NA
  data_set$en_b0205 <- NA
  data_set$en_b0206 <- NA

  data_set$en_b0301 <- NA
  data_set$en_b0302 <- NA
  data_set$en_b0303 <- NA
  data_set$en_b0304 <- NA
  data_set$en_b0305 <- NA
  data_set$en_b0306 <- NA

  data_set$en_b0301 <- NA
  data_set$en_b0302 <- NA
  data_set$en_b0303 <- NA
  data_set$en_b0304 <- NA
  data_set$en_b0305 <- NA
  data_set$en_b0306 <- NA

  if (year %in% c(2011, 2013, 2015, 2017)) {
    ### 사회물리적 환경

    #### 지역의 사회 물리적 환경에 대한 긍정적 태도율

    data_set$en_a0101 <- ifelse(data_set$ena_01a1 == 1, 1,
                                ifelse(data_set$ena_01a1 == 2, 0, NA))
    data_set$en_a0102 <- ifelse(data_set$ena_01b1 == 1, 1,
                                ifelse(data_set$ena_01b1 == 2, 0, NA))
    data_set$en_a0103 <- ifelse(data_set$ena_01c1 == 1, 1,
                                ifelse(data_set$ena_01c1 == 2, 0, NA))
    data_set$en_a0104 <- ifelse(data_set$ena_01d1 == 1, 1,
                                ifelse(data_set$ena_01d1 == 2, 0, NA))
    data_set$en_a0105 <- ifelse(data_set$ena_01e1 == 1, 1,
                                ifelse(data_set$ena_01e1 == 2, 0, NA))
    data_set$en_a0106 <- ifelse(data_set$ena_01f1 == 1, 1,
                                ifelse(data_set$ena_01f1 == 2, 0, NA))
    data_set$en_a0107 <- ifelse(data_set$ena_01g1 == 1, 1,
                                ifelse(data_set$ena_01g1 == 2, 0, NA))

    #### 사회적 연결망(친척)
    data_set$en_b0101 <- ifelse(data_set$enb_01z1 == 1, 1,
                                ifelse(data_set$enb_01z1 %in% 2:6, 0, NA))
    data_set$en_b0102 <- ifelse(data_set$enb_01z1 == 2, 1,
                                ifelse(data_set$enb_01z1 %in% c(1, 3:6), 0, NA))
    data_set$en_b0103 <- ifelse(data_set$enb_01z1 == 3, 1,
                                ifelse(data_set$enb_01z1 %in% c(1:2, 4:6), 0, NA))
    data_set$en_b0104 <- ifelse(data_set$enb_01z1 == 4, 1,
                                ifelse(data_set$enb_01z1 %in% c(1:3, 5:6), 0, NA))
    data_set$en_b0105 <- ifelse(data_set$enb_01z1 == 5, 1,
                                ifelse(data_set$enb_01z1 %in% c(1:4, 6), 0, NA))
    data_set$en_b0106 <- ifelse(data_set$enb_01z1 == 6, 1,
                                ifelse(data_set$enb_01z1 %in% 1:5, 0, NA))

    #### 사회적 연결망(이웃)
    data_set$en_b0201 <- ifelse(data_set$enb_02z1 == 1, 1,
                                ifelse(data_set$enb_02z1 %in% 2:6, 0, NA))
    data_set$en_b0202 <- ifelse(data_set$enb_02z1 == 2, 1,
                                ifelse(data_set$enb_02z1 %in% c(1, 3:6), 0, NA))
    data_set$en_b0203 <- ifelse(data_set$enb_02z1 == 3, 1,
                                ifelse(data_set$enb_02z1 %in% c(1:2, 4:6), 0, NA))
    data_set$en_b0204 <- ifelse(data_set$enb_02z1 == 4, 1,
                                ifelse(data_set$enb_02z1 %in% c(1:3, 5:6), 0, NA))
    data_set$en_b0205 <- ifelse(data_set$enb_02z1 == 5, 1,
                                ifelse(data_set$enb_02z1 %in% c(1:4, 6), 0, NA))
    data_set$en_b0206 <- ifelse(data_set$enb_02z1 == 6, 1,
                                ifelse(data_set$enb_02z1 %in% 1:5, 0, NA))

    #### 사회적 연결망(친구)
    data_set$en_b0301 <- ifelse(data_set$enb_03z1 == 1, 1,
                                ifelse(data_set$enb_03z1 %in% 2:6, 0, NA))
    data_set$en_b0302 <- ifelse(data_set$enb_03z1 == 2, 1,
                                ifelse(data_set$enb_03z1 %in% c(1, 3:6), 0, NA))
    data_set$en_b0303 <- ifelse(data_set$enb_03z1 == 3, 1,
                                ifelse(data_set$enb_03z1 %in% c(1:2, 4:6), 0, NA))
    data_set$en_b0304 <- ifelse(data_set$enb_03z1 == 4, 1,
                                ifelse(data_set$enb_03z1 %in% c(1:3, 5:6), 0, NA))
    data_set$en_b0305 <- ifelse(data_set$enb_03z1 == 5, 1,
                                ifelse(data_set$enb_03z1 %in% c(1:4, 6), 0, NA))
    data_set$en_b0306 <- ifelse(data_set$enb_03z1 == 6, 1,
                                ifelse(data_set$enb_03z1 %in% 1:5, 0, NA))

    #### 사회적 참여율

    data_set$en_b0401 <- ifelse(data_set$enb_04z1 == 1, 1,
                                ifelse(data_set$enb_04z1 == 2, 0, NA))
    data_set$en_b0402 <- ifelse(data_set$enb_05z1 == 1, 1,
                                ifelse(data_set$enb_05z1 == 2, 0, NA))
    data_set$en_b0403 <- ifelse(data_set$enb_06z1 == 1, 1,
                                ifelse(data_set$enb_06z1 == 2, 0, NA))
    data_set$en_b0404 <- ifelse(data_set$enb_07z1 == 1, 1,
                                ifelse(data_set$enb_07z1 == 2, 0, NA))

  }

  data_set$dong_p <- gsub("[[:punct:]]+", "_", gsub("[[:punct:]]$", "",
                                                    data_set$dong_p))

  return(data_set)
}
