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
  data$job <- ifelse(data$soa_06z1 %in% c("1","2"),"1.전문행정관리",
                     ifelse(data$soa_06z1 == "3", "2.사무직",
                            ifelse(data$soa_06z1 %in% c("4","5"), "3.판매서비스직",
                                   ifelse(data$soa_06z1 == "6", "4.농림어업",
                                          ifelse(data$soa_06z1 %in% c("7","8","9"), "5.기능단순노무직",
                                                 ifelse(data$soa_06z1 %in% c("10","11","12","13"),"6.기타",NA))))))
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
  ## 흡연

  ###1. 현재 흡연율

  data$sm_a0100 <- NA
  data$sm_a0100[data$sma_03z2==8] <- NA
  data$sm_a0100[data$sma_03z2 %in% c(1,2)] <- 1 # current smoker
  data$sm_a0100[data$sma_03z2 == 3] <- 0
  data$sm_a0100[data$sma_01z2 == 2] <- 0


  ###2. 평생 흡연율

  if (year %in% 2011:2018) {
    data$sm_a0200 <- ifelse(data$sma_01z2 == 1, 1,
                            ifelse(data$sma_01z2 == 2, 0, NA))

    ### 3. 흡연시작연령
    data$sm_a0300 <- ifelse(data$sma_01z2 == 1 &
                              (data$sma_02z1>=0 & data$sma_02z1<=110) &
                              (data$sma_02z1>=0 & data$sma_02z1<=data$age),
                            data$sma_02z1, NA)


    ### 4. 매일 흡연자의 하루 평균 흡연량

    #### 분모정의(매일 흡연자)
    data$sm_a0400 <- ifelse(data$sma_01z2 == 1,
                            ifelse(data$sma_03z2 == 1, 1,
                                   ifelse(data$sma_03z2 %in% c(2,3),0,NA)),
                            ifelse(data$sma_01z2 == 2, 0, NA))

  }
  #### 분자정의(흡연량)
  data$sm_b0100 <- ifelse(data$smb_01z1 >= 1 & data$smb_01z1 <= 776,
                          data$smb_01z1, NA)

  ### 4-1. 매일 흡연자의 하루 평균 흡연량_10개비 미만
  data$sm_b0201 <- ifelse(data$smb_01z1 >= 1 & data$smb_01z1 <= 9, 1,
                          ifelse(data$smb_01z1 >= 10 & data$smb_01z1 <= 776, 0, NA))

  ### 4-2. 매일흡연자의 하루평균흡연량_10~19개비
  data$sm_b0202 <- ifelse(data$smb_01z1 >= 1 & data$smb_01z1 <= 9, 0,
                          ifelse(data$smb_01z1 >= 10 & data$smb_01z1 <= 19, 1,
                                 ifelse(data$smb_01z1 >= 20 & data$smb_01z1 <= 776, 0, NA)))

  ### 4-3. 매일흡연자의 하루평균흡연량_20~39개비
  data$sm_b0203 <- ifelse(data$smb_01z1 >= 1 & data$smb_01z1 <= 19, 0,
                          ifelse(data$smb_01z1 >= 20 & data$smb_01z1 <= 39, 1,
                                 ifelse(data$smb_01z1 >= 40 & data$smb_01z1 <= 776, 0, NA)))

  ### 4-4. 매일흡연자의 하루평균흡연량_40개비 이상
  data$sm_b0204 <- ifelse(data$smb_01z1 >= 1 & data$smb_01z1 <= 39, 0,
                          ifelse(data$smb_01z1 >= 40 & data$smb_01z1 <= 776, 1, NA))

  ### 5. 금연시도율

  data$sm_d0600 <- NA
  data$sm_d0600[data$smd_02z2==1] <- 1
  data$sm_d0600[data$smd_02z2 %in% c(2,3)] <- 0

  ### 6. 현재흡연자의 1개월 내 금연계획률
  data$sm_d0500 <- ifelse(data$smd_01z2 == 1, 1,
                          ifelse(data$smd_01z2 %in% 2:4, 0, NA))



  ### 음주

  #### 월간음주율

  data$dr_a0400 <- NA
  data$dr_a0400[data$dra_01z1 ==2 |data$drb_02z1==2 | data$drb_01z2 ==1] <- 0
  data$dr_a0400[data$dra_01z1==1 & data$drb_02z1==1 & data$drb_01z2 %in% c(2:5)] <- 1

  #### 고위험 음주율

  #성별에 따라 기준이 다름

  data$dr_a0500 <- NA
  data$dr_a0500[data$drb_01z2 %in% c(1:3)] <- 0
  data$dr_a0500[data$sex=="1" & data$drb_01z2 %in% c(4:5) & data$drb_03z1 %in% c(1:3)] <- 0
  data$dr_a0500[data$sex=="1" & data$drb_01z2 %in% c(4:5) & data$drb_03z1 %in% c(4:5)] <- 1
  data$dr_a0500[data$sex=="2" & data$drb_01z2 %in% c(4:5) & data$drb_03z1 %in% c(1:2)] <- 0
  data$dr_a0500[data$sex=="2" & data$drb_01z2 %in% c(4:5) & data$drb_03z1 %in% c(3:5)] <- 1


  ### 운동 및 신체활동

  data$ph_a0500 <- NA
  if (year %in% c(2011:2017, 2019)) {

    #### 중등도이상 신체활동 실천율
    # 격렬한 신체활동
    data$pha_05z1 <- as.numeric(data$pha_05z1)
    data$pha_06z1 <- as.numeric(data$pha_06z1)

    data$ph_a0100 <- NA
    data$ph_a0100[is.na(data$pha_06z1) & data$pha_05z1 %in% c(0:24)] <-
      data$pha_05z1[is.na(data$pha_06z1) & data$pha_05z1 %in% c(0:24)]*60
    data$ph_a0100[data$pha_05z1 %in% c(0:24) & data$pha_06z1 %in% c(0:60)] <-
      data$pha_05z1[data$pha_05z1 %in% c(0:24) & data$pha_06z1 %in% c(0:60)]*60 + data$pha_06z1[data$pha_05z1 %in% c(0:24) & data$pha_06z1 %in% c(0:60)]
    data$ph_a0100[is.na(data$pha_05z1) & (data$pha_06z1 %in% c(0:60))] <- data$pha_06z1[is.na(data$pha_05z1) & (data$pha_06z1 %in% c(0:60))]

    data$ph_a0200 <- NA
    data$ph_a0200[data$pha_04z1 %in% c(0:2) | (data$pha_04z1 %in% c(3:7) & data$ph_a0100 <=19)] <- 0
    data$ph_a0200[(data$pha_04z1 %in% c(3:7) & data$ph_a0100 >= 20)] <- 1

    # 중등도 신체활동
    data$pha_08z1 <- as.numeric(data$pha_08z1)
    data$pha_09z1 <- as.numeric(data$pha_09z1)

    data$ph_a0300 <- NA
    data$ph_a0300[is.na(data$pha_09z1) & (data$pha_08z1 %in% c(0:24))] <-
      data$pha_08z1[is.na(data$pha_09z1) & (data$pha_08z1 %in% c(0:24))]*60
    data$ph_a0300[data$pha_08z1 %in% c(0:24) & data$pha_09z1 %in% c(0:60)] <-
      data$pha_08z1[data$pha_08z1 %in% c(0:24) & data$pha_09z1 %in% c(0:60)]*60 + data$pha_09z1[data$pha_08z1 %in% c(0:24) & data$pha_09z1 %in% c(0:60)]
    data$ph_a0300[is.na(data$pha_08z1) & (data$pha_09z1 %in% c(0:60))] <- data$pha_09z1[is.na(data$pha_08z1) & (data$pha_09z1 %in% c(0:60))]

    data$ph_a0400 <- NA
    data$ph_a0400[data$pha_07z1 %in% c(0:4) | (data$pha_04z1 %in% c(5:7) & data$ph_a0300 <=29)] <- 0
    data$ph_a0400[(data$pha_07z1 %in% c(5:7) & data$ph_a0300 >= 30)] <- 1

    # 중등도이상 신체활동 실천율

    data$ph_a0500 <- NA
    data$ph_a0500[data$ph_a0200==1 | data$ph_a0400==1] <- 1
    data$ph_a0500[data$ph_a0200==0 & data$ph_a0400==0] <- 0

  }

  #### 걷기실천율


  data$phb_03z1 <- as.numeric(data$phb_03z1)
  data$phb_02z1 <- as.numeric(data$phb_02z1)

  data$ph_b0100 <- NA
  data$ph_b0100[is.na(data$phb_03z1) & (data$phb_02z1 %in% c(0:24))] <-
    data$phb_02z1[is.na(data$phb_03z1) & (data$phb_02z1 %in% c(0:24))]*60
  data$ph_b0100[data$phb_02z1 %in% c(0:24) & data$phb_03z1 %in% c(0:60)] <-
    data$phb_02z1[data$phb_02z1 %in% c(0:24) & data$phb_03z1 %in% c(0:60)]*60 + data$phb_03z1[data$phb_02z1 %in% c(0:24) & data$phb_03z1 %in% c(0:60)]
  data$ph_b0100[is.na(data$phb_02z1) & (data$phb_03z1 %in% c(0:60))] <- data$phb_03z1[is.na(data$phb_02z1) & (data$phb_03z1 %in% c(0:60))]


  data$ph_b0200 <- NA
  data$ph_b0200[data$phb_01z1 %in% c(0:4) | (data$phb_01z1 %in% c(5:7) & data$ph_b0100 <=29)] <- 0
  data$ph_b0200[(data$phb_01z1 %in% c(5:7) & data$ph_b0100 >= 30)] <- 1

  #### 비만율

  # 저체중

  data$ob_a0201 <- NA
  data$ob_a0201 <- ifelse(data$ob_a0101 >= 0 & data$ob_a0101 < 10,NA,
                          ifelse(data$ob_a0101 >= 10 & data$ob_a0101 <18.5, 1,
                                 ifelse(data$ob_a0101 >= 18.5 & data$ob_a0101 <50, 0,
                                        ifelse(is.na(data$ob_a0101), NA, NA))))
  # 정상체중

  data$ob_a0202 <- NA
  data$ob_a0202 <- ifelse(data$ob_a0101 >= 0 & data$ob_a0101 < 10,NA,
                          ifelse(data$ob_a0101 >= 10 & data$ob_a0101 <18.5, 0,
                                 ifelse(data$ob_a0101 >= 18.5 & data$ob_a0101 <25, 1,
                                        ifelse(data$ob_a0101 >= 25 & data$ob_a0101 <50, 0,
                                               ifelse(is.na(data$ob_a0101), NA, NA)))))

  # 비만
  data$ob_a0203 <- NA
  data$ob_a0203 <- ifelse(data$ob_a0101 >= 0 & data$ob_a0101 < 10,NA,
                          ifelse(data$ob_a0101 >= 10 & data$ob_a0101 <18.5, 0,
                                 ifelse(data$ob_a0101 >= 18.5 & data$ob_a0101 <25, 0,
                                        ifelse(data$ob_a0101 >= 25 & data$ob_a0101 <50, 1,
                                               ifelse(is.na(data$ob_a0101), NA, NA)))))


  #### 주관적 비만인지율

  data$ob_a0300 <- NA

  data$ob_a0300[data$oba_01z1 %in% c(1,2,3)] <- 0
  data$ob_a0300[data$oba_01z1 %in% c(4,5)] <- 1


  #### 체중조절 시도율
  data$ob_b0100 <- NA

  data$ob_b0100[data$obb_01z1 %in% c(1,2)] <- 0
  data$ob_b0100[data$obb_01z1 %in% c(3,4)] <- 1

  ### 구강건강

  #### 저작불편호소율
  #분모: age>= 65

  data$or_b0100 <- NA
  data$or_b0100[data$orb_01z1 %in% c(1,2)] <- 1
  data$or_b0100[data$orb_01z1 %in% c(3,4,5)] <- 0

  #### 구강검진 수진율
  data$or_e0500 <- NA
  data$or_e0500[data$ore_05z1==1] <-1
  data$or_e0500[data$ore_05z1==2] <-0

  #### 점심식사후 칫솔질 실천율
  #분모: ord_01d2 %in% c(1,2)

  data$or_d0050 <- ifelse(data$ord_01d2 %in% c(1,2), 1, 0)

  data$or_d0100 <- NA
  data$or_d0100[data$ord_01d2==1] <-1
  data$or_d0100[data$ord_01d2==2] <-0

  ### 정신건강

  #### 스트레스 인지율

  data$mt_a0100 <- NA
  data$mt_a0100[data$mta_01z1 %in% c(1,2)] <- 1
  data$mt_a0100[data$mta_01z1 %in% c(3,4)] <- 0



  #### 우울감 경험률

  data$mt_b0100 <- NA
  data$mt_b0100[data$mtb_01z1 == 1] <- 1
  data$mt_b0100[data$mtb_01z1 == 2] <- 0


  #### 인플루엔자 예방접종률

  data$sc_a0100 <- NA
  data$sc_a0100[data$sca_01z1==1] <- 1
  data$sc_a0100[data$sca_01z1==2] <- 0


  ### 이환

  #### 혈압인지율


  if(year %in% c(2011, 2013:2019)) {

    data$il_a1900 <- ifelse(data$hya_19z1 == 1, 1,
                            ifelse(data$hya_19z1 == 2, 0, NA))

  }

  if (year == 2012) {
    data$il_a1900 <- 9
  }

  #### 혈당인지율
  if(year %in% c(2011, 2013:2019)) {

    data$il_b1900 <- ifelse(data$dia_19z1 == 1, 1,
                            ifelse(data$dia_19z1 == 2, 0, NA))
  }

  if (year == 2012) {
    data$il_b1900 <- 9
  }


  #### 고혈압 의사진단 경험률(30세이상)
  #분모: age>= 30

  data$il_a0200 <- NA
  data$il_a0200[data$hya_04z1==1] <- 1
  data$il_a0200[data$hya_04z1==2] <- 0


  #### 고혈압 약물치료율
  #분모: age>=30 & il_a0200==1
  data$il_a0500 <- NA
  data$il_a0500[data$hya_06z1==1 & data$hya_14a1==1 & data$hya_14b1>=20 & data$hya_14b1<=31] <- 1
  data$il_a0500[(data$hya_06z1==1 & data$hya_14a1==1 & data$hya_14b1>=0 & data$hya_14b1<=19) |
                  (data$hya_06z1==1 & data$hya_14a1==2)| data$hya_06z1==2] <- 0


  ### 고혈압 관리교육 장소(병의원/보건소)
  data$il_a0701 <- NA
  data$il_a0702 <- NA
  data$il_a0703 <- NA

  if(year %in% 2011:2017) {
    data$il_a0701 <- ifelse(data$il_a0200 == 1 & data$hya_11a1 == 1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11a1 == 2, 0, NA))
    data$il_a0702 <- ifelse(data$il_a0200 == 1 & data$hya_11b1 ==1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11b1 == 2, 0, NA))

    data$il_a0703 <- ifelse(data$il_a0200 == 1 & data$hya_11c1 ==1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11c1 == 2, 0, NA))

  }

  if (year == 2018) {
    data$il_a0701 <- ifelse(data$il_a0200 == 1 & data$hya_11a1 == 1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11a1 == 2, 0, NA))

    data$il_a0703 <- ifelse(data$il_a0200 == 1 & data$hya_11c1 ==1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11c1 == 2, 0, NA))
  }

  if (year == 2019) {
    data$il_a0701 <- ifelse(data$il_a0200 == 1 & data$hya_11a2 == 1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11a2 == 2, 0, NA))

    data$il_a0703 <- ifelse(data$il_a0200 == 1 & data$hya_11c2 ==1, 1,
                            ifelse(data$il_a0200 == 1 & data$hya_11c2 == 2, 0, NA))
  }


  ### 고혈압 관리교육 이수율
  if (year %in% 2011:2017) {
    data$il_a0800 <- ifelse(data$il_a0701 == 1 |
                              data$il_a0702 == 1 |
                              data$il_a0703 == 1, 1,
                            ifelse(data$il_a0701 == 0 &
                                     data$il_a0702 == 0 &
                                     data$il_a0703 == 0, 0, NA))

  }
  if (year ==2018) {
    data$il_a0800 <- ifelse(data$il_a0701 == 1 | data$il_a0703 == 1, 1,
                            ifelse(data$il_a0701 == 0 & data$il_a0703 == 0, 0, NA))
  }

  if (year == 2019) {
    data$il_a0800 <- NA
    data$il_a0800[data$il_a0200 == 1] <- 0
    data$il_a0800[data$il_a0701 == 1 | data$il_a0703 == 1] <- 1

  }

  #### 당뇨병 의사진단경험률(30세이상)
  #분모: age>= 30

  data$il_b0200 <- NA
  data$il_b0200[data$dia_04z1==1] <- 1
  data$il_b0200[data$dia_04z1==2] <- 0


  #### 당뇨병 치료율
  #분모: age>=30 & il_b0200==1

  data$il_b0600 <- NA
  data$il_b0600[data$dia_06z1==1 & (data$dia_13a1 ==1|data$dia_13b1==1)] <- 1
  data$il_b0600[(data$dia_06z1==1 & (data$dia_13a1 ==2 & data$dia_13b1==2)) |
                  data$dia_06z1==2] <- 0


  #### 당뇨병 안질환 합병증검사 수진율
  #분모: age >=30 & il_b0200==1

  data$il_b0900 <- NA
  data$il_b0900[data$dia_14z1==1] <- 1
  data$il_b0900[data$dia_14z1==2] <- 0


  #### 당뇨병 신장질환 합병증검사 수진율
  #분모: age >=30 & il_b0200==1

  data$il_b1000 <- NA
  data$il_b1000[data$dia_15z1==1] <- 1
  data$il_b1000[data$dia_15z1==2] <- 0


  #### 이상지질혈증 평생 의사진단 경험률
  #분모: age >= 30

  data$il_r0100 <- NA
  data$il_r0100[data$dla_01z1==1] <- 1
  data$il_r0100[data$dla_01z1==2] <- 0



  #### 관절염 의사진단 경험률(50세이상)
  #age >= 50

  data$il_g0100 <- NA
  data$il_g0100[data$ara_20z1==1] <- 1
  data$il_g0100[data$ara_20z1==2] <- 0


  ### 의료이용

  #### 필요의료서비스 미치료율
  #12년부터~(11년 제외) josa_year %in% c(2012,2013,2014,2015,2016)
  if ( year == 2011) {
    data$sr_a0100 <- 9
  }

  if (year %in% 2012:2018) {
    data$sr_a0100 <- NA
    data$sr_a0100[data$sra_01z1==1] <- 1
    data$sr_a0100[data$sra_01z1==2] <- 0
  }

  ### 활동제한 및 삶의 질

  #### 양호한 주관적 건강 인지율

  data$ql_a0100 <- NA
  data$ql_a0100[data$qoa_01z1 %in% c(1,2)] <- 1
  data$ql_a0100[data$qoa_01z1 %in% c(3:5)] <- 0


  #### 삶의 질(EQ-5D)


  ## 운동능력
  data$ql_c0100 <- NA
  data$ql_c0100[data$qoc_01z1 %in% c(1,3)] <- 0
  data$ql_c0100[data$qoc_01z1 == 2] <- 1

  data$ql_c0200 <- NA
  data$ql_c0200[data$qoc_01z1 %in% c(1,2)] <- 0
  data$ql_c0200[data$qoc_01z1 == 3] <- 1

  ## 자기관리
  data$ql_c0300 <- NA
  data$ql_c0300[data$qoc_02z1 %in% c(1,3)] <- 0
  data$ql_c0300[data$qoc_02z1 == 2] <- 1

  data$ql_c0400 <- NA
  data$ql_c0400[data$qoc_02z1 %in% c(1,2)] <- 0
  data$ql_c0400[data$qoc_02z1 == 3] <- 1

  ## 일상활동
  data$ql_c0500 <- NA
  data$ql_c0500[data$qoc_03z1 %in% c(1,3)] <- 0
  data$ql_c0500[data$qoc_03z1 == 2] <- 1

  data$ql_c0600 <- NA
  data$ql_c0600[data$qoc_03z1 %in% c(1,2)] <- 0
  data$ql_c0600[data$qoc_03z1 == 3] <- 1

  ## 통증 불편
  data$ql_c0700 <- NA
  data$ql_c0700[data$qoc_04z1 %in% c(1,3)] <- 0
  data$ql_c0700[data$qoc_04z1 == 2] <- 1

  data$ql_c0800 <- NA
  data$ql_c0800[data$qoc_04z1 %in% c(1,2)] <- 0
  data$ql_c0800[data$qoc_04z1 == 3] <- 1

  ## 불안 우울
  data$ql_c0900 <- NA
  data$ql_c0900[data$qoc_05z1 %in% c(1,3)] <- 0
  data$ql_c0900[data$qoc_05z1 == 2] <- 1

  data$ql_c1000 <- NA
  data$ql_c1000[data$qoc_05z1 %in% c(1,2)] <- 0
  data$ql_c1000[data$qoc_05z1 == 3] <- 1

  ## EQ-5D
  data$ql_c1100 <- NA
  data$ql_c1100[data$qoc_01z1  %in% c(1:3) & data$qoc_02z1 %in% c(1:3) & data$qoc_03z1 %in% c(1:3) &
                  data$qoc_04z1 %in% c(1:3) & data$qoc_05z1 %in% c(1:3)] <- 0
  data$ql_c1100[data$ql_c0200 == 1 |data$ql_c0400 ==1 | data$ql_c0600==1| data$ql_c0800==1 | data$ql_c1000==1]<- 1

  data$ql_c1200 <- 1- (0.05 + 0.096*data$ql_c0100 + 0.418*data$ql_c0200 + 0.046*data$ql_c0300 + 0.136*data$ql_c0400 +
                         0.051*data$ql_c0500 + 0.208*data$ql_c0600 + 0.037*data$ql_c0700 + 0.151*data$ql_c0800 +
                         0.043*data$ql_c0900 + 0.158*data$ql_c1000 + 0.05*data$ql_c1100)
  data$ql_c1200[data$qoc_01z1==1 & data$qoc_02z1==1 & data$qoc_03z1==1 & data$qoc_04z1==1 & data$qoc_05z1==1] <- 1


  #### EQ-VAS

  data$ql_c1300 <- NA
  data$ql_c1300[data$qoc_06z1 %in% c(0:100)] <- as.numeric(data$qoc_06z1[data$qoc_06z1 %in% c(0:100)])


  ### 보건기관 이용

  #### 보건기관 이용률
  data$ct_a0100 <- NA

  if (year %in% 2011:2013) {
    data$ct_a0100[data$hma_01z1 %in% c(2:4)] <- 1
    data$ct_a0100[data$hma_01z1 == 1] <- 0

  } else if (year %in% 2014:2017) {
    data$ct_a0100[data$hma_01z2 == 1] <- 1
    data$ct_a0100[data$hma_01z2 == 2] <- 0

  }


  return(data)


}
