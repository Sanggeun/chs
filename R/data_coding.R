#' coding chs index.
#'
#' data_coding function will code health indicators using raw data of community health survey.
#'
#' @param year the year investigated.
#' @param data_set raw data of community health survey.
#' @return
#' @examples
#' @export
#' @encoding UTF8

data_coding <- function(data_set, year) {

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

  if (year == 2017) {
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
  #data_set$income <- ifelse(data_set$fma_24z1 == 1, "1.50만원미만",
   #                  ifelse(data_set$fma_24z1 == 2, "2.50-100만원미만",
    #                 ifelse(data_set$fma_24z1 == 3, "3.100-200만원미만",
     #                ifelse(data_set$fma_24z1 == 4, "4.200-300만원미만",
      #               ifelse(data_set$fma_24z1 == 5, "5.300-400만원미만",
       #              ifelse(data_set$fma_24z1 == 6, "6.400-500만원미만",
        #             ifelse(data_set$fma_24z1 == 7, "7.500-600만원미만",
         #            ifelse(data_set$fma_24z1 == 8, "8.600만원이상", NA))))))))



  ### 세대
  if (year %in% c(2011,2013)) {
    data_set$fma_19z1 <- data_set$fma_19z2 # family type
  }
  data_set$generation <- ifelse(data_set$fma_19z1 %in% 1:7, "1세대",
                                ifelse(data_set$fma_19z1 %in% 8:16, "2세대",
                                       ifelse(data_set$fma_19z1 %in% 17:19, "3세대",NA)))

  ### 동읍면
  data_set$town <- ifelse(data_set$dong_type == 1, "1.동",
                          ifelse(data_set$dong_type == 2, "2.읍면", NA))

  # 건강행태

  ###  흡연

  #### 현재 흡연율

  data_set$sm_a0100 <- NA
  data_set$sm_a0100[data_set$sma_03z2==8] <- NA
  data_set$sm_a0100[data_set$sma_03z2 %in% c(1,2)] <- 1 # current smoker
  data_set$sm_a0100[data_set$sma_03z2 == 3] <- 0
  data_set$sm_a0100[data_set$sma_01z2 == 2] <- 0

  ### 금연시도율

  data_set$sm_d0600 <- NA
  data_set$sm_d0600[data_set$smd_02z2==1] <- 1
  data_set$sm_d0600[data_set$smd_02z2 %in% c(2,3)] <- 0


  #### 현재 비흡연자의 직장실내간접흡연노출률
 # 분모: 일을 하고 있는 현재 비흡연자(평생 비흡연자, 과거 흡연자) sm_e0200==1

  # 분모
  data_set$sm_e0200 <- NA

  if (year %in% c(2011, 2013)) {
    data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==2] <- 1
    data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==1 & data_set$sma_03z2 %in% c(1,2)] <- 0
    data_set$sm_e0200[data_set$smc_05z1 %in% c(1,2,3) & data_set$sma_01z2==1 & data_set$sma_03z2 == 3] <- 1
  }

  if (year %in% 2014:2017){
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

  if (year %in% 2014:2017) {
    data_set$sm_c0800[data_set$smc_10z1 == 1] <- 1
    data_set$sm_c0800[data_set$smc_10z1 == 2] <- 0
    data_set$sm_c0800[data_set$smc_10z1 == 3] <- NA
  }
  data_set$sm_c0800[data_set$soa_06z1 %in% c(11,12,13)] <- NA

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
  } else if (year %in% c(2014, 2015, 2016, 2017)) {
    data_set$sf_a0300[data_set$sfa_04z3 %in% c(2:5)] <- 1
    data_set$sf_a0300[data_set$sfa_04z3 ==1] <- 0
  }


  # 분자
  data_set$sf_a0400 <- NA
  if (year %in% 2011:2013) {
    data_set$sf_a0400[data_set$sfa_04z2 ==5] <- 1
    data_set$sf_a0400[data_set$sfa_04z2 %in% c(1:4)] <- 0
  } else if (year %in% 2014:2017) {
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
  if (year %in% 2014:2017) {
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




  ### 예방접종

  #### 인플루엔자 예방접종률

  data_set$sc_a0100 <- NA
  data_set$sc_a0100[data_set$sca_01z1==1] <- 1
  data_set$sc_a0100[data_set$sca_01z1==2] <- 0



  ### 이환

  #### 혈압인지율


  if(year %in% c(2011, 2013:2017)) {

  data_set$il_a1900 <- ifelse(data_set$hya_19z1 == 1, 1,
                              ifelse(data_set$hya_19z1 == 2, 0, NA))

  }

  if (year == 2012) {
    data_set$il_a1900 <- 9
  }

  #### 혈당인지율
  if(year %in% c(2011, 2013:2017)) {

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

  if (year %in% 2012:2017) {
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

  data_set$dong_p <- gsub("[[:punct:]]+", "_", gsub("[[:punct:]]$", "",
                                                    data_set$dong_p))



  return(data_set)
}

