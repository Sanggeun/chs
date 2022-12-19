#' coding chs index.
#'
#' data_coding_meduse function will code health indicators using raw data of community health survey.
#'
#' @param year the year investigated.
#' @param data_set raw data of community health survey.
#' @return
#' @examples
#' @export
#' @encoding UTF8

### 의료이용

#### 필요의료서비스 미치료율
#12년부터~(11년 제외) josa_year %in% c(2012,2013,2014,2015,2016)
data_coding_meduse <- function(data_set, year) {
  if (year == 2011) {
    data_set$sr_a0100 <- 9
  }
  
  if (year %in% 2012:2018) {
    data_set$sr_a0100 <- NA
    data_set$sr_a0100[data_set$sra_01z1==1] <- 1
    data_set$sr_a0100[data_set$sra_01z1==2] <- 0
  }
  
  if (year %in% 2019:2021) {
    data_set$sr_a0100 <- NA
    data_set$sr_a0100[data_set$sra_01z3 == 1] <- 1
    data_set$sr_a0100[data_set$sra_01z3 %in% 2:3] <- 0
  }
  
  #### 미충족의료 이유 ###
  
  if (year %in% 2012:2018) {
    data_set$sr_a0200 <- NA
    data_set$sr_a0200[data_set$sra_02z1 == 1] <- 1 # 경제적 이유
    data_set$sr_a0200[data_set$sra_02z1 == 2] <- 2 # 예약이 어려움
    data_set$sr_a0200[data_set$sra_02z1 == 3] <- 3 # 교통이 불편
    data_set$sr_a0200[data_set$sra_02z1 == 4] <- 4 # 시간이 없어서 (원하는 시간에 병원이 문을 열지 않음)
    data_set$sr_a0200[data_set$sra_02z1 == 5] <- 5 # 대기시간
    data_set$sr_a0200[data_set$sra_02z1 == 6] <- 6 # 증상이 가벼움
    data_set$sr_a0200[data_set$sra_02z1 == 7] <- 7 # 기타
    
    data_set$sr_a0300 <- ifelse(data_set$sra_02z1 %in% c(2,5), 1,
                                ifelse(data_set$sra_02z1 %in% c(1,3), 2,
                                       ifelse(data_set$sra_02z1 %in% c(4,6,7), 3, NA)))
  }
  
  if (year %in% 2019:2020) {
    data_set$sr_a0200 <- NA
    
    data_set$sr_a0200[data_set$sra_02z2 == 1] <- 1
    data_set$sr_a0200[data_set$sra_02z2 == 2] <- 2
    data_set$sr_a0200[data_set$sra_02z2 == 3] <- 3
    data_set$sr_a0200[data_set$sra_02z2 == 4] <- 4
    data_set$sr_a0200[data_set$sra_02z2 == 5] <- 5
    data_set$sr_a0200[data_set$sra_02z2 == 6] <- 6
    data_set$sr_a0200[data_set$sra_02z2 == 7] <- 7
    data_set$sr_a0200[data_set$sra_02z2 == 8] <- 8
    
    data_set$sr_a0300 <- ifelse(data_set$sra_02z2 %in% c(5,6), 1,
                                ifelse(data_set$sra_02z2 %in% c(3,4), 2,
                                       ifelse(data_set$sra_02z2 %in% c(1,2,7,8), 3, NA)))
  }
  
  if (year == 2021) {
    data_set$sr_a0200 <- NA
    data_set$sr_a0200[data_set$sra_02z3 == 1] <- 1 # 시간이 없어서
    data_set$sr_a0200[data_set$sra_02z3 == 2] <- 2 # 증상이 가벼움
    data_set$sr_a0200[data_set$sra_02z3 == 3] <- 3 # 경제적 이유
    data_set$sr_a0200[data_set$sra_02z3 == 4] <- 4 # 교통이 불편, 거리가 멀어서
    data_set$sr_a0200[data_set$sra_02z3 == 5] <- 5 # 대기시간
    data_set$sr_a0200[data_set$sra_02z3 == 6] <- 6 # 예약이 어려움
    data_set$sr_a0200[data_set$sra_02z3 == 7] <- 7 # 진료가 무서움
    data_set$sr_a0200[data_set$sra_02z3 == 8] <- 8 # 코로나 유행 때문에
    data_set$sr_a0200[data_set$sra_02z3 == 9] <- 9 # 기타
    
    data_set$sr_a0300 <- ifelse(data_set$sra_02z3 %in% c(5,6,8), 1,
                                ifelse(data_set$sra_02z3 %in% c(3,4), 2,
                                       ifelse(data_set$sra_02z3 %in% c(1,2,7,9), 3, NA)))
  }
  
  return(data_set)
}