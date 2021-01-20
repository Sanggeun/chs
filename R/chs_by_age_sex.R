#' stratified result by age and sex.
#'
#' chs_by_age_sex function will calculate stratified rate of health indicators.
#'
#' @param data_set data set.
#' @param by_var strata variable.
#' @param de_var target(dependent) variable.
#' @param cluster cluster.
#' @param strata strata.
#' @param var_name indicator name.
#' @param weight weight.
#' @param digits integer indicating the number of decimal places.#'
#' @return stratifed rates
#' @examples
#' @export
#' @importFrom survey svymean
#' @importFrom survey svydesign
#' @importFrom survey svyby

# 표준인구의 성별, 연령별 특수율
# 2011-2019

chs_by_age_sex <- function(data_set,de_var="ph_a0500",var_name,cluster="JIJUM_CD", by_var = c("sex","age_10"),
                           strata=c("BOGUN_CD","dong_type","house_type"),weight="wt",digits=1) {
  # 지표별 분모지정_하위 데이터셋 만들기  
  if (var_name == "남자현재흡연율") {
    data_set <- data_set[which(data_set$sex ==1), ]
  } else if (var_name == "금연시도율") {
    data_set <- data_set[which(data_set$sm_a0100==1), ]
  } else if (var_name == "현재 비흡연자의 직장실내간접흡연노출률") {
    data_set <- data_set[which(data_set$sm_e0200==1 & data_set$josa_year!=2012), ]
  } else if (var_name == "운전시 안전벨트 착용률") {
    data_set <- data_set[which(data_set$sfa_01z1==1), ]
  } else if (var_name == "동승자 안전벨트 착용률") {
    data_set <- data_set[which(data_set$sf_a0300==1), ]
  } else if (var_name == "음주운전 경험률") {
    data_set <- data_set[which(data_set$sf_b0400==1), ]
  } else if (var_name == "영양표시독해율") {
    data_set <- data_set[which(data_set$josa_year %in% c(2014, 2015, 2016, 2017, 2018, 2019)), ]
  } else if (var_name == "저작불편 호소율") {
    data_set <- data_set[which(data_set$age>=65), ]
  } else if (var_name == "점심식사후 칫솔질 실천율") {
    data_set <- data_set[which(data_set$ord_01d2 %in% c(1,2)), ]
  } else if (var_name == "고혈압 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>= 30), ]
  } else if (var_name == "고혈압 약물치료율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_a0200==1), ]
  } else if (var_name == "당뇨병 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>= 30), ]
  } else if (var_name == "당뇨병 치료율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "당뇨병 안질환 합병증검사 수진율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "당뇨병 신장질환 합병증검사 수진율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "이상지질혈증 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>=30), ]
  } else if (var_name == "관절염 의사진단경험률(50세이상)") {
    data_set <- data_set[which(data_set$age >= 50), ]
  } else if (var_name == "필요의료서비스 미치료율") {
    data_set <- data_set[which(data_set$josa_year %in% c(2012,2013,2014,2015,2016,2017, 2018, 2019)), ]
  } 
  
  
  
  data_tmp <- data_set
  data_tmp$target <- data_tmp[[de_var]]
  chs_design_1 <- svydesign(id=as.formula(paste0("~",cluster)), strata = as.formula(paste0("~",paste(strata, collapse = "+"))), 
                            weights = as.formula(paste0("~",weight)), nest = TRUE ,data=data_tmp)
  by_formula <- as.formula(paste0("~", paste(by_var, collapse = " + ")))
  
  options(survey.lonely.psu = "adjust")
  
  asr<-matrix(nrow=12,ncol=3,byrow=TRUE) 
  a<-svyby(as.formula(paste0("~",de_var)),by_formula ,chs_design_1,svymean,se=T,na.rm=T,deff=T,ci=T,keep.vars=T)
  b<-as.matrix(coef(a))
  
  return(b[,1])
  
}