#' coding district varible
#'
#' code_district_daegu functions will code district variable.
#'
#' @param data_set community health survey raw data
#' @param var_dong dong variable of chs data
#' @param var_district district variable (new)
#' @return
#' @encoding UTF8
#' @example
#'   data <- code_district_daegu(data, var_dong = "dong_p", var_district = "district")
#' @export

# 대구(2011-2018)
code_district_daegu <- function(data_set, var_dong = "dong_p", var_district = "district") {

  data_set[[var_district]] <- ifelse(data_set[[var_dong]] %in% c("동인동", "삼덕동","성내1동","성내2동","성내3동","대신동", "남산1동","남산2동", "남산3동", "남산4동", "대봉1동", "대봉2동"), "중구",
                                     ifelse(data_set[[var_dong]] %in% c("공산동","불로_봉무동","도평동"),"불로공산권",
                                            ifelse(data_set[[var_dong]] %in% c("안심1동", "안심2동", "안심3_4동"),"안심권",
                                                   ifelse(data_set[[var_dong]] %in% c("지저동", "동촌동", "방촌동",  "해안동"), "동촌권",
                                                          ifelse(data_set[[var_dong]] %in% c("신암1동", "신암2동", "신암3동","신암4동", "신암5동", "신천1_2동", "신천3동", "신천4동",  "효목1동", "효목2동"), "동대구권",
                                                                 ifelse(data_set[[var_dong]] %in% c("내당1동", "내당2_3동", "내당4동"), "내당권",
                                                                        ifelse(data_set[[var_dong]] %in% c("비산1동","비산2_3동","비산4동","비산5동","비산6동","비산7동", "원대동"), "비산권",
                                                                               ifelse(data_set[[var_dong]] %in% c("평리1동","평리2동",  "평리3동", "평리4동","평리5동", "평리6동", "상중이동"), "평리권",
                                                                                      ifelse(data_set[[var_dong]] %in% c("이천동", "봉덕1동", "봉덕2동", "봉덕3동"),"이천봉덕권",
                                                                                             ifelse(data_set[[var_dong]] %in% c("대명1동","대명2동", "대명3동", "대명4동", "대명5동", "대명10동"),"대명권",
                                                                                                    ifelse(data_set[[var_dong]] %in% c("대명6동", "대명9동", "대명11동"),"앞산권",
                                                                                                           ifelse(data_set[[var_dong]] %in% c("고성동", "칠성동", "침산1동","침산2동",  "침산3동","산격1동", "산격2동", "산격3동", "산격4동", "대현동", "복현1동", "복현2동", "검단동", "무태조야동", "노원동"),"강남지역",
                                                                                                                  ifelse(data_set[[var_dong]] %in% c("관문동", "태전1동", "태전2동", "구암동", "관음동", "읍내동", "동천동", "국우동"), "강북지역",
                                                                                                                         ifelse(data_set[[var_dong]] %in% c("범어1동","범어2동","범어3동","범어4동","만촌1동","만촌2동", "만촌3동"), "범어만촌권",
                                                                                                                                ifelse(data_set[[var_dong]] %in% c("수성1가동", "수성2_3가동", "수성4가동", "황금1동", "황금2동", "중동", "상동", "파동", "두산동"), "중동권",
                                                                                                                                       ifelse(data_set[[var_dong]] %in% c("지산1동","지산2동","범물1동", "범물2동"), "지산범물권",
                                                                                                                                              ifelse(data_set[[var_dong]] %in% c("고산1동","고산2동","고산3동"),"고산권",
                                                                                                                                                     ifelse(data_set[[var_dong]] %in% c("성당동", "두류1_2동", "두류3동", "감삼동", "죽전동", "용산1동", "본리동", "본동"),"두류권",
                                                                                                                                                            ifelse(data_set[[var_dong]] %in% c("용산2동","이곡1동","이곡2동","신당동","장기동"), "성서권",
                                                                                                                                                                   ifelse(data_set[[var_dong]] %in% c("월성1동","월성2동","진천동", "상인1동","상인2동", "상인3동", "도원동", "송현1동", "송현2동"), "상인권",
                                                                                                                                                                          ifelse(data_set[[var_dong]] %in% c("다사읍", "하빈면"), "북부권",
                                                                                                                                                                                 ifelse(data_set[[var_dong]] %in% c("화원읍", "옥포면", "가창면", "옥포읍"), "중부권",
                                                                                                                                                                                        ifelse(data_set[[var_dong]] %in% c("논공읍","현풍면", "유가면", "구지면", "유가읍", "현풍읍"),"남부권", NA)))))))))))))))))))))))
  return(data_set)
}

#' coding district varible
#'
#' code_district_ulsan functions will code district variable.
#'
#' @param data_set community health survey raw data
#' @param var_dong dong variable of chs data
#' @param var_district district variable (new)
#' @return
#' @encoding UTF8
#' @example
#'   data <- code_district_ulsan(data, var_dong = "dong_p", var_district = "district")
#' @export
#'
#'
# 울산(2013-2017)
code_district_ulsan <- function(data_set, var_dong = "dong_p", var_district = "district") {

  data_set[[var_district]] <- ifelse(data_set[[var_dong]] %in% c("태화동","다운동"), "태화다운권",
                                     ifelse(data_set[[var_dong]] %in% c("성안동","우정동", "약사동", "복산1동", "복산2동"),"성안복산권",
                                            ifelse(data_set[[var_dong]] %in% c("병영1동","병영2동"), "병영권",
                                                   ifelse(data_set[[var_dong]] %in% c("중앙동", "학성동", "반구1동", "반구2동"), "중앙권",
                                                          ifelse(data_set[[var_dong]] %in% c("방어동", "일산동"), "방어일산권",
                                                                 ifelse(data_set[[var_dong]] %in% c("화정동","대송동"), "화정대송권",
                                                                        ifelse(data_set[[var_dong]] %in% c("전하1동", "전하2동"), "전하권",
                                                                               ifelse(data_set[[var_dong]] %in% c("남목1동","남목2동","남목3동"), "남목권",
                                                                                      ifelse(data_set[[var_dong]] %in% c("신정1동","신정2동", "신정3동","신정4동","신정5동", "달동",
                                                                                                                         "삼산동","옥동","대현동", "수암동", "선암동", "야음장생포동"), "삼산권",
                                                                                             ifelse(data_set[[var_dong]] %in% c("무거동","삼호동"), "무거권",
                                                                                                    ifelse(data_set[[var_dong]] %in% c("농소1동","농소2동","농소3동"),"농소권",
                                                                                                           ifelse(data_set[[var_dong]] %in% c("강동동"), "강동권",
                                                                                                                  ifelse(data_set[[var_dong]] %in% c("염포동","양정동"), "염포양정권",
                                                                                                                         ifelse(data_set[[var_dong]] %in% c("효문동","송정동"), "효문권",
                                                                                                                                ifelse(data_set[[var_dong]] %in% c("범서읍"),"중부권",
                                                                                                                                       ifelse(data_set[[var_dong]] %in% c("언양읍", "두동면","두서면","상북면","삼남면","삼동면"), "서부권",
                                                                                                                                              ifelse(data_set[[var_dong]] %in% c("온산읍","온양읍","서생면","청량면","웅촌면"), "남부권", NA)))))))))))))))))

}
