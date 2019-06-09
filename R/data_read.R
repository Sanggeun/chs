#' Reading chs data.
#'
#' dataread_chs_raw function will read raw data of community health survey.
#'
#' @param year the year investigated.
#' @param filename Path to the xls/xlsx file.
#' @param data3 TRUE to use third data sheet, FALSE to use only two data sheet.
#' @return
#' @examples
#' @export

dataread_chs_raw <- function(year, filename, data3 = FALSE) {
    data_d1 <- readxl::read_excel(filename, sheet = "data1"); data_d1 <- data_d1[-1,]
    data_d2 <- readxl::read_excel(filename, sheet = "data2"); data_d2 <- data_d2[-1,]

    # 13년부터는 by='id' 제외
    data_d2$id <- data_d1$id
    data_all <- merge(data_d1, data_d2, by='id')


    if(data3) {
    data_d3 <- readxl::read_excel(filename, sheet = "data3"); data_d3 <- data_d3[-1,]
    data_d3$id <- data_d1$id
    data_all <- merge(data_all,data_d3, by='id')
    }

    write.csv(data_all,"a.csv")
    data_all <- read.csv("a.csv", stringsAsFactors = FALSE)

    return(data_all)
}

# data11 <- dataread_chs_raw(year = "2011", filename = "data/2011_대구광역시_중구보건소.xlsx")
# data12 <- dataread_chs_raw(year = "2012", filename = "data/2012_중구보건소.xlsx")
# data13 <- dataread_chs_raw(year = "2013", filename = "data/2013_중구보건소.xlsx")
# data14 <- dataread_chs_raw(year = "2014", filename = "data/2014_중구보건소.xlsx", data3=TRUE)











