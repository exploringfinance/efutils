#' Is NA, NAN, or Infinite
#'
#' @param x value to be checked
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_is.nainf(NA)
#' ef_is.nainf(1)
#'
#' }
ef_is.nainf = function(x){
  x = (is.na(x) | is.nan(x) | !is.finite(x))
  return(x)
}


#' If NA, NAN, or Infinate repalce with value
#'
#' Uses ifelse to replace NA only if value is NA
#'
#' @param x value to be checked
#' @param y value to replace if x is NA, NAN, or Infinite
#'
#' @return a data frame of cots symbols with descriptions
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ef_if.na(NA,3)
#' ef_if.na(4,5)
#'
#' }
ef_if.na <-  function(x,y){
    ifelse(ef_is.nainf(x),y,x)
  }



#' Check Vector Overlap
#'
#' Will return overlapping or non overlapping variables
#'
#' @param vec1 first vector
#' @param vec2 second vector
#' @param in_out Out will return non matching and In will return matching
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_overlap(c(1:10),c(7:15),'Out')
#' ef_overlap(c(1:10),c(7:15),'In')
#'
#' }
ef_overlap <- function(vec1, vec2, in_out='Out') {
  matched <- intersect(vec1, vec2)
  all <-  union(vec1, vec2)
  non.matched <- all[!all %in% matched]
  return(if(in_out=='In'){matched} else {non.matched})
}



#' Set Options
#'
#' Set non scientific notation and show all columns in Tibble
#'
#' @return None
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ef_set_opt()
#'
#' }
#'
ef_set_opt <- function(){
  options(scipen=999) ##Non scientific Notation
  options(dplyr.width = Inf) ##Show all columns in Tibble
}



#' Add comma to number and round to digits
#'
#' @param x any numerical value
#' @param dig round digits
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_addcomma(100000.12244)
#'
#' }
ef_addcomma <- function(x, dig = 0){

  format(round(x, dig), nsmall = dig, big.mark=",")

  }


#' Convert number to text and percentage
#'
#' @param x number to convert to percentage
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_conv_perc(.1234)
#'
#' }
ef_conv_perc <- function(x){paste0(round(x,4)*100,'%')}


#' Create a datemap with columns
#'
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_create_datemap()
#'
#' }
ef_create_datemap <- function(){

  ### Create data frame with map
  datemap <- dplyr::tibble(dayend = seq(as.Date("1980/1/1"), Sys.Date()+lubridate::years(10), by = "day"))
  datemap$weekend <- lubridate::ceiling_date(as.Date(datemap$dayend),"week")-lubridate::days(1)
  datemap$monthend <- lubridate::ceiling_date(as.Date(datemap$dayend),"month")-lubridate::days(1)
  datemap$monthstart <- lubridate::floor_date(as.Date(datemap$dayend),"month")
  datemap$qtrend <- lubridate::ceiling_date(as.Date(datemap$dayend),"quarter")-lubridate::days(1)
  datemap$yearend <- lubridate::ceiling_date(as.Date(datemap$dayend),"year")-lubridate::days(1)
  datemap$dayofweek <- weekdays(datemap$dayend)


  strt = as.numeric(lubridate::as_datetime(as.Date('1980-01-01'), tz='America/New_York'))
  end = as.numeric(lubridate::as_datetime(Sys.Date()+lubridate::days(7), tz='America/New_York'))
  url = paste0('https://query1.finance.yahoo.com/v7/finance/download/IBM?period1=',
         strt,'&period2=',end,'&interval=1d&events=history&includeAdjustedClose=true')
  ibm = read.csv(url, stringsAsFactors = F) %>% dplyr::as_tibble()
  ibm$Date = as.Date(ibm$Date)

  nyse_hols <- xml2::read_html('https://www.nyse.com/markets/hours-calendars')
  nyse_hols <- nyse_hols %>%
    rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table(header = T)

  nyse_hols %>%
    tidyr::pivot_longer(cols = c(2:4)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dt = as.character(readr::parse_number(value)),
                  loc = stringr::str_locate(value,dt)[2],
                  val = paste0(substr(value,1,loc),' ',name),
                  date = as.Date(val,'%A, %B %d %Y')) %>%
    dplyr::filter(!is.na(dt)) -> nyse_fin


    datemap %>%
      dplyr::mutate(bus_day = dplyr::case_when(dayend %in% ibm$Date ~ 'yes',
                                               dayend <= max(ibm$Date) ~ 'no',
                                               dayofweek %in% c('Saturday','Sunday') ~ 'no',
                                               dayend %in% nyse_fin$date ~ 'no',
                                               TRUE ~ 'yes')) -> ef_datemap

    options(ef_datemap = ef_datemap)

    return(ef_datemap)

}


#' Number of business days or all days datediff
#'
#' @param start start date of filter
#' @param end end date of filter
#' @param bus_only indicate business only
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ef_date_diff('2020-01-01','2020-12-31',bus_only = FALSE)
#' ef_date_diff('2020-01-01','2018-12-31',bus_only = FALSE)
#' ef_date_diff('2020-01-05','2020-01-05',bus_only = FALSE)
#' ef_date_diff('2020-01-01','2020-12-31',bus_only = TRUE)
#'
#' }
ef_date_diff = function(start,end,bus_only = FALSE){

  ef_datemap <- getOption("ef_datemap")
  if (is.null(ef_datemap)) {
    ef_create_datemap()
    ef_datemap <- getOption("ef_datemap")
  }

  bus_filt = if(bus_only){c('yes')}else{c('yes','no')}

  ef_datemap %>%
    dplyr::filter(dayend >= min(start,end), dayend <= max(start,end), bus_day %in% bus_filt) -> date_list

  dd = nrow(date_list)-1

  return(ifelse(start<=end,dd,-dd))

}



#' Replace spaces with . in column header and convert to tibble
#'
#' @param tibDF Tibble or Dataframe
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' testdf = dplyr::tibble(`test col` = c(1:10))
#' ef_tibcol(testdf)
#'
#' }
ef_tibcol <- function(tibDF)
{
  tibDF1 <- dplyr::as_tibble(tibDF)
  colnames(tibDF1) <- gsub(' ','.',colnames(tibDF1))
  return(tibDF1)
}

