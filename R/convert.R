#' First Day of the Ethiopian Year
#'
#' This function returns the first day of the Ethiopian year
#' @param year numeric year to find the first day of
#' @export
firstDayEthiopian <- function(year) {
  firstDay <- floor(year / 100) - floor(year / 400) - 4

  # if the prev ethiopian year is a leap year, new-year is on 12th
  firstDay[(year - 1) %% 4 == 3] <- firstDay[(year - 1) %% 4 == 3] + 1

  return(firstDay)
}

#' Convert Ethiopian Date to Gregorian Calendar
#'
#' This function takes an Ethiopian date and converts
#' it to a Gregorian date.
#' @param ethiopianDate Date or character representing a date in the Ethiopian calendar
#' @return Date from the Gregorian calendar
#' @export
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate as_date
toGregorian <- function(ethiopianDate) {
  if (!length(ethiopianDate)) { return(NULL) }
  if (is.na(ethiopianDate)) { return(NA) }
  if (is.character(ethiopianDate) && ethiopianDate == '') { return(NULL) }

  year <- lubridate::year(ethiopianDate)
  month <- lubridate::month(ethiopianDate)
  day <- lubridate::day(ethiopianDate)

  gregorianDate <- ethiopianToGregorian(year, month, day)

  return(lubridate::as_date(gregorianDate, format = '%Y-%m-%d'))
}

#' Convert Ethiopian Date to Gregorian Calendar
#'
#' This function takes the year, month and day associated with 
#' a date in the Ethiopian calendar and converts it to a 
#' date in the Gregorian calendar.
#' @param year numeric representing Ethiopian year
#' @param month numeric representing Ethiopian month
#' @param date numeric representing Ethiopian day
#' @return character representation of Gregorian date
#' @export
ethiopianToGregorian <- function(year = numeric(), month = numeric(), date = numeric()) {
  if (any(is.na(c(year,month,day)))) { return(NA) }
  if (any(!length(c(year,month,day)))) { return(NULL) }
  if (any(c(year,month,day) == '')) { return(NULL) }

  dates <- data.frame('year'=year, 'month'=month, 'date'=date)

  findGregorianDate <- function(row) {
    row <- as.list(row)
    firstDay = firstDayEthiopian(row$year)
  
    # September (Ethiopian) sees 7y difference
    gregorianYear = row$year + 7
  
    # Number of days in gregorian months
    # starting with September (index 1)
    # Index 0 is reserved for leap years switches.
    gregorianMonths = c(0, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30)
  
    # if next gregorian year is leap year, February has 29 days.
    nextYear = gregorianYear + 1
    if (((nextYear %% 4 == 0) && (nextYear %% 100 != 0)) || (nextYear %% 400 == 0)) {
      gregorianMonths[7] <- 29
    }
  
    # calculate number of days up to that date
    until <- ((row$month - 1) * 30) + row$date
    if (until <= 37 && row$year <= 1575) {  # mysterious rule
      until <- until + 28
      gregorianMonths[1] = 31
    } else {
      until <- until + firstDay - 1
    }
  
    # if ethiopian year is leap year, paguemain has six days
    if (row$year - 1 %% 4 == 3) {
      until <- until + 1
    }
  
    # calculate month and date incremently
    m <- 1
    for (i in seq(1, length(gregorianMonths))) {
      if (until <= gregorianMonths[i]) {
        m <- i
        gregorianDate <- until
        break
      } else {
        m <- i
        until <- until - gregorianMonths[i]
      }
    }
  
    # if m > 4, we're already on next Gregorian year
    if (m > 5) {
      gregorianYear <- gregorianYear + 1
    }
  
    # Gregorian months ordered according to Ethiopian
    order = c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    gregorianMonth = order[m]
  
    gregorianDate <- ISOdate(gregorianYear, gregorianMonth, gregorianDate)
    gregorianDate <- format(gregorianDate, '%Y-%m-%d')
  
    return(gregorianDate)
  }
 
  dates$gregorianDate <- apply(dates, 1, findGregorianDate)
  return(dates$gregorianDate)
}

#' Convert Gregorian Date to Ethiopian Calendar
#'
#' This function takes a Gregorian date and converts
#' it to an Ethiopian date.
#' @param gregorianDate Date or character representing a date in the Gregorian calendar
#' @return Date from the Ethiopian calendar
#' @export
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate as_date
toEthiopian <- function(gregorianDate) {
  if (!length(gregorianDate)) { return(NULL) }
  if (is.na(gregorianDate)) { return(NA) }
  if (is.character(gregorianDate) && gregorianDate == '') { return(NULL) }

  year <- lubridate::year(gregorianDate)
  month <- lubridate::month(gregorianDate)
  day <- lubridate::day(gregorianDate)

  ethiopianDate <- gregorianToEthiopian(year, month, day)

  return(lubridate::as_date(ethiopianDate, format = '%Y-%m-%d'))
}

#' Convert Gregorian Date to Ethiopian Calendar
#'
#' This function takes the year, month and day associated with 
#' a date in the Gregorian calendar and converts it to a 
#' date in the Ethiopian calendar.
#' @param year numeric representing Gregorian year
#' @param month numeric representing Gregorian month
#' @param date numeric representing Gregorian day
#' @return character representation of Ethiopian date
#' @export
gregorianToEthiopian <- function(year = numeric(), month = numeric(), date = numeric()) {
  if (any(is.na(c(year,month,day)))) { return(NA) }
  if (any(!length(c(year,month,day)))) { return(NULL) }
  if (any(c(year,month,day) == '')) { return(NULL) }

  dates <- data.frame('year'=year, 'month'=month, 'date'=date)

  # dates between 5-14 of May 1582 are invalid
  if (any(dates$month == 10 & dates$date >= 5 & dates$date <= 14 & dates$year == 1582)) {
     stop("Invalid Date between 5-14 May 1582.")
  }

  findEthiopianDate <- function(row) {
    row <- as.list(row)
    # Number of days in gregorian months
    # Index 1 is reserved for leap years switches.
    gregorianMonths <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    
    # Number of days in ethiopian months
    # Index 1 is reserved for leap years switches.
    ethiopianMonths <- c(0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 5, 30, 30, 30, 30)

    # if gregorian leap year, February has 29 days.
    if (((row$year %% 4 == 0) && (row$year %% 100 != 0)) || (row$year %% 400 == 0)) {
      gregorianMonths[3] <- 29
    }
  
    # September sees 8y difference
    ethiopianYear <- row$year - 8
  
    # if ethiopian leap year pagumain has 6 days
    if (ethiopianYear %% 4 == 3) {
      ethiopianMonths[11] <- 6
    } else {
      ethiopianMonths[11] <- 5
    }
  
    # Ethiopian new year in Gregorian calendar
    firstDay = firstDayEthiopian(row$year - 8)
  
    until <- sum(gregorianMonths[1:row$month])
    until <- until + row$date
  
    # update tahissas (december) to match january 1st
    if (ethiopianYear %% 4 == 0) {
      tahissas <- 26
    } else {
      tahissas <- 25
    }
  
    # take into account the 1582 change
    if (row$year < 1582) {
      ethiopianMonths[2] <- 0
      ethiopianMonths[3] <- tahissas
    } else if (until <= 277 && row$year == 1582) {
      ethiopianMonths[2] <- 0
      ethiopianMonths[3] <- tahissas
    } else {
      tahissas <- firstDay - 3
      ethiopianMonths[2] <- tahissas
    }
  
    # calculate month and date incremently
    m = 1
    for (m in seq(1, length(ethiopianMonths))) {
      if (until <= ethiopianMonths[m]) {
        if (m == 2 || ethiopianMonths[m] == 0) {
          ethiopianDate <- until + (30 - tahissas)
        } else {
          ethiopianDate <- until
          break
        }
      } else {
        until <- until - ethiopianMonths[m]
      }
    }
  
    if (m > 10) {
      ethiopianYear <- ethiopianYear + 1
    }
  
    # Ethiopian months ordered according to Gregorian
    order = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1, 2, 3, 4)
    ethiopianMonth = order[m]
  
    ethiopianDate <- ISOdate(ethiopianYear, ethiopianMonth, ethiopianDate)
    ethiopianDate <- format(ethiopianDate, '%Y-%m-%d')
  
    return(ethiopianDate)
  }

  dates$ethiopianDate <- apply(dates, 1, findEthiopianDate)
  return(dates$ethiopianDate)
}
