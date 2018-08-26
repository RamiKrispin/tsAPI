#' EIA API Category Query
#' @export eia_query
#' @description A function to pull the available categories in the eia API
#' @param api_key A character, the user API key for the eia website
#' @param category_id A character, the category ID as defined in the eia API
#' @return A list, with the series metadata


eia_query <- function(api_key, category_id = NULL){
  url <- get <- output <- NULL
  url <- paste("http://api.eia.gov/category/?api_key=", api_key, sep = "")
  if(!base::is.null(category_id)){
    url <- base::paste(url, "&category_id=", category_id, "&", sep = "")
  }

  get <- httr::GET(url = url)
  output <- jsonlite::fromJSON(httr::content(get, as = "text"))
  return(output)
}

#' EIA API Series Query
#' @export eia_series
#' @description A function to pull a series from the eia API based on series ID
#' @param api_key A character, the user API key for the eia website
#' @param series_id A character, the series ID as defined in the eia API,
#' to query for the available series IDs use the eia_query function
#' @return A list, with the series metadata

eia_series <- function(api_key, series_id){
  url <- get <- output <- NULL
  url <- paste("http://api.eia.gov/series/?series_id=", series_id, "&api_key=", api_key, sep = "")
  get <- httr::GET(url = url)
  output <- jsonlite::fromJSON(httr::content(get, as = "text"))
  return(output)
}


#' Parse a EIA Output
#' @export eia_parse
#' @description A parsing function for series from the EIA API
#' @param raw_series A list, the EIA API output for series request using the eia_series function
#' @param type A character, define the class of the output, possible options c("xts", "zoo", "ts", "data.frame", "data.table", "tbl")
#' @return A time series object according to the type argument setting
#' @example
#' \dontrun{
#' # Set you eia API key
#' api_key <- "Set you API key"
#'
#' # Querying the eia API to get possible categories
#' eia_query(api_key = api_key) # getting the full list of categories
#' eia_query(api_key = api_key, category_id = "0") # querying the Electricity category
#' childseries <- eia_query(api_key = api_key, category_id = "1") # querying the Electricity Net Generation child series
#' series_id <- childseries$category$childseries$series_id[1] # pulling the first series ID from the list of Net Generation series
#'
#' # Pulling the series from the eia API
#' raw_series <- eia_series(api_key = api_key, series_id = series_id)
#'
#' # Parsing the raw output to "ts" format
#' ts.obj <- eia_parse(raw_series = raw_series, type = "ts")
#'
#' # Parsing the raw output to "zoo" format
#' zoo.obj <- eia_parse(raw_series = raw_series, type = "zoo")
#' }
eia_parse <- function(raw_series, type = "xts"){

  # Error handling
  if(!type %in% c("xts", "zoo", "ts", "data.frame", "data.table", "tbl")){
    stop("The value of the 'type' is invalid, please one of the follow c('xts', 'zoo', 'ts', 'data.frame', 'data.table', 'tbl')")
  }
  `%>%` <- magrittr::`%>%`
  if(base::is.null(raw_series$series$data) ||
     base::is.null(raw_series$series$f)){
    stop("The input is not valid, please make sure you are using the output from 'eia_series' as input for the eia_parse function")
  }
  date <- raw_series$series$data[[1]][,1]
  data <- raw_series$series$data[[1]][,2]
  if(type == "xts"){
    if(raw_series$series$f == "M"){
      output <- xts::xts(x =  base::as.numeric(data), order.by =  zoo::as.yearmon(date))
    } else if (raw_series$series$f == "Q"){
      output <- xts::xts(x =  base::as.numeric(data), order.by =  zoo::as.yearqtr(date))
    } else if (raw_series$series$f == "A"){
      output <- xts::xts(x =  base::as.numeric(data), order.by =
                           lubridate::ymd(base::paste(base::as.numeric(date), "01-01", sep = "")))
    }
  } else if(type == "zoo"){
    if(raw_series$series$f == "M"){
      output <- zoo::zoo(x =  base::as.numeric(data), order.by =  zoo::as.yearmon(date))
    } else if (raw_series$series$f == "Q"){
      output <- zoo::zoo(x =  base::as.numeric(data), order.by =  zoo::as.yearqtr(date))
    } else if (raw_series$series$f == "A"){
      output <- zoo::zoo(x =  base::as.numeric(data), order.by =
                           lubridate::ymd(base::paste(base::as.numeric(date), "01-01", sep = "")))
    }
  } else if(type == "ts"){
    if(raw_series$series$f == "M"){
      df <- data.frame(data = base::as.numeric(data),
                       date = zoo::as.yearmon(date)) %>% dplyr::arrange(date)
      start_date <- zoo::as.Date.yearmon(min(df$date))
      output <- stats::ts(data =  df$data,
                          start = c(lubridate::year(start_date), lubridate::month(start_date)),
                          frequency = 12)
    } else if (raw_series$series$f == "Q"){
      df <- data.frame(data = base::as.numeric(data),
                       date = zoo::as.yearqtr(date)) %>% dplyr::arrange(date)
      start_date <- zoo::as.Date.yearqtr(min(df$date))
      output <- stats::ts(data =  df$data,
                          start = c(lubridate::year(start_date), lubridate::quarter(start_date)),
                          frequency = 4)
    } else if (raw_series$series$f == "A"){
      df <- data.frame(data = base::as.numeric(data),
                       date = base::as.numeric(date)) %>% dplyr::arrange(date)

      output <- stats::ts(data =  df$data,
                          start = base::min(df$date),
                          frequency = 1)
    }
  } else if(type %in% c("data.frame", "data.table", "tbl")){
    if(raw_series$series$f == "M"){
      df <- data.frame(data = base::as.numeric(data),
                       date = zoo::as.yearmon(date)) %>% dplyr::arrange(date)
    } else if (raw_series$series$f == "Q"){
      df <- data.frame(data = base::as.numeric(data),
                       date = zoo::as.yearqtr(date)) %>% dplyr::arrange(date)
    } else if (raw_series$series$f == "A"){
      df <- data.frame(data = base::as.numeric(data),
                       date = base::as.numeric(date)) %>% dplyr::arrange(date)
    }

    if(type == "data.frame"){
      output <- df
    } else if(type == "data.table"){
      output <- data.table::as.data.table(df)
    } else if(type == "tbl"){
      output <- dplyr::as.tbl(df)
    }
  }

  return(output)
}

