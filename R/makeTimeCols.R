#' Insert year, month, week columns based on an index date column
#' @description Adds year month week columns to a dataframe based on an index date column
#' @param df The dataframe to modify
#' @param index_col Date The column with dates to index from. If character will be coerced to Date
#' @param date_format Character format argument to as.Date() see ?as.Date
#' @importFrom lubridate year month week is.Date
#' @export
#'
makeTimeCols <- function(df, index_col, date_format = '%Y-%m-%d') {
    if(!is.Date(df[[index_col]])){df[[index_col]] <- as.Date(df[[index_col]], format = date_format)}
    if(sum(is.na(df[[index_col]]))) stop('Error in makeTimeCols() data.format failed to coerce in as.Date()')
    df$Year <- lubridate::year(df[[index_col]])
    df$Month <- lubridate::month(df[[index_col]])
    df$Week <- lubridate::week(df[[index_col]])
    return(df)
}
