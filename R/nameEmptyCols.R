#' Get the dataframe names of columns which are all NA
#' @description Returns names of dataframe columns that are all NA/NULL.
#' @param df A dataframe to check. Attempts to coerce to df.
#' @importFrom purrr map
#' @returns Character vector of dataframe names
#'
#' @examples
#' df <- iris
#' df$Sepal.Width <- NA
#' nameEmptyCols(df)
#' df$Petal.Width <- NA
#' empty <- nameEmptyCols(df)
#' empty
#' df[!names(df) %in% nameEmptyCols(df)]
#'
#' @export
#'
nameEmptyCols <- function(df){
    if(!inherits(df, 'data.frame')) df <- as.data.frame(df)
    if(nrow(df) == 0 || ncol(df) == 0) return(character())
    e <- unlist(purrr::map(names(df), ~ sum(is.na(df[[.x]]), na.rm = TRUE)))
    names(e) <- names(df)
    return(names(e[e == nrow(df)]))
}
