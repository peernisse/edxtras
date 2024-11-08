#' Validate multiple columns in each row of a dataframe
#' @returns Returns original dataframe with flag column `valid_rows` flags `TRUE`
#' where all specified columns return TRUE for their
#' respective validation rules, else `FALSE`.
#' Default behavior when `rulesObj` is not supplied returns flag column 
#' `valid_rows` flags `TRUE` when columns in `cols` satisfy `complete.cases()`.
#' If no `cols` are supplied, all columns are included in the evaluation.
#' 
#' @description Identify rows in a dataframe where specified columns are all
#' valid.
#' @param df  dataframe whose column values are subjected to validation rules
#' in combinations
#' @param cols Vector of column names to validate
#' @param rulesObj List Named list of validation functions
#' @param ruleset Character Grouping name of custom ruleset.
#' @family validation
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'     'c1' = seq.Date(as.Date('1974-06-22'), as.Date('2000-06-24'), 'day'),
#'     'c2' = sample(100:10000, 9500, replace = TRUE),
#'     'c3' = sample(c('misfit', 'toys', 'island', 'rich', NA_character_), 9500, replace = TRUE),
#'     'c4' = sample(zipcodes, 9500, replace = TRUE),
#'     'c5' = sample(zipcodeDB$median_home_value, 9500, replace = TRUE)
#' )
#' 
#' rulesObj <- validRules(
#'     list(
#'         'c1' = function(x) !is.na(x) & is.Date(x),
#'         'c2' = function(x) x >= 500,
#'         'c3' = function(x) x %in% c('misfit', 'toys', 'island'),
#'         'c4' = function(x) nchar(x) == 5,
#'         'c5' = function(x) !is.na(x) & is.numeric(x)
#'     ),
#'     ruleset = 'test'
#' )
#' 
#' # Defaults to `complete.cases()` if `rulesObj` is not supplied
#' 
#' validated_df <- validateRows(df)
#' head(validated_df)
#' 
#' # Defaults to `complete.cases()` for supplied `cols` if `rulesObj` is not supplied
#' 
#' validated_df <- validateRows(df, cols = c('c4', 'c5'))
#' head(validated_df)
#' 
#' # Returns rows where all `cols` return TRUE for rules supplied in `rulesObj`
#' 
#' validated_df <- validateRows(
#'     df, 
#'     cols = c('c3', 'c4', 'c5'), 
#'     rulesObj = rulesObj, 
#'     ruleset = 'test'
#' )
#' 
#' head(validated_df, 20)
#' 
#' @export
validateRows <- function(df, cols = list(), rulesObj = NULL, ruleset = 'default') {
    stopifnot(inherits(df, 'data.frame'))
    if(length(cols) == 0) cols <- names(df)
    if(!length(intersect(names(df), cols)) == length(cols)) {
        stop('All names in `cols` must be present in `df`')
    }
    if(!is.null(rulesObj) & !inherits(rulesObj, 'validRules')) {
        stop('`rulesObj` is not of class `validRules`')
    }
    # stop if ruleset not in names rulesObj
    # stop if rules names not in names df
    
    if(is.null(rulesObj)) {
        default <- which(complete.cases(df[, cols]) == TRUE)
        df$valid_rows <- ifelse(row.names(df) %in% default, TRUE, FALSE)
        return(df)
    }
    
    results <- purrr::map(cols, ~ which(rulesObj[[ruleset]][[.x]](df[[.x]]) == TRUE))
    valid <- Reduce(intersect, results)
    df$valid_rows <- ifelse(row.names(df) %in% valid, TRUE, FALSE)
    
    return(df)
}
