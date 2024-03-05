#' Apply validation rule from a list of rules to a column based on column name
#' @description Runs validation rules on specified column of a dataframe df by column name. Returns NA if no validation rule exists
#' @param df A dataframe whose column values are subjected to validation rules via switch()
#' @param col The name of the column to validate
#' @param rulesObj List Named list of validation functions
#' @param ruleset Character Grouping name of custom ruleset.
#' @family validation
#' @returns Numeric value percent of column values passing validation rule from validRules()
#' @examples
#' df <- data.frame('lastname' = c('AAA','BBB','CCC'), 'gender' = c('Male','Female','M A L E'))
#' validateColumn(df, 'lastname')
#' validateColumn(df, 'gender')
#' 
#' rulesObj <- validRules()
#' rulesObj <- rulesObj %>% addRules(list('norule' = function(x) x =='B'))
#' validateDF(df, rulesObj = rulesObj)
#' 
#' @export
#'
validateColumn <- function(df, col, rulesObj = NULL, ruleset = 'default'){
    if(is.null(rulesObj) & ruleset != 'default') stop('Custom ruleset but "rulesObj" not provided')
    if(is.null(rulesObj) & ruleset == 'default') validRules <- validRules()
    if(!is.null(rulesObj)) validRules <- function() {rulesObj}
    
    if(is.null(validRules()[[ruleset]][[col]])) valid <- NA
    if(!is.null(validRules()[[ruleset]][[col]])) valid <- validRules()[[ruleset]][[col]](df[[col]])

    if(!exists('valid')){out <- NA_real_}
    if(length(valid) == 1 && is.na(valid)){out <- NA_real_}
    if(length(valid) == 1 && !is.na(valid)){out <- round(sum(valid, na.rm = TRUE)/nrow(df)*100, 1)}
    if(length(valid) > 1){out <- round(sum(valid, na.rm = TRUE)/nrow(df)*100, 1)}

    return(out)
}
