#' Apply validateColumn and validRUles across a dataframe
#' @description Runs validation rules on all columns of dataframe df by column name. Returns NA if no validation rule exists
#' @param df A dataframe whose column values are subjected to validation rules via switch()
#' @param rulesObj List Named list of validation functions
#' @param ruleset Character Grouping name of custom ruleset.
#' @importFrom purrr map
#' @importFrom tidyr as_tibble
#' @family validation
#' @returns Single row dataframe with validation results and same columns as df
#' @examples
#' df <- data.frame('lastname' = c('AAA','BBB','CCC'),
#' 'gender' = c('Male','Female','M A L E'), 'norule' = c('A','B','C'))
#' validateDF(df)
#' rulesObj <- validRules()
#' rulesObj <- rulesObj %>% addRules(list('norule' = function(x) x =='B'))
#' validateDF(df, rulesObj = rulesObj)
#'
#' @export
validateDF <- function(df, rulesObj = NULL, ruleset = 'default'){
    if(!exists('validateColumn')){stop('missing function validateColumn()')}
    if(!nrow(df) > 0){stop('Validation failed. Dataframe has no rows')}
    names <- names(df)
    percValid <- map(names, ~ validateColumn(df, .x, rulesObj = rulesObj, ruleset = ruleset))
    names(percValid) <- names
    return(tidyr::as_tibble(percValid))
}
