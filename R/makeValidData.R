#' Produce summary table of completeness and validity for variables in a dataframe
#' @description Runs completeness and validity calculations on input dataframe df.
#' Filters data by reporter to reduce process time. Use with map() to run on multiple reporters
#' @param df A dataframe whose column values are subjected to validation rules via switch()
#' @param indexCol The column to filter on. Should contain the values given in `filtRep`
#' @param filtRep Character A length 1 vector of value to filter on before
#' @param rulesObj List Named list of validation functions
#' @param ruleset Character Grouping name of custom ruleset.
#' calculation (e.g., input$filtRep a checkboxGroup of reporter names)
#' @family validation
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @importFrom dplyr select filter %>% mutate rename bind_rows arrange
#' @importFrom shiny validate
#' @returns Tibble with same columns as input DF, plus column "metric", and 2
#' rows: Completeness percentage and validity percentage
#' @examples
#' df <- data.frame('myindex' = c('Bob','Bob','Larry'),
#' 'lastname' = c('AAA','BBB','CCC'),
#' 'gender' = c('Male','Female','M A L E'), 'norule' = c('A','B','C'))
#' makeValidData(df, indexCol = 'myindex', filtRep = 'Bob')
#' makeValidData(df, indexCol = 'myindex', filtRep = 'Larry')
#' reps <- c('Bob', 'Larry')
#' purrr::map_df(reps, ~ makeValidData(df, indexCol = 'myindex', filtRep = .x))
#' @export
#'
makeValidData <- function(df, indexCol = 'reporter', filtRep, 
                          rulesObj = NULL, ruleset = 'default'){

    if(!nrow(df) > 0) stop(paste('DF dim is:', dim(df), 'No data to process.'))
    if(!indexCol %in% names(df)) stop('Index column is not present in the dataframe.')
    if(!length(filtRep) > 0) validate('No input values are present to filter on.')
    if(sum(filtRep %in% df[[indexCol]]) == 0) validate('Input values not present in indexCol')
    if(sum(is.na(filtRep)) > 0) warning('Filter values in filtRep contain NA')
    if(sum(!is.na(df[[indexCol]])) == 0) warning('indexCol is all NA. Output will be returned but group is NA')

    validityAllReporters <- df %>%
        rename(index = !!indexCol) %>%
        filter(index %in% filtRep) %>%
        nest(data = !index) %>%
        mutate(output = map(data, ~ validateDF(.x, rulesObj = rulesObj, ruleset = ruleset))) %>%
        select(index, output) %>%
        unnest(cols = c(output)) %>%
        mutate(metric = 'Validity') #%>%
    #rename(!!indexCol := 'index')

    completenessAllReporters <- df %>%
        rename(index = !!indexCol) %>%
        filter(index %in% filtRep) %>%
        nest(data = !index) %>%
        mutate(output = map(data, ~as.data.frame(t(round(colMeans(!is.na(.x))*100, 1))))) %>%
        select(index, output) %>%
        unnest(cols = c(output)) %>%
        mutate(metric = 'Completeness') #%>%
    #rename(!!indexCol := 'index')

    out <- bind_rows(validityAllReporters, completenessAllReporters) %>%
        select(index, metric, everything()) %>%
        mutate(
            index = factor(index, levels = index %>% unique() %>% sort())
        ) %>%
        arrange(index, metric) %>%
        rename(!!indexCol := 'index')

    return(out)
}
