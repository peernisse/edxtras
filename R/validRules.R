# validRules S3 Class

## Constructor ----

#' new_validRules
#' @description Create a new named list object of class validRules
#' @param x List of named validation rule functions of the form 
#' "'ruleName' = function(x) {rule logic for x...}" 
#' @param ruleset Character. List name to 
#' @family validation
#'
#' @noRd
new_validRules <- function(x = list(), ruleset = 'default'){
    stopifnot(is.list(x))
    out <- list()
    
    if(length(x) == 0 & ruleset == 'default'){
        
        out[[ruleset]] <- list(
            'firstname' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\- ]*[a-zA-Z ]*?$", x, perl = TRUE),
            'lastname' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\- ]*[a-zA-Z ]*?$", x, perl = TRUE),
            'dob' = function(x) !is.na(as.POSIXct(x, format = "%m/%d/%Y")) & as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()) & as.POSIXct(x, format = "%m/%d/%Y") > as.POSIXct(Sys.Date() - 110*365),
            'street' = function(x) grepl("^\\d+?(-|\\d*?)\\d+? \\w+.*$", x, perl = TRUE),
            'city' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\- ]*[a-zA-Z ]*?$", x, perl = TRUE),
            'state' = function(x) x %in% state.abb | x == "DC" | x == "PR",
            'zipcode' = function(x) substr(x, 1,5) %in% zipcodes,
            'phone' = function(x) x != "" & !is.na(x) & (x >= 1000000000 & x <= 9999999999),
            'race' = function(x) x %in% c("W","H","B","AI_AN","A","White","Black / African-American","Asian","American Indian","Native Hawaiian / Pacific Islander"),
            'ethnicity' = function(x) x %in% c("H", "NH","Not Hispanic or Latino","Hispanic or Latino"),
            'gender' = function(x) x %in% c("Male", "Female", "M", "F", "m", "f"),
            'pregnancy' = function(x) NA,
            
            'reportingagency' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\-, \\(, \\), \\_, \\/, \\. ]+[0-9]*[a-zA-Z ]*?$", x, perl = TRUE),
            #diagnostic_name?
            'diagnosticstreet' = function(x) grepl("^\\d+?(-|\\d*?)\\d+? \\w+.*$", x, perl = TRUE),
            'diagnosticcity' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\- ]*[a-zA-Z ]*?$", x, perl =  TRUE),
            'diagnosticstate' = function(x) x %in% state.abb | x == "DC" | x == "PR",
            'diagnosticzip' = function(x) substr(x, 1,5) %in% zipcodes,
            'facility_street' = function(x) grepl("^\\d+?(-|\\d*?)\\d+? \\w+.*$", x, perl = TRUE),
            'facility_city' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\- ]*[a-zA-Z ]*?$", x, perl =  TRUE),
            'facility_state' = function(x) x %in% state.abb | x == "DC" | x == "PR",
            'facility_zip'= function(x) substr(x, 1,5) %in% zipcodes,
            
            
            'clinician' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\.,\\,,\\- ]*[a-zA-Z ]*?$", x, perl = TRUE),
            'clinicianphone' = function(x) x != "" & !is.na(x) & (x >= 1000000000 & x <= 9999999999),
            'providerphone' = function(x) x != "" & !is.na(x) & (x >= 1000000000 & x <= 9999999999),
            
            #facility_visit_type?
            'visit_date' = function(x) !is.na(as.POSIXct(x, format = '%Y-%m-%d')) & as.POSIXct(x, format = '%Y-%m-%d') <= as.POSIXct(Sys.Date()),
            #admission_date?
            #discharge_date?
            
            'performinglab' = function(x) ifelse(x %in% c('unknown','UNKNOWN','UNK','unk',''), FALSE, grepl("^[a-zA-Z]+[a-zA-Z,\\',\\-, \\(, \\), \\_, \\/, \\. ]+[0-9]*[a-zA-Z ]*?$", x, perl = TRUE)),
            'testresult' = function(x) grepl("^\\w+", x, perl = TRUE) & !str_to_lower(x) %in% c('see notes', 'pending confirmation','pending confirmation only'),
            'datetested' = function(x) !is.na(as.POSIXct(x, format = "%m/%d/%Y")) & as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()),
            'collectiondate' = function(x) !is.na(as.POSIXct(x, format = "%m/%d/%Y")) & as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()),
            'datereported' = function(x) !is.na(as.POSIXct(x, format = "%m/%d/%Y")) & as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()),
            'localTestValid' = function(x) x == "valid",
            'clinicianfacility' = function(x) grepl("^[a-zA-Z]+[a-zA-Z,\\',\\-, \\(, \\), \\_, \\/, \\. ]+[0-9]*[a-zA-Z ]*?$", x, perl = TRUE),
            
            # New fields added Oct 2023
            
            'discharge_date' = function(x) !is.na(as.POSIXct(x, format = "%m/%d/%Y")) & as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date())
        )
        
    }
    
    if(length(x) >= 1 & purrr::pluck_depth(x) == 2 & ruleset != 'default')
        warning(paste0('Argument ruleset value "', ruleset, '" ignored because ruleset(s) names already provided in x'))
    if(length(x) >= 1 & purrr::pluck_depth(x) == 2) { out <- x } 
    if(length(x) >= 1 & purrr::pluck_depth(x) == 1) { out[[ruleset]] <- x }
    
    structure(out, class = 'validRules')
}

## Validator ----

#' validate_validRules
#' @family validation
#' @noRd
validate_validRules <- function(x){
    
    if(!inherits(x, 'validRules')) stop('Arg x is not of class "validRules"')
    if(!is.list(x)) x <- as.list(x)
    
    if(purrr::pluck_depth(x) == 1) stop('Here is an issue')
    
    if(purrr::pluck_depth(x) > 2) stop('x list depth cannot be > 2 nodes deep')
    if(purrr::pluck_depth(x) == 2){
        if(any(!purrr::map_lgl(seq_along(x[[1]]), ~ is.function(x[[1]][[.x]])))) 
            stop('Named list items in x must be functions.')
    }
    
    x
}

## Helper ----

#' Organize and access validation rules from a list of class `validRules`
#' @description List of validation rule functions for EDX validation use. 
#' THe default ruelset is from the EMSA QA2 application. If different rules 
#' are needed, create your own named list of rules and call validRules() to 
#' create the rules object. See examples.
#' @param x Named List or vector of validation rule functions. Names should 
#' correspond to column names in a dataframe being validated
#' @param ruleset Character Grouping ruleset group name to assign the list in x. 
#' Multiple rulesets can be contained in a validRUles object. Add new 
#' rulesets with `addRules()`. See `?addRules`
#' @returns List of validation functions named for the column name they validate
#' @family validation
#' @examples
#' df <- data.frame("lastname" = c("AAA", "BBB"))
#' 
#' # Use default rules for EMSA QA2 Datasource
#' names(validRules())
#' names(validRules()[['default']])
#' validRules()[["default"]][["lastname"]]
#' validRules()[["default"]][["lastname"]](df$lastname)
#' 
#' # Create custom rule lists
#' myrules <- list(
#'     'columnname1' = function(x) x + 1,
#'     'columnname2' = function(x) x - 1
#' )
#' 
#' validRules(myrules)
#' 
#' myRuleSet <- validRules(myrules, ruleset = 'myRulesetName')
#' 
#' myrules2 <- list(
#'     "ruleset1" = list(
#'         'columnname1' = function(x) x + 1,
#'         'columnname2' = function(x) x - 1
#'     ),
#'     
#'     "ruleset2" = list(
#'         'columnname1' = function(x) x + 1,
#'         'columnname2' = function(x) x - 1
#'     )
#' )
#' 
#' validRules(myrules2)
#' validRules(myrules2) %>% addRules(myRuleSet, ruleset = 'myRulesetName')
#' 
#'
#' @export
#'
validRules <- function(x = list(), ruleset = 'default'){
    if(length(x) > 0 & is.null(names(x))) 
        stop('x must be a named list or named vector')
    validate_validRules(new_validRules(x, ruleset = ruleset))
}

## Generics ----

#' Add Rules to a Validation Rules List
#' @description Generic to add items to a S3 list or vector
#' @param x Named List or vector object to add to
#' @param y Named List or vector to append to x
#' @param ruleset Character Name of rule set to add the rules to
#' in x. If not present, it will be created.
#'
#' @family validation
#' @examples
#' # Add rule to default 
#' rulesObj <- validRules()
#' rulesObj <- rulesObj %>% 
#'     addRules(list('newrule' = function(x) x =='B'), ruleset = 'newRuleSet')
#'     
#' str(rulesObj)
#'
#' @export
addRules <- function(x, y, ruleset = 'default'){
    UseMethod("addRules")
}

#' Edit Rules in a Validation Rules List
#' @description Generic to edit items to a S3 list or vector
#' @param x List of class validRules containing named rules to edit
#' @param ruleset Character Name of rule set to edit the rules to
#' @param newrules List Named list of functions to replace old functions with.
#' If newrules names are present, they are updated. If not present, they are ignored.
#'
#' @family validation
#' @examples
#' n <- 100
#' myDF <- data.frame(
#'     'numbers' = sample(c(84121,85236,65987,NA_real_), n, replace = TRUE),
#'     'chars' = sample(c('word','sentence of words', 'Bob', '42', '1111'), n, replace = TRUE),
#'     'dates' = sample(seq.Date(as.Date('2021-01-01'), Sys.Date() + 100, 'days'), n, replace = TRUE)
#' )
#' 
#' # Hypothetical validation requirements ----
#'    ## Find missing numbers in "numbers"
#'        ## Find numeric characters in "chars"
#'        ## Find dates older than 2 years or any dates in the future in "dates"
#'
#' # Set up named list of rules ----
#'
#' rules <- list(
#'     'numbers' = function(x) is.na(as.numeric(x)),
#'     'chars' = function(x) grepl('[^0-9]', x),
#'     'dates' = function(x) {!is.na(as.POSIXct(x, format = "%m/%d/%Y")) & 
#'         as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()) & 
#'         as.POSIXct(x, format = "%m/%d/%Y") > as.POSIXct(Sys.Date() - 2*365)}
#' )
#' 
#' # Create class `validRules` ruleset ----
#' 
#' myRules <- validRules(rules, ruleset = 'myRuleset')
#' 
#' validateDF(myDF, rulesObj = myRules, ruleset = 'myRuleset')
#' 
#' # Change some rules ----
#' 
#' myRules <- editRules(x = myRules, ruleset = 'myRuleset',
#'     newrules = list(
#'         'chars' = function(x) x == 'Bob',
#'         'notpresentrule' = function(x) x > 7000
#'     )                     
#' )
#' 
#' validateDF(myDF, rulesObj = myRules, ruleset = 'myRuleset')
#' @export
editRules <- function(x, ruleset = 'default', newrules){
    UseMethod("editRules")
}

## Methods ----

#' addRules() Method for Class `validRules` 
#' @family validation
#' @param x Named List or vector object to add to
#' @param y Named List or vector to append to x
#' @param ruleset Character Name of rule set to add the rules to
#' @importFrom purrr pluck pluck_depth
#' 
#' @export
#' @noRd
addRules.validRules <- function(x, y, ruleset = 'default'){
    stopifnot(inherits(x, 'validRules'))
    #if(is.null(names(y)) || names(y) == '') #TODO this breaks for multiple rules length > 1 in coercion to logical
    if(!sum(is.null(names(y))) == 0) stop('y must be a named list or named vector')
    if(purrr::pluck_depth(y) > 2) stop('y list depth cannot be > 2 nodes deep')
    
    if(ruleset %in% names(x)){
        if(purrr::pluck_depth(y) == 2) y <- purrr::pluck(y, 1)
        if(any(names(y) %in% names(x[[ruleset]])))
            stop(
                paste0('y and x have duplicate names: ',
                    intersect(names(y), names(x[[ruleset]])),
                    '. Names of x and y must be unique.')
            )
        
        x[[ruleset]] <- c(x[[ruleset]], unlist(y))
    }
    
    if(!ruleset %in% names(x)){
        if(purrr::pluck_depth(y) == 2) y <- purrr::pluck(y, 1)
        x[[ruleset]] <- y
    }
    
    validate_validRules(structure(x, class = 'validRules'))
}

#' editRules() Method for Class `validRules` 
#' @family validation
#' @param x Named List or vector object to edit
#' @param ruleset Character Name of rule set to edit the rules to
#' @param newrules List Named list of functions to replace old functions with.
#' If newrules names are present, they are updated. If not present, they are ignored.
#' @importFrom purrr pluck pluck_depth
#' 
#' @export
#' @noRd
editRules.validRules <- function(x, ruleset = 'default', newrules){
    if(!ruleset %in% names(x)) {stop(
        paste0('Target ruleset "', ruleset, '" not present in x'))
    } 
    if(purrr::pluck_depth(newrules) > 1) {
        stop('"newrules" node depth > 1. Multiple rulesets cannot be edited at once.')
    }
    if(length(names(newrules)) != length(unique(names(newrules)))) {
        stop('Named rules in "newrules" must have unique names.')
    }
    
    rulenames <- names(x[[ruleset]])
    newrules_names <- names(newrules)
    missingrules_names <- setdiff(newrules_names, rulenames)
    changerules_names <- intersect(rulenames, newrules_names)
    
    if(length(missingrules_names) > 0){
        message(paste0('Ignoring unmatched rule names: ', missingrules_names))
    }
    
    if(!length(changerules_names) > 0) {
        warning('No named rules in x match those given in "newrules". 
            No changes made to x.'
        )
        return(validate_validRules(structure(x, class = 'validRules')))
    } else {
        
        for(i in changerules_names){
            # TODO 12/20/2023 Try to make checking for if newrules are meant for the same 
            # data type as the rule it is replacing
            
            x[[ruleset]][[i]] <- newrules[[i]]
        }
        
        validate_validRules(structure(x, class = 'validRules'))
    }
}

#' Print method for S3 class validRules
#' @description Prints the rules pretty by ruleset
#' @param x List of grouped validation rule functions
#' @param ... Arguments passed to tidyr::as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#' @export
#' @noRd
print.validRules <- function(x, ...){
    stopifnot(class(x)[1] == 'validRules')
    print(tidyr::as_tibble(x), ...)
}

#' as.data.frame() method for S3 class validRules
#' @description Puts the rules in a tibble dataframe
#' @param x List of grouped validation rule functions with class `validRules`
#' @importFrom purrr map
#' @returns Tibble of rules with three columns Ruleset, Name, Rule 
#' @export
#' @noRd
as.data.frame.validRules <- function(x, row.names, optional, ...){
    ruleset <- names(x)
    rulesdf <- data.frame()
    
    for(i in seq_along(ruleset)){
        names <- names(x[[i]])
        calls <- purrr::map(seq_along(names), ~ 
            as.character(attributes(x[[i]][[.x]])[[1]])
        ) %>% unlist()
        
        #TODO 12/13/2023 this is in need of improvement
        
        if(length(names) != length(calls)){
            calls <- purrr::map(seq_along(names), ~ 
                as.character(enquote(x[[i]][[.x]]))[2]                        
            ) %>% unlist() 
        } 
        # this error only occurs in devtools::check(). It all works interactive and in devtools::run_examples()
        # The function attributes get stripped in `default` so this is needed to print the functions
        
        rulesdf <- dplyr::bind_rows(rulesdf,
            data.frame('Ruleset' = ruleset[[i]],'Name' = names, 'Rule' = calls)
        )
    }
    
    return(rulesdf)
}


