# TESTS FOR validRules S3 Class functions

## validRules() ----

test_that('validRules() stops if no names',{
    x <- list(5,12)
    expect_true(is.null(names(x)))
    expect_error(validRules(x), 'x must be a named list or named vector')
})

test_that("validRules returns NULL on missing column name", {
    df <- data.frame('nothere' = as.Date('1974-06-22'))
    out <- validRules()[['default']][['datereported']](df$datereported)
  expect_equal(out, logical(0))
})

test_that('validRules does not have duplicate names',{
    expect_equal(length(names(validRules())), length(unique(names(validRules()))))
})

test_that('validRules returns list', {
    expect_true(is.list(validRules()))
    expect_equal(inherits(validRules(), 'validRules'), TRUE)
})

test_that('validRules() default rules are all functions',{
    yes <- purrr::map(names(validRules()[['default']]), 
               ~ is.function(validRules()[['default']][[.x]])) %>% 
        unlist() %>% 
        sum()
    
    expect_equal(length(validRules()[['default']]), yes)
})

test_that('validRules() stops on non-functions in list',{
    expect_error(validRules(list('bob' = 42)), 
        'Named list items in x must be functions.'
    )
})

test_that('validRules handles dates correctly',{
    x <- as.POSIXct(c(NA, "2023-06-08 UTC", "2023-06-04 UTC", NA, NA, NA, "2023-06-10 UTC", "2023-06-10 UTC", NA, "2023-06-01 UTC"))

    t <- c(
        sum(validRules()[['default']][['datereported']](x))/length(x)*100,
        sum(validRules()[['default']][['dob']](x))/length(x)*100,
        sum(validRules()[['default']][['collectiondate']](x))/length(x)*100,
        sum(validRules()[['default']][['datetested']](x))/length(x)*100,
        sum(validRules()[['default']][['visit_date']](x))/length(x)*100

    )

    expect_equal(t, c(50, 50, 50, 50, 50))
})

test_that('validRules rejects future dates',{
    x <- Sys.Date() + 1
    y <- Sys.Date() - 1

    t <- c(
        validRules()[['default']][['datereported']](x),
        validRules()[['default']][['datereported']](y),
        validRules()[['default']][['dob']](x),
        validRules()[['default']][['dob']](y),
        validRules()[['default']][['collectiondate']](x),
        validRules()[['default']][['collectiondate']](y),
        validRules()[['default']][['datetested']](x),
        validRules()[['default']][['datetested']](y),
        validRules()[['default']][['visit_date']](x),
        validRules()[['default']][['visit_date']](y)
    )

    expect_equal(sum(t)/length(t)*100, 50)

})

test_that('validRules() works with new zipcodes internal data object', {
    expect_equal(validRules()[['default']][['zipcode']]('84121'), TRUE)
    expect_equal(validRules()[['default']][['diagnosticzip']]('84121'), TRUE)
    expect_equal(validRules()[['default']][['facility_zip']]('84121'), TRUE)
    expect_equal(validRules()[['default']][['zipcode']]('ABC'), FALSE)
    expect_equal(validRules()[['default']][['facility_zip']](84121), TRUE)
    expect_equal(validRules()[['default']][['facility_zip']](42), FALSE)
})

## new_validRules() ----

test_that('new_validRules() corrects for x pluck depth of 1',{
    
    myrules <- list(
        'columnname1' = function(x) x + 1,
        'columnname2' = function(x) x - 1
    )
    
    expect_equal(purrr::pluck_depth(myrules), 1)
    
    out <- new_validRules(myrules)
    
    expect_equal(names(out), 'default')
    expect_equal(purrr::pluck_depth(out), 2)
    expect_true(inherits(out, 'validRules'))
    
})

test_that('new_validRules() assigns custom rulset name for x pluck depth of 1',{
    myrules <- list(
        'columnname1' = function(x) x + 1,
        'columnname2' = function(x) x - 1
    )
    expect_equal(purrr::pluck_depth(myrules), 1)
    expect_equal(names(new_validRules(myrules)), 'default')
    expect_equal(names(validRules(myrules)), 'default')
    
    expect_equal(names(new_validRules(myrules, 'TEST')), 'TEST')
    expect_equal(names(validRules(myrules, 'TEST')), 'TEST')
})

## validate_validRules() ----

test_that('validate_validRules() rejects bad structures', {
    
    myrules <- list(
        'columnname1' = function(x) x + 1,
        'columnname2' = function(x) x - 1
    )
    
    expect_equal(inherits(myrules,'validRules'), FALSE)
    expect_error(validate_validRules(myrules), 'Arg x is not of class "validRules"')
    
    mr <- data.frame(
        'columnname1' = 'Stuff',
        'columnname2' = 42
    )
    class(mr) <- 'validRules'
    expect_error(validate_validRules(mr), 'Named list items in x must be functions.')
    
    mr <- list('a1' = list('a2' = list('a3' = 42)))
    class(mr) <- 'validRules'
    expect_equal(purrr::pluck_depth(mr), 4)
    expect_error(validate_validRules(mr), 'x list depth cannot be > 2 nodes deep')
    
})

## addRules.validRules() ----

test_that('addRules.validRules() error stops work',{
    x <- list('a1' = 42)
    y <- list('a2' = 42)
    expect_error(addRules.validRules(x, y))
    
    myrules <- validRules(
        list(
            'columnname1' = function(x) x + 1,
            'columnname2' = function(x) x - 1
        )
    )
    expect_equal(class(myrules), 'validRules')
    expect_error(
        addRules.validRules(myrules, list(42,42), ruleset = 'new'),
        'y must be a named list or named vector'
    )
    expect_error(
        addRules.validRules(myrules, list('toodeep' = list(42)), ruleset = 'new'),
        'y list depth cannot be > 2 nodes deep'
    )
    
    expect_equal(
        names(
            addRules.validRules(
                myrules, list('columnname1' = function(x) x * 2), ruleset = 'new')
        ),
        c('default', 'new')
    )
    expect_error(
        addRules.validRules(
            myrules, list('columnname1' = function(x) x * 2), ruleset = 'default'),
        'y and x have duplicate names: columnname1. Names of x and y must be unique.'
    )
})

test_that('addRules.validRules()...',{
    myrules <- validRules(
        list(
            'columnname1' = function(x) x + 1,
            'columnname2' = function(x) x - 1
        )
    )
    out <- validRules() %>% addRules(myrules)
    expect_equal(names(out), 'default')
    
    expect_equal(c('columnname1','columnname2') %in% 
        names(out[['default']]), c(TRUE, TRUE))
    
    expect_equal(inherits(out, 'validRules'), TRUE)
    expect_equal(purrr::pluck_depth(out), 2)
    
    out <- validRules(myrules)
    expect_equal(names(out), 'default')
    expect_equal(purrr::pluck_depth(out), 2)
    expect_equal(inherits(out, 'validRules'), TRUE)
    
    expect_warning(validRules(myrules, 'myruleset'))
})

## editRules() ----

test_that('editRules() stops/warns work...',{
    n <- 100
    set.seed(42)
    myDF <- data.frame(
        'numbers' = sample(c(84121,85236,65987,NA_real_), n, replace = TRUE),
        'chars' = sample(c('word','sentence of words', 'Bob', '42', '1111'), n, replace = TRUE),
        'dates' = sample(seq.Date(as.Date('2021-01-01'), Sys.Date() + 100, 'days'), n, replace = TRUE)
    )
    rules <- list(
        'numbers' = function(x) is.na(as.numeric(x)),
        'chars' = function(x) grepl('[^0-9]', x),
        'dates' = function(x) {!is.na(as.POSIXct(x, format = "%m/%d/%Y")) & 
                as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()) & 
                as.POSIXct(x, format = "%m/%d/%Y") > as.POSIXct(Sys.Date() - 2*365)}
    )
    x <- validRules(rules, ruleset = 'myCustomRules')
    
    expect_error(
        editRules(
            x, 'NOTmyCustomRules', list('chars' = function(x) grepl('Bob', x))
        ), 
        'Target ruleset "NOTmyCustomRules" not present in x'
    )
    
    expect_error(
        editRules(x, 'myCustomRules', list('toodeep' = list('toodeep'))),
        '"newrules" node depth > 1. Multiple rulesets cannot be edited at once.'
    )
    
    expect_error(
        editRules(x, 
            'myCustomRules', 
            list('bob' = function(x) x + 1, 
                 'bob' = function(x) x - 1
            )
        ),
        'Named rules in "newrules" must have unique names.'
    )
})










