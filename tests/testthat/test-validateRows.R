# validateRows

## validateRows() ----

test_that("validateRows() defaults to complete.cases...", {
    set.seed(42)
    df <- data.frame(
        'c1' = seq.Date(as.Date('1974-06-22'), as.Date('2000-06-24'), 'day'),
        'c2' = sample(100:10000, 9500, replace = TRUE),
        'c3' = sample(c('misfit', 'toys', 'island', 'rich', NA_character_), 9500, replace = TRUE),
        'c4' = sample(zipcodes, 9500, replace = TRUE),
        'c5' = sample(zipcodeDB$median_home_value, 9500, replace = TRUE)
    )
    
    out <- validateRows(df, cols = c('c3', 'c5'), rulesObj = NULL, ruleset = NULL)
    expect_true('valid_rows' %in% names(out))
    expect_true(ncol(out) == ncol(df) + 1)
    expect_equal(length(which(out$valid_rows == TRUE)), 5735)
    
})

test_that("validateRows() argument stops work", {
    set.seed(42)
    df <- data.frame(
        'c1' = seq.Date(as.Date('1974-06-22'), as.Date('2000-06-24'), 'day'),
        'c2' = sample(100:10000, 9500, replace = TRUE),
        'c3' = sample(c('misfit', 'toys', 'island', 'rich', NA_character_), 9500, replace = TRUE),
        'c4' = sample(zipcodes, 9500, replace = TRUE),
        'c5' = sample(zipcodeDB$median_home_value, 9500, replace = TRUE)
    )
    
    dft <- tibble::as_tibble(df)
    expect_equal(which(validateRows(dft)$valid_rows == 'valid'), which(validateRows(df)$valid_rows == 'valid'))
    
    expect_error(
        validateRows(df, cols = c('not', 'here')), 
        'All names in `cols` must be present in `df`'
    )
    
    expect_error(
        validateRows(df, rulesObj = list('not' = 'right class')),
        '`rulesObj` is not of class `validRules`'
    )
    
    df <- 'this sentence'
    expect_error(validateRows(df))
    
})

test_that('validateRows() applies rules on specified columns', {
    set.seed(42)
    df <- data.frame(
        'c1' = seq.Date(as.Date('1974-06-22'), as.Date('2000-06-24'), 'day'),
        'c2' = sample(100:10000, 9500, replace = TRUE),
        'c3' = sample(c('misfit', 'toys', 'island', 'rich', NA_character_), 9500, replace = TRUE),
        'c4' = sample(zipcodes, 9500, replace = TRUE),
        'c5' = sample(zipcodeDB$median_home_value, 9500, replace = TRUE)
    )
    
    rulesObj <- validRules(
        list(
            'c1' = function(x) !is.na(x) & is.Date(x),
            'c2' = function(x) x >= 500,
            'c3' = function(x) x %in% c('misfit', 'toys', 'island'),
            'c4' = function(x) nchar(x) == 5,
            'c5' = function(x) !is.na(x) & is.numeric(x)
        ),
        ruleset = 'test'
    )
    
    out <- validateRows(df, cols = c('c1', 'c2'), rulesObj = rulesObj, ruleset = 'test')
    expect_equal(nrow(df), nrow(out))
    
    expect_equal(length(which(out$valid_rows == TRUE)), 9148)
    
    out <- validateRows(df, cols = c('c1', 'c2', 'c5'), rulesObj = rulesObj, ruleset = 'test')
    expect_equal(nrow(df), nrow(out))
    
    expect_equal(length(which(out$valid_rows == TRUE)), 6938)
    
})
















