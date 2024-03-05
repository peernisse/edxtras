# TESTS FOR validateColumn()

# TEST DATA ----

testDF <- data.frame(
    'one' = c(1,2,3),
    'two' = c('A','B','C')
)

testDF2 <- data.frame(
    'firstname' = c(1, 2, 3),
    #'clinician' = 'A, B, C',
    'clinician' = c('A', 'B', 'C')
)

## validateColumn() ----

test_that("validateColumn returns NA when no validation rule", {
    expect_equal(validateColumn(testDF, 'one'), NA_real_)
    expect_equal(validateColumn(testDF, 'two'), NA_real_)
})

test_that("validateColumn returns 0 for wrong datatype", {
    expect_equal(validateColumn(testDF2, 'firstname'), 0)
})

test_that("validateColumn returns 100", {
    expect_equal(validateColumn(testDF2, 'clinician'), 100)
})

test_that('validateColumn counts null for coersion of non-date or different format', {
    df <- data.frame('datetested' = c('06/22/1974','02/45/2022'))
    col <- 'datetested'
    expect_true(is.character(df[[col]]))
    expect_equal(validateColumn(df, col), 50)

    df <- data.frame('datetested' = c('06-22-1974','02-10-2022'))
    col <- 'datetested'
    expect_true(is.character(df[[col]]))
    expect_equal(validateColumn(df, col), 0)
})
