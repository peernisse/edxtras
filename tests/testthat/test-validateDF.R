# TESTS FOR validateDF()

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

## validateDF() ----

test_that('validateDF returns single row tibble', {
    cols <- ncol(testDF2)
    expect_true(tibble::is_tibble(validateDF(testDF2)))
    expect_equal(nrow(validateDF(testDF2)), 1)
    expect_equal(ncol(validateDF(testDF2)), cols)
})
