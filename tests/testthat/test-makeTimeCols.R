# TEST makeTimeCols ----

# makeTimeCols() ----

test_that("makeTimeCols() works", {
    df <- data.frame('date' = as.character('2023-11-03'))
    out <- makeTimeCols(df, index_col = 'date')
    expect_equal(dim(out), c(1, 4))
    expect_equal(names(out), c('date', 'Year', 'Month', 'Week'))
    expect_equal(out[1, 4], 44)

    df <- data.frame('date' = as.character('2023/11/03'))
    expect_error(makeTimeCols(df, index_col = 'date'))

    out <- makeTimeCols(df, index_col = 'date', date_format = '%Y/%m/%d')
    expect_equal(dim(out), c(1, 4))
    expect_equal(names(out), c('date', 'Year', 'Month', 'Week'))
    expect_equal(out[1, 4], 44)

})
