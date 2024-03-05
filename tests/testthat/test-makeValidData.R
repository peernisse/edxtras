# TESTS FOR makeValidData()

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

## makeValidData ----

test_that('makeValidData stops on error if inputs not present', {
    df <- data.frame('reporter' = c('A','B','C'),'firstname' = c(1, 2, 3), 'clinician' = c('A', 'B', 'C' ))
    indexCol <- 'nothere'
    filtRep <- c('A','B','C')
    expect_error(makeValidData(df, indexCol, filtRep))

    indexCol <- 'reporter'
    filtRep <- c()
    expect_error(makeValidData(df, indexCol, filtRep))
})

test_that('makeValidData returns 2x rows of intersection indexCol and filtRep',{

    df <- data.frame('reporter' = sample(letters, 10, replace = TRUE),
                     'firstname' = c(1:10),
                     'clinician' = c(sample(letters, 8, replace = FALSE), NA, NA)
    )

    indexCol <- 'reporter'
    set.seed(42)
    filtRep <- sample(letters, 10, replace = F)
    x <- length(base::intersect(filtRep, df$reporter))
    out <- makeValidData(df, indexCol, filtRep)
    expect_equal(nrow(out), 2*x)
    expect_equal(unique(out$metric),c('Completeness', 'Validity'))

})

test_that('makeValidData returns ncol of df + 1', {
    df <- data.frame(
        'reporter' = sample(letters, 10, replace = T),
        'firstname' = c(1:10),
        'clinician' = c(sample(letters, 8, replace = F), NA, NA),
        'noRule' = sample(names(iris), 10, replace = TRUE),
        'NA' = rep(NA, 10),
        'T1' = rep(42, 10),
        'T2' = rep('shave', 10),
        'T3' = c(rep(69, 5),rep(NA,5)),
        'T4' = rep('shave', 10),
        'T5' = rep(NA, 10)
    )

    filtRep <- sample(letters, 10, replace = FALSE)
    df <- df[,c(1,2:sample(2:10,1))]
    indexCol <- 'reporter'
    out <- makeValidData(df, indexCol, filtRep)
    expect_equal(ncol(out), ncol(df) + 1)
})

test_that('makeValidData returns 50 for half NA', {
    #TODO Add a numeric column and a date column to this test
    df <- data.frame('reporter' = 'bob', 'clinician' = c(sample(letters,5), rep(NA, 5)))
    filtRep <- 'bob'
    indexCol <- 'reporter'
    out <- makeValidData(df, indexCol, filtRep)
    expect_equal(unique(out$clinician), 50)
})

test_that('makeValidData works with any indexCol and returns tibble',{
    #TODO add some tests for the actual output beyond just "is this a tibble"
    df <- data.frame(
        'reporter' = sample(letters, 10, replace = T),
        'firstname' = c(1:10),
        'clinician' = c(sample(letters, 8, replace = F), NA, NA),
        'noRule' = sample(names(iris), 10, replace = TRUE),
        'NA' = rep(NA, 10),
        'T1' = rep(42, 10),
        'T2' = rep('shave', 10),
        'T3' = c(rep(69, 5),rep(NA,5)),
        'T4' = rep(c('burma','shave'), 10),
        'T5' = rep(NA, 10)
    )

    indexCol <- sample(c('reporter','firstname','noRule','T1','T2','T4'), 1)
    filtRep <- unique(df[[indexCol]])

    expect(tibble::is_tibble(makeValidData(df, indexCol, filtRep)), 'Output is not a tibble')
})

test_that('makeValidData indexCol is present in output first column', {
    o1 <- makeValidData(testDF, 'one', unique(testDF$one))
    o2 <- makeValidData(testDF, 'two', unique(testDF$two))
    expect_equal(names(o1)[1], 'one')
    expect_equal(names(o2)[1], 'two')
})
