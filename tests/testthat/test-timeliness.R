# Tests for timelines()

## Test data ----

set.seed(42)
dft <- data.frame(
    'group1' = rep(c('A', 'B', "C"), 100),
    'group2' = rep(c('F', 'U'), 150),
    'd1' = c(seq.Date(as.Date('2023-01-01'), as.Date('2023-10-23'), 'day'), rep(NA_character_, 4))
) %>% 
mutate(
    d2 = d1 + sample(1:100, 300, replace = TRUE)
)

## timeliness() ----

test_that("timeliness() checks work", {
  
    expect_warning(
        timeliness(dft, 'd1', 'd2', units = 'hours'),
        '4 rows ignored where date columns NA.'
    )
    
    d <- unclass(dft)
    expect_error(timeliness(d, 'd1', 'd2', units = 'hours'))
    expect_error(
        timeliness(dft, 'd1', 'd2', 
            FUNS = list(mean, median), 
            colNames = list('mean', 'mean')
        ),
        'Duplicate values in colNames.'
    )
    expect_error(
        timeliness(dft, 'd1', 'd2',
            FUNS = list(mean, median),
            colNames = list('mean', 'group1')
        ),
        'colNames group1 already in use in df. Choose different colNames.'
    )
    
    d <- dplyr::filter(dft, !is.na(d1))
    expect_error(
        timeliness(d, 'd1', 'd2',
            FUNS = list(mean, median),
            colNames = list('mean')
        ),
        'colNames must be provided for each function in FUNS.'
    )
    
    d <- dft
    d$group1[1] <- NA
    expect_error(
        timeliness(d, 'd1', 'd2',
            FUNS = list(mean, median),
            colNames = list('mean', 'median'),
            gpBy = c('group1', 'group2')
        ),
        'Grouping columns cannot contain NA.'
    )
    
    d <- dft %>% 
        dplyr::filter(., !is.na(d1)) %>% 
        dplyr::mutate(d1 = as.character(d1), d2 = as.character(d2))
    d$d1[1] <- "ABC"
    expect_error(
        timeliness(d, 'd1', 'd2'),
        'Values in fromCol must be of, or coercible to, type Date or POSIXct.'
    )
    
    d <- dft %>% 
        dplyr::filter(., !is.na(d1)) %>% 
        dplyr::mutate(d1 = as.character(d1), d2 = as.character(d2))
    d$d2[1] <- "ABC"
    expect_error(
        timeliness(d, 'd1', 'd2'),
        'Values in toCol must be of, or coercible to, type Date or POSIXct.'
    )
    
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    
    d$d1[[1]] <- d$d2[[1]] + 1
    
    expect_warning(timeliness(d, 'd1', 'd2'), 'One or more dates in fromCol are > toCol.')
})

test_that('timeliness() ungroups grouped input',{
    d <- dplyr::filter(dft, !is.na(d1), !is.na(d2))
    d <- dplyr::group_by(d, group1, group2)
    expect_equal(class(d), c("grouped_df", "tbl_df", "tbl", "data.frame"))
    out <- timeliness(d, 'd1', 'd2')
    expect_equal(class(out), 'data.frame')
})

test_that('timeliness() coerces numeric dates',{
    d <- dft %>% 
        dplyr::filter(., !is.na(d1)) %>% 
        mutate(
            d1 = as.numeric(d1),
            d2 = as.numeric(d2)
        )
    expect_equal(is.numeric(d$d1), TRUE)
    expect_equal(is.numeric(d$d2), TRUE)
    out <- timeliness(d, 'd1', 'd2')
    expect_equal(class(out$d1), c("POSIXct", "POSIXt"))
    expect_equal(class(out$d2), c("POSIXct", "POSIXt"))
    expect_equal(class(out$timeliness), "difftime")
    
})

test_that('timeliness() skips non-aggregating functions when grouping applied', {
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    expect_warning(
        timeliness(d, 'd1', 'd2', gpBy = c('group1'), 
            FUNS = list(mean, median, function(x) x < 24), 
            colNames = list('mean', 'median', 'test'),
            units = 'hours'
        )
    )
})

test_that('timeliness() returns nrow same as length of groups when grouping applied', {
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    gp <- c('group1', 'd2')
    
    expect_error(
        timeliness(d, 'd1', 'd2', gpBy = gp, 
            FUNS = list(mean, median, function(x) x < 24), 
            colNames = list('mean', 'median', 'bob'),
            units = 'hours'
        ),
        'Cannot group by fromCol or toCol.'
    )
        
    gp <- c('group1', 'group2')
    gb <- unique(d[gp])
    
    out <- timeliness(d, 'd1', 'd2', gpBy = gp, 
        FUNS = list(mean, median), 
        colNames = list('mean', 'median'),
        units = 'hours'
    )
    
    expect_equal(nrow(gb), nrow(out))
    
    
})

test_that('timeliness() outputs for ungrouped use cases works',{
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    
    # Case custom functions no grouping produces columns of repeated
    # values for grouping functions (e.g., mean) and columns of
    # rowwise results for functions that process a single value
    
    out <- timeliness(d, fromCol = 'd1', toCol = 'd2',
        FUNS = list(mean,median, function(x) x, function(x) x <= 7),
        colNames = list('mean_timeliness', 'median_timeliness', 'time_diff', 'lt_1_week'),
        units = 'days'
    )
    
    expect_equal(length(unique(out$mean_timeliness)) == 1, TRUE)
    expect_equal(unname(table(out$lt_1_week)[1]), 274)
    expect_equal(unname(table(out$lt_1_week)[2]), 22)
    
    # Case with NAs
    suppressWarnings(
        out <- timeliness(dft, fromCol = 'd1', toCol = 'd2',
            FUNS = list(mean,median, function(x) x, function(x) x <= 7),
            colNames = list('mean_timeliness', 'median_timeliness', 'time_diff', 'lt_1_week'),
            units = 'days'
        )
    )
    
    expect_equal(
        complete.cases(out[297:300, c("d1", "d2", "mean_timeliness",
            "median_timeliness", "time_diff", "lt_1_week" )]
        ),
        c(FALSE, FALSE,FALSE,FALSE)
    )

    
    
})

test_that("timeliness() outputs for grouped use cases works",{
    
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    # Case no NA in date columns
    out <- timeliness(d, fromCol = 'd1', toCol = 'd2', gpBy = c('group1', 'group2'),
        FUNS = list(mean, median),
        colNames = list('mean_timeliness', 'median_tineliness'),
        units = 'days'
    )
    
    expect_equal(nrow(out), 6)
    expect_equal(ncol(out), 4)
    
    # Case NAs present in time cols. NAs are ignored
    
    suppressWarnings(
        out <- timeliness(dft, fromCol = 'd1', toCol = 'd2', gpBy = c('group1', 'group2'),
            FUNS = list(mean, median),
            colNames = list('mean_timeliness', 'median_tineliness'),
            units = 'days'
        )
    )
    
    expect_equal(nrow(out), 6)
    expect_equal(ncol(out), 4)
    
    d <- dft
    d[297, 3] <- as.Date('2023-10-24')
    
    suppressWarnings(
        out <- timeliness(d, fromCol = 'd1', toCol = 'd2', gpBy = c('group1', 'group2'),
            FUNS = list(mean, median),
            colNames = list('mean_timeliness', 'median_tineliness'),
            units = 'days'
        )
    )
    
    expect_equal(nrow(out), 6)
    expect_equal(ncol(out), 4)
    
})

test_that("timeliness() arguments passed to difftime works",{
    
    t <- sample(c('auto', 'secs', 'mins', 'hours', 'days', 'weeks'), 1)
    d <- dft %>% 
        dplyr::filter(., !is.na(d1))
    out <- timeliness(d, fromCol = 'd1', toCol = 'd2', units = t)
    expect_equal(attributes(out$timeliness[1])$units, t)
})


