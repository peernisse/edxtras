# nameEmptyCols ----

## nameEmptyCols() ----

test_that("nameEmptyCols() works", {

    df <- iris
    df$Sepal.Width <- NA
    expect_equal(nameEmptyCols(df), "Sepal.Width")

    df$Petal.Width <- NA
    empty <- nameEmptyCols(df)
    expect_equal(empty, c("Sepal.Width", "Petal.Width"))
    expect_equal(ncol(df[!names(df) %in% nameEmptyCols(df)]), 3)
})

test_that("nameEmptyCols() coerces non DF to DF", {

    df <- iris
    df$Sepal.Width <- NA

    dft <- as_tibble(df)
    expect_equal(nameEmptyCols(dft), "Sepal.Width")

    dfl <- as.list(df)
    expect_equal(nameEmptyCols(dfl), "Sepal.Width")

    dfm <- as.matrix(df)
    expect_equal(nameEmptyCols(dfm), "Sepal.Width")
})

test_that("nameEmptyCols() does empty DF", {

    df <- iris[0,]
    expect_equal(nrow(df), 0)
    expect_equal(nameEmptyCols(df), character())

    df <- iris[,0]
    expect_equal(ncol(df), 0)
    expect_equal(nameEmptyCols(df), character())
})






