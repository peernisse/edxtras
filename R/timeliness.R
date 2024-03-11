# Timeliness functions

#' Calculate time differences from 2 columns in a dataframe with various outputs
#' @description Compares 2 date columns in a dataframe. Wraps `base::difftime()` 
#' though with varying types of output based on arguments used. Supports custom
#' functions to be applied to output of `base::difftime()`. 
#' @importFrom dplyr ungroup bind_rows bind_cols mutate select all_of any_of
#' @importFrom tidyr nest
#' @importFrom stats complete.cases
#' @returns Original dataframe with column `timeliness` containing output of 
#' `base::difftime()`. If argument `FUNS` is used without grouping columns in
#' `gpBy`, `FUNS` output is propagated in added columns named from values
#' in `colNames`. If `gpBy` is used and `FUNS` contains aggregating functions 
#' (e.g., mean), returns summary dataframe with grouping columns and columns defined
#' by `FUNS` and `colNames`. Non-aggregating functions are ignored if `gpBy`
#' is used. 
#' 
#' @param df Dataframe A dataframe containing 2 columns of Date, POSIXct, or 
#' character coercible to date by `as.Date()`.
#' 
#' @param fromCol Character Name of the column containing the start dates.
#' 
#' @param toCol  Character Name of the column containing the end dates.
#' 
#' @param gpBy Character Vector of column names to group by. Ignored if `FUNS` 
#' is not used.
#' 
#' @param FUNS List Optional aggregating functions (e.g., mean, median, std, sum...)
#' or non-aggregating function to be applied to the output of `base::difftime()`.
#' If grouping with `gpBy` is not used, aggregating functions return single value
#' result column, and non-aggregating functions (e.g., function(x) x <= 7) 
#' return rowwise results. If grouping with `gpBy` is used, aggregating functions 
#' return a single value for each group, non-aggregating functions are ignored and a 
#' warning is displayed.
#'  
#' @param colNames List List of characters to use as column names for columns 
#' created by functions in `FUNS`.
#' 
#' @param ... Additional arguments passed to `base::difftime()`
#' 
#' @examples
#' dft <- data.frame(
#' 'group1' = rep(c('A', 'B', "C"), 100),
#' 'group2' = rep(c('F', 'U'), 150),
#' 'd1' = c(seq.Date(as.Date('2023-01-01'), as.Date('2023-10-23'), 'day'), rep(NA_character_, 4))
#' ) %>% 
#' dplyr::mutate(
#'     d2 = d1 + sample(1:100, 300, replace = TRUE)
#' )
#' 
#' # Will warn about NAs
#' out <- timeliness(dft, 'd1', 'd2', units = 'hours')
#' head(out)
#' tail(out)
#' 
#' # Removing NAs
#' d <- dft %>% 
#' dplyr::filter(., !is.na(d1))
#' 
#' out <- timeliness(d, 'd1', 'd2', units = 'hours')
#' head(out)
#' 
#' suppressWarnings(
#'     out <- timeliness(dft, fromCol = 'd1', toCol = 'd2',
#'         FUNS = list(mean,median, function(x) x, function(x) x <= 7),
#'         colNames = list('mean_timeliness', 'median_timeliness', 
#'                 'time_diff', 'lt_1_week'),
#'              units = 'days'
#'     )
#' )
#' head(out)
#' 
#' # Using grouping with gpBy argument
#' 
#' out <- timeliness(d, fromCol = 'd1', toCol = 'd2', gpBy = c('group1', 'group2'),
#'     FUNS = list(mean, median),
#'     colNames = list('mean_timeliness', 'median_timeliness'),
#'     units = 'days'
#' )
#' head(out)
#' 
#' 
#' @export
timeliness <- function(df, fromCol, toCol, gpBy = NULL, FUNS = list(), colNames = list(), ...) {
    
    stopifnot(inherits(FUNS, 'list'))
    stopifnot(inherits(colNames, 'list'))
    stopifnot(inherits(df, 'data.frame'))
    if(any(gpBy %in% c(fromCol, toCol))) stop("Cannot group by fromCol or toCol.")
    if(inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    
    if(any(duplicated(colNames))) stop('Duplicate values in colNames.')
    if(any(colNames %in% names(df))) {
        dups <- intersect(colNames, names(df))
        stop(paste('colNames', dups, 'already in use in df. Choose different colNames.'))
    }
    
    if(!is.null(gpBy)) {
        chk <- df %>% dplyr::select(all_of(!!gpBy))
        if(any(is.na(chk))) stop('Grouping columns cannot contain NA.')
    }
    
    keep <- which(complete.cases(dplyr::select(df, all_of(c(fromCol, toCol)))))
    nouse <- which(!complete.cases(dplyr::select(df, all_of(c(fromCol, toCol)))))
    
    if(length(keep) != nrow(df) & length(nouse) > 0) {
        warning(paste(nrow(df) - length(keep), 'rows ignored where date columns NA.'))
        nouse <- df[nouse,]
        df <- df[keep,]
    }
    
    if(length(FUNS) != length(colNames)) {
        stop('colNames must be provided for each function in FUNS.')
    }
    
    if(is(try(as.POSIXct(as.Date(df[[fromCol]])), silent = TRUE) , 'try-error')) {
        stop('Values in fromCol must be of, or coercible to, type Date or POSIXct.')
    }
    
    if(is(try(as.POSIXct(as.Date(df[[toCol]])), silent = TRUE) , 'try-error')) {
        stop('Values in toCol must be of, or coercible to, type Date or POSIXct.')
    }
    
    # Coerce numeric date columns to POSIXct, as.Date handles numeric dates correctly
    
    if(any(is.numeric(df[[fromCol]]), is.numeric(df[[toCol]]))) {
        df[[fromCol]] <- as.POSIXct(as.Date(df[[fromCol]]))
        df[[toCol]] <- as.POSIXct(as.Date(df[[toCol]]))
    }
    
    if(any(df[[fromCol]] > df[[toCol]])) warning('One or more dates in fromCol are > toCol.')
    
    # Case no functions provided return timeliness column
    
    if(length(FUNS) == 0) {
        if(!is.null(gpBy)) warning('Argument gpBy ignored because no FUNS provided.')
        timeliness <- difftime(df[[toCol]], df[[fromCol]], ...)
        tls <- cbind(df, timeliness)
        if(is.data.frame(nouse)) {
            out <- (dplyr::bind_rows(tls, nouse))
            names(out) <- names(tls)
        } else {
            out <- tls
        }
        
        return(out)
    }
    
    # Case functions provided
    
    if(length(FUNS) > 0) {
        
        if(!is.null(gpBy)) {
            
            #check for if any of FUNS return not a length 1 result and ignore and warn
            
            for(i in seq_along(FUNS)) {
                fn <- match.fun(FUNS[[i]])
                if(length(fn(1:2)) > 1) {
                    warning(paste(c('Non-summary function:',eval(fn),' will be skipped because gpBy is used.'), collapse = ' '))
                }
            }
          
            #remove extraneous columns
            
            dfg <- df %>% dplyr::select(all_of(gpBy), all_of(c(fromCol, toCol)))
            
            #nest to calculate
            
            g <- tidyr::nest(dfg, data = !any_of(gpBy)) 
            df <- data.frame()
            
            for(i in seq_along(g$data)) {
          
                d <- g[i, ]
                
                dat <- g$data[[i]]
                out <- dplyr::select(d, !data)
                
                for(j in seq_along(FUNS)) {
                    
                    fn <- match.fun(FUNS[[j]])
                    name <- colNames[[j]]
                    
                    if(length(fn(difftime(dat[[toCol]], dat[[fromCol]], ...))) > 1) {
                        next
                    }
                    
                    newd <- d %>% 
                        dplyr::mutate(
                            !!name := fn(difftime(dat[[toCol]], dat[[fromCol]], ...))
                        ) %>% 
                        dplyr::select(all_of(name))
                    out <- dplyr::bind_cols(out, newd)
                }
                
                df <- dplyr::bind_rows(df, out)
            }
            
            return(df)
            
        } else {
            
            for(i in 1:length(FUNS)) {
                fn <- match.fun(FUNS[[i]])
                name <- colNames[[i]]
                df <- dplyr::mutate(df, !!name := fn(difftime(df[[toCol]], df[[fromCol]], ...)))
            }
            
            if(length(nouse) > 0) {
                return(dplyr::bind_rows(df, nouse))
            } else {return(df)}
        }
    }
}
