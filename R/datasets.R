# Dataset documentation for edxtras

#' US zipcodes database
#'
#' A dataframe of US zipcodes and related geographic info
#'
#' @format A dataframe with 41877 rows and 24 variables
#' \describe{
#'   \item{zipcode}{US 5-digit zipcodes}
#'   \item{zipcode_type}{2010 State FIPS Code}
#'   \item{major_city}{Major city serving the ZIP code}
#'   \item{post_office_city}{Major city serving the ZIP code}
#'   \item{common_city_list}{List of common cities represented by the ZIP code}
#'   \item{county}{Name of county containing the ZIP code}
#'   \item{state}{Two-digit state code for ZIP code location}
#'   \item{lat}{Latitude of the centroid for the ZIP code}
#'   \item{lng}{Longitude of the centroid for the ZIP code}
#'   \item{timezone}{Timezone of the ZIP code}
#'   \item{radius_in_miles}{Radius of the ZIP code in miles}
#'   \item{area_code_list}{List of area codes for telephone numbers
#'   within this ZIP code}
#'   \item{population}{Total population of the ZIP code}
#'   \item{population_density}{Population density of the ZIP
#'   code (persons per square mile)}
#'   \item{land_area_in_sqmi}{Area of the land contained within the ZIP code
#'   in square miles}
#'   \item{water_area_in_sqmi}{Area of the waters contained within the ZIP code
#'   in square miles}
#'   \item{housing_units}{Number of housing units within the ZIP code}
#'   \item{occupied_housing_units}{Number of housing units within the ZIP code}
#'   \item{median_home_value}{Median home price within the ZIP code}
#'   \item{bounds_west}{Bounding box coordinates}
#'   \item{bounds_east}{Bounding box coordinates}
#'   \item{bounds_north}{Bounding box coordinates}
#'   \item{bounds_south}{Bounding box coordinates}
#' }
#'
#'
#' @source Taken from the `zipcodeR` package. That package gives new warnings
#' in 2023 due to reliance on deprecated package `sp`. THis dataset is all we
#' use in validation and so has been excerpted to remove dependency on
#' the `zipcodeR` package. Raw data source for updates:
#' https://github.com/MacHu-GWU/uszipcode-project/files/5183256/simple_db.log
#' 
#' zipcodeR package on GitHub
#' https://github.com/gavinrozzi/zipcodeR
#'
"zipcodeDB"


#' US Zipcodes list
#'
#' A vector of 5 digit US zipcodes to use for validation
#'
#' @format A Character vector of US 5 digit zipcodes
#'
#' @source The `zipcode` column of `zipcodeDB`
#' https://github.com/MacHu-GWU/uszipcode-project/files/5183256/simple_db.log
"zipcodes"
