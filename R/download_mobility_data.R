download_mobility_data <- function(url){

  # download archived google mobility data:
  # details https://www.google.com/covid19/mobility/index.html
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

  raw_mobility_data <- readr::read_csv(
    url,
    col_types = cols(
      country_region_code = col_character(),
      country_region = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      date = col_date(format = "%Y-%m-%d"),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double(),
      census_fips_code = col_character()
    )
  ) |>
    filter(
      country_region == "Australia" & is.na(sub_region_2)
    )

  write_csv(
    x = raw_mobility_data,
    file = "data/mobility/Global_Mobility_Report_AUS.csv"
  )

  raw_mobility_data

}
