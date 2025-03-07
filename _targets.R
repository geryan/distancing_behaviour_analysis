
library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "mgcv",
    "readr",
    "tidyr",
    "purrr",
    "readxl",
    #"greta",
    "ggplot2",
    "cowplot",
    "stringr"
  )
)

tar_source()

# need to download data an reactivate parse_uom_surveys in parse_all_surveys


list(
  tar_target(
    dates,
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2023-12-31"),
      by = "day"
    )
  ),

  tar_target(
    raw_mobility_data,
    download_mobility_data()
  ),

  tar_target(
    mobility_data,
    process_mobility_data(raw_mobility_data)
  ),

  tar_target(
    mobility_results,
    mobility_fit_and_predict(
      mobility_data
    )
  ),

  tar_target(
    micro_dat,
    get_microdistancing_data(
      dates = dates
    )
  ),

  tar_target(
    micro_results,
    microdistancing_model_fit_and_predict(
      data = micro_dat
    )
  ),

  tar_target(
    macro_data,
    get_macrodistancing_data(
      dates = dates
    )
  ),

  tar_target(
    pointless_end_target,
    "pointless_end_target"
  )

)
