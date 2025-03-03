
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
    "cowplot"
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
    micro_dat,
    microdistancing_data(
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
    pointless_end_target,
    "pointless_end_target"
  )

)
