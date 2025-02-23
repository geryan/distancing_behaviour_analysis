
library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "mgcv",
    "readr",
    "tidyr",
    "purrr",
    "readxl"
  )
)

tar_source()

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
    pointless_end_target,
    "pointless_end_target"
  )

)
