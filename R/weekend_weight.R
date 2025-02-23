weekend_weight <- function(date) {
  weekend_today <- is_weekend(date)
  weekend_yesterday <- is_weekend(date - 1)
  (weekend_today + weekend_yesterday) / 2
}
