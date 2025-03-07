location_change <- function(dates = NULL) {

  google_change_trends <- readRDS("outputs/google_change_trends.RDS")

  location_change_trends <- google_change_trends %>%
    mutate(location = case_when(
      datastream == "residential" ~ "home",
      datastream == "transit stations" ~ "transit",
      datastream == "parks" ~ "public",
      datastream == "workplaces" ~ "work",
      datastream == "retail and recreation" ~ "retail",
      TRUE ~ "other"
    )) %>%
    filter(location != "other") %>%
    select(-state_datastream, -datastream) %>%
    pivot_wider(names_from = location, values_from = change)

  # optionally add all dates and states, and pad missing values (prior to first
  # mobility data) with 1s
  if (!is.null(dates)) {
    location_change_trends <- location_change_trends %>%
      group_by_all() %>%
      old_right_join(
        expand_grid(
          state = unique(.$state),
          date = dates
        )
      ) %>%
      ungroup() %>%
      mutate_at(vars(-state, -date), replace_na, 1)
  }

  location_change_trends

}
