#Creation of grid-weeks 
#create vector of weeks 
weeks <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  mutate(week = floor_date(Date, "week")) %>%
  distinct(week)
#grid x week
panel_df <- expand.grid(
  grid_id = grid_sf$grid_id,
  week = weeks$week
)

#aggregate territorial events on grid-week

territorial_events_sf <- territorial_events_sf %>%
  mutate(
    event_date = as.Date(event_date),
    week = floor_date(event_date, "week")
  )

territorial_grid <- st_join(
  territorial_events_sf,
  grid_sudan,
  left = FALSE
)

territorial_counts <- territorial_grid %>%
  st_drop_geometry() %>%
  count(grid_id, week, name = "n_territorial_events")

#check (for sanity)
summary(territorial_counts$n_territorial_events)
sum(is.na(territorial_counts$week))
