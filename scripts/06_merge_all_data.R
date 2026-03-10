#merge all data
#do we have all we need? check check: 

#all grid-week combinations: 
panel_df
#all hospital attacks per grid week
hospital_attacks_grid_week
#all territorial counts:
territorial_counts
#conflict intensity per grid-week
conflict_intensity_grid_week
#population per grid
population_grid

#make sure to have the same time period of the data
# Check min / max week in each dataset
range(hospital_attacks_grid_week$week, na.rm = TRUE)
range(territorial_counts$week, na.rm = TRUE)
range(conflict_intensity_grid_week$week, na.rm = TRUE)

start_week <- as.Date("2023-03-26")  # latest common start
end_week   <- as.Date("2024-11-03")  # earliest common end

hospital_attacks_grid_week <- hospital_attacks_grid_week %>%
  dplyr::filter(week >= start_week, week <= end_week)

territorial_counts <- territorial_counts %>%
  dplyr::filter(week >= start_week, week <= end_week)

conflict_intensity_grid_week <- conflict_intensity_grid_week %>%
  dplyr::filter(week >= start_week, week <= end_week)

#check if correctly filtered
range(hospital_attacks_grid_week$week)
range(territorial_counts$week)
range(conflict_intensity_grid_week$week)
#check numbers of weeks per set
Reduce(
  intersect,
  list(
    unique(hospital_attacks_grid_week$week),
    unique(territorial_counts$week),
    unique(conflict_intensity_grid_week$week)
  )
) |> length()
#correct the weeeks of territorial counts
all_weeks <- seq(
  from = as.Date("2023-03-26"),
  to   = as.Date("2024-11-03"),
  by   = "week"
)

territorial_counts <- territorial_counts %>%
  tidyr::complete(
    grid_id,
    week = all_weeks,
    fill = list(n_territorial_events = 0)
  )

range(hospital_attacks_grid_week$week)
range(territorial_counts$week)
range(conflict_intensity_grid_week$week)

#check: same keys? 
lapply(
  list(
    panel_df,
    hospital_attacks_grid_week,
    territorial_counts,
    conflict_intensity_grid_week
  ),
  function(x) names(x)
)

#merge (left joins, no inner join or we lose structural zeros)
panel_master <- panel_df %>%
  left_join(
    hospital_attacks_grid_week,
    by = c("grid_id", "week")
  ) %>%
  left_join(
    territorial_counts,
    by = c("grid_id", "week")
  ) %>%
  left_join(
    conflict_intensity_grid_week,
    by = c("grid_id", "week")
  )
#transform NAs to 0, for ZINB essential
panel_master <- panel_master %>%
  mutate(
    hospital_attacks = ifelse(is.na(hospital_attacks), 0, hospital_attacks),
    n_territorial_events = ifelse(is.na(n_territorial_events), 0, n_territorial_events),
    conflict_intensity = ifelse(is.na(conflict_intensity), 0, conflict_intensity)
  )
#sanity and mandatory checks
summary(panel_master$hospital_attacks)
mean(panel_master$hospital_attacks)
#mean is almost zero: 0.0005526495
#check logic
panel_master %>%
  dplyr::filter(hospital_attacks > 0) %>%
  dplyr::select(grid_id, week, hospital_attacks, conflict_intensity, n_territorial_events) %>%
  head(10)

#check wether it happens often that hosptial attack = 1 and rest =0
hospital_only <- panel_master %>%
  dplyr::filter(
    hospital_attacks > 0,
    conflict_intensity == 0,
    n_territorial_events == 0
  ) %>%
  dplyr::select(grid_id, week, hospital_attacks, conflict_intensity, n_territorial_events)

nrow(hospital_only)
#share of hospital attack grid-weeks (without something else happening)
total_hospital_weeks <- panel_master %>%
  filter(hospital_attacks > 0) %>%
  nrow()

nrow(hospital_only) / total_hospital_weeks

#share of hospital attacks with conflict in grid-week
hospital_with_conflict <- panel_master %>%
  filter(
    hospital_attacks > 0,
    conflict_intensity > 0
  )
nrow(hospital_with_conflict)
nrow(hospital_only)

#share of hospital attacks and territorial changes in grid-week
hospital_with_territorial <- panel_master %>%
  filter(
    hospital_attacks > 0,
    n_territorial_events > 0
  )

nrow(hospital_with_territorial)
nrow(hospital_only)

#and share of both 
hospital_with_territorial_conflict <- panel_master %>%
  filter(
    hospital_attacks > 0,
    n_territorial_events > 0, 
    conflict_intensity > 0
  )

nrow(hospital_with_territorial_conflict)

#testing things to check whether merge was correct
sum(conflict_intensity_grid_week$conflict_intensity)
nrow(conflict_intensity_clean)
nrow(conflict_intensity_grid_week)

#creating lags - because attacks do not (or do??) occur time-delayed
panel_master_lag <- panel_master %>%
  arrange(grid_id, week) %>%
  group_by(grid_id) %>%
  mutate(
    conflict_intensity_lag1 = lag(conflict_intensity, 1),
    territorial_events_lag1 = lag(n_territorial_events, 1)
  ) %>%
  ungroup()
#sanity check
summary(panel_master_lag$conflict_intensity_lag1)
summary(panel_master_lag$territorial_events_lag1)

#check share: Lag share - Did something happen before (in grid), that might have influenced the attack? 
panel_master_lag %>%
  filter(hospital_attacks > 0) %>%
  summarise(
    share_conflict_lag1 = mean(conflict_intensity_lag1 > 0, na.rm = TRUE),
    share_territorial_lag1 = mean(territorial_events_lag1 > 0, na.rm = TRUE)
  )
#ergebnis: share_conflict_lag1 = 0.526; share_territorial_lag1 = 0.0977

#check share without lag: Contemporaneous Share - Does soemthing happen in the same grid-week?
panel_master %>%
  filter(hospital_attacks > 0) %>%
  summarise(
    share_conflict_now = mean(conflict_intensity > 0),
    share_territorial_now = mean(n_territorial_events > 0)
  )
#ergebnis:   share_conflict_now         0.5970149     share_territorial_now   0.1567164

#merge population grid on panel_master_lag ----
population_grid_clean <- population_grid %>%
  rename(grid_id = ID)

panel_master_lag <- panel_master_lag %>%
  left_join(population_grid_clean, by = "grid_id")

sum(is.na(panel_master_lag$population_2023))

#loose grids where populatin = 0 
panel_master_lag <- panel_master_lag %>%
  mutate(
    population_2023 = as.numeric(unlist(population_2023))
  )

panel_master_lag <- panel_master_lag %>%
  mutate(
    population = ifelse(is.na(population_2023) | is.nan(population_2023), 0, population_2023)
  )

panel_master_lag <- panel_master_lag %>%
  filter(population > 0)

#check
str(panel_master_lag$population)
table(panel_master_lag$population == 0)

panel_master_lag <- panel_master_lag %>%
mutate(log_population = log(population))


#without losing the zero population grids: ----
if (FALSE) {
panel_master_lag <- panel_master_lag %>%
  mutate(
    population_2023 = as.numeric(unlist(population_2023))
  ) %>%
  mutate(
    population = ifelse(is.na(population_2023) | is.nan(population_2023), 0, population_2023)
  )

# keep zero-population grids
# no filter(population > 0)

# check
str(panel_master_lag$population)
table(panel_master_lag$population == 0, useNA = "ifany")

# safe log transform
panel_master_lag <- panel_master_lag %>%
  mutate(log_population = log1p(population))
}





