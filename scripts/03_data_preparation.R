#Data Preparation -> descriptive Analysis

#Insecurity Insight ----
#Filter for Sudan only
sudan_HIA_data <- subset(InsecurityInsight_data, Country == "Sudan")
# put date into date format
sudan_HIA_data$event_date <- as.Date(sudan_HIA_data$Date, format = "%Y-%m-%d")
#filter events starting from 2023
sudan_HIA_data_2023 <- sudan_HIA_data %>%
  filter(event_date >= as.Date("2023-01-01"))
#check data
str(sudan_HIA_data)
colnames(sudan_HIA_data)

#filtering attacks on hospitals (health facilities) #sudan_hospital_attacks_data is dataset we're working with
sudan_hospital_attacks_data <- sudan_HIA_data_2023 %>%
  filter(
    `Number of Attacks on Health Facilities Reporting Destruction` > 0 |
      `Number of Attacks on Health Facilities Reporting Damaged` > 0 |
      `Forceful Entry into Health Facility` > 0 |
      `Occupation of Health Facility` > 0
  )

#filter for events with geo precision
sudan_hospital_attacks_data <- sudan_hospital_attacks_data %>%
  dplyr::filter(!(`Geo Precision` == '(4) Province, State, Governorate'| `Geo Precision` == ' (6) Country' ))


#bring it in correct coordinate reference system (CRS) form 
sudan_hospital_attacks_sf <- st_as_sf(
  sudan_hospital_attacks_data,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

sudan_hospital_attacks_sf <- st_transform(
  sudan_hospital_attacks_sf,
  st_crs(sudan_meters)
)

attacks_coords <- sudan_hospital_attacks_sf %>%
  dplyr::mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  )

attacks_by_location <- attacks_coords %>%
  st_drop_geometry() %>%
  dplyr::count(x, y, name = "n_attacks")

attacks_locations_sf <- st_as_sf(
  attacks_by_location,
  coords = c("x", "y"),
  crs = st_crs(sudan_meters)
)

#prepare Insecurity Insight Data for grid week ---- 
  sudan_hospital_attacks_sf <- sudan_hospital_attacks_sf %>%
  mutate(
    event_date = as.Date(event_date),
    week = floor_date(event_date, "week")
  )
#sanity checks
summary(sudan_hospital_attacks_sf$event_date)
st_crs(sudan_hospital_attacks_sf)
st_crs(grid_sudan)

#map attacks on grids
#first bring data to common CRS
sudan_hospital_attacks_sf <- st_transform(
  sudan_hospital_attacks_sf,
  st_crs(grid_sudan)
)

hospital_attacks_grid <- st_join(
  sudan_hospital_attacks_sf,
  grid_sudan,
  left = FALSE
)

hospital_attacks_grid_week <- hospital_attacks_grid %>%
  st_drop_geometry() %>%
  count(grid_id, week, name = "hospital_attacks")

#Filter for RSF and SAF - attacks_by_actor new dataset 
colnames(sudan_hospital_attacks_sf)

attacks_by_actor <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  dplyr::count(`Reported Perpetrator Name`, name = "n_attacks") %>%
  dplyr::arrange(desc(n_attacks))

#see how many attacks per actor 
attacks_by_actor

#see percentage of attacks per actor
attacks_by_actor <- attacks_by_actor %>%
  dplyr::mutate(
    share = round(n_attacks / sum(n_attacks) * 100, 1)
  )
attacks_by_actor

#WHO Hospital location data (not precise enough, exlude from analysis) ----
location_hospital_data <- subset(WHO_Hospital_data, Country == "Sudan")
#is na? 
sum(is.na(location_hospital_data$Long) | 
      is.na(location_hospital_data$Lat))

location_hospital_data <- location_hospital_data %>%
  dplyr::filter(
    !is.na(Long),
    !is.na(Lat)
  )

#hospitals in geometric unit 
hospitals_sf <- st_as_sf(
  location_hospital_data,
  coords = c("Long", "Lat"),
  crs = 4326   # WGS84
)

#ACLED territorial changes ----
territorial_change_data <- ACLED_data

str(territorial_change_data)
colnames(territorial_change_data)


#filter for events with geo and time precision lower than 3
territorial_change_data <- territorial_change_data %>%
  dplyr::filter(!(geo_precision == 3 | time_precision == 3))

#check, what kinds of sub_event are in the data
territorial_change_data %>% count(sub_event_type, sort =TRUE)
#filter for the territorial changes
territorial_events <- territorial_change_data %>%
  filter(sub_event_type %in% c(
    "Government regains territory",
    "Non-state actor overtakes territory", 
    "Non-violent transfer of territory"
  ))

territorial_events_sf <- territorial_events %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(grid_sudan))

#ACLED for: prepare data for conflict intensity ----
conflict_intensity_data <- ConflictIntensity_data

str(conflict_intensity_data)
#check the disorder_type, and event_type
conflict_intensity_data %>% count(disorder_type, sort =TRUE)

conflict_intensity_data %>% count(event_type, sort =TRUE)

#filter for relevant events and mutate dates for further calculation
conflict_intensity_clean <- conflict_intensity_data %>%
  filter(disorder_type == "Political violence") %>%
  mutate(
    event_date = as.Date(event_date),
    week = floor_date(event_date, "week")
  ) %>%
  filter(event_date >= as.Date("2023-01-01"))

#sanity check
nrow(conflict_intensity_clean)
summary(conflict_intensity_clean$event_date)
table(conflict_intensity_clean$event_type)

#lose the events where geoprecision and time precision has value 3
conflict_intensity_clean <- conflict_intensity_clean %>%
  dplyr::filter(!(geo_precision == 3 | time_precision == 3))

#check if entries with value 3 is gone 
sum(conflict_intensity_clean$geo_precision == 3, na.rm = TRUE)
sum(conflict_intensity_clean$time_precision == 3, na.rm = TRUE)


#mutate in sf 
conflict_intensity_sf <- conflict_intensity_clean %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(grid_sudan))
#and map on grids
conflict_grid <- st_join(
  conflict_intensity_sf,
  grid_sudan,
  left = FALSE
)
#and aggregate conflict intensity per grid week 
conflict_intensity_grid_week <- conflict_grid %>%
  st_drop_geometry() %>%
  count(grid_id, week, name = "conflict_intensity")

summary(conflict_intensity_grid_week$conflict_intensity)


#world population data ----
#prepare WorldPop Data for control variable ----

pop_raster
# Check: grid_id cannot be nummerized again
stopifnot("grid_id" %in% names(grid_sudan))

# project pop_raster in CRS of grid_sudan
pop_raster_proj <- project(pop_raster, st_crs(grid_sudan)$wkt)

# grid_sudan -> SpatVector
grid_vect <- vect(grid_sudan)

# extract populaton per grid (sum)
population_grid <- extract(
  pop_raster_proj,
  grid_vect,
  fun = sum,
  na.rm = TRUE
)

population_grid
#rename column
population_grid <- population_grid %>%
  rename(population_2023 = global_pop_2023_CN_1km_R2025A_v1)

names(population_grid)
