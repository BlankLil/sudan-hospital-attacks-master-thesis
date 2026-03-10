#Descriptive Analysis -> plotting
#helper for saving pngs ----
save_png <- function(plot, name, w = 18, h = 12,
                     path = "output") {
  dir.create(path, showWarnings = FALSE)
  ggsave(
    filename = file.path(path, paste0(name, ".png")),
    plot = plot,
    width = w, height = h, units = "cm",
    dpi = 300,
    device = "png"
  )
}

#Plot: counting the attacks against hospitals ----
weekly_attacks <- sudan_hospital_attacks_data %>%
  mutate(week = floor_date(event_date, "week")) %>%
  group_by(week) %>%
  summarise(count = n())

# create plot
p1 <- ggplot(weekly_attacks, aes(x = week, y = count)) +
  geom_line() +
  labs(title = "Attacks on hospitals in Sudan",
       x = "Week",
       y = "Number of attacks") +
  theme_minimal(base_family = "Times New Roman")

save_png(p1, "p1")


#check with conflict intensity
weekly_attacks_2 <- conflict_intensity_clean %>%
  mutate(week = floor_date(event_date, "week")) %>%
  group_by(week) %>%
  summarise(count = n())

scale_factor <- max(weekly_attacks$count, na.rm = TRUE) /
  max(weekly_attacks_2$count, na.rm = TRUE)

p2 <- ggplot() +
  geom_line(
    data = weekly_attacks,
    aes(x = week, y = count),
    linewidth = 1
  ) +
  geom_line(
    data = weekly_attacks_2,
    aes(x = week, y = count * scale_factor),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_y_continuous(
    name = "Number of hospital attacks",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Conflict intensity"
    )
  ) +
  labs(
    title = "Hospital attacks and conflict intensity in Sudan",
    x = "Week"
  ) +
  theme_minimal(base_family = "Times New Roman")

print(p2)

save_png(p2, "p2")


#define baseline and escalation ans (incl. rubustness check) ----
#rename column count
weekly_attacks <- weekly_attacks%>%
rename(n_attacks = count )

threshold <- quantile(weekly_attacks$n_attacks, 0.75, na.rm = TRUE)

weekly_attacks <- weekly_attacks %>%
  mutate(
    escalation_75 = ifelse(n_attacks >= threshold, "Escalation", "Baseline")
  )
#what's our 75th percentile
p75 <- quantile(weekly_attacks$n_attacks, probs = 0.75, na.rm = TRUE)
p75

#check distribution
summary(weekly_attacks$n_attacks)
#histogram 
hist(weekly_attacks$n_attacks, breaks = 20)


#robustness check 80th percentile ----
p80 <- quantile(weekly_attacks$n_attacks, probs = 0.80, na.rm = TRUE)

weekly_attacks <- weekly_attacks %>%
  mutate(
    escalation_80 = ifelse(n_attacks >= p80, 1, 0)
  )
p80

#robustness check 90th percentile ----
p90 <- quantile(weekly_attacks$n_attacks, probs = 0.90, na.rm = TRUE)

weekly_attacks <- weekly_attacks %>%
  mutate(
    escalation_90 = ifelse(n_attacks >= p90, 1, 0)
  )
p90

#check with 60th percentile
p60 <- quantile(weekly_attacks$n_attacks, probs = 0.50, na.rm = TRUE)

weekly_attacks <- weekly_attacks %>%
  mutate(
    escalation_60 = ifelse(n_attacks >= p60, 1, 0)
  )
p60


#möglicher plot um die thresholds zu zeigen
ggplot(weekly_attacks, aes(x = week, y = n_attacks)) +
  geom_line() +
  geom_hline(yintercept = p75, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = p80, linetype = "dotted", color = "orange") +
  geom_hline(yintercept = p90, linetype = "dotdash", color = "red") +
  theme_minimal()


#plot the time periods
escalation_plot <- ggplot(weekly_attacks, aes(x = week, y = n_attacks)) +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(color = escalation_75),
    size = 2
  ) +
  scale_color_manual(
    values = c("Baseline" = "grey", "Escalation" = "red")
  ) +
  labs(
    x = "Week",
    y = "Number of attacks on hospitals",
    title = "Weekly attacks on hospitals in Sudan",
    color = ""
  ) +
  theme_minimal(base_family = "Times New Roman")

save_png(escalation_plot, "escalation_plot")

#identfiy escalation periods
escalation_weeks <- weekly_attacks %>%
  filter(escalation_75 == "Escalation")

escalation_weeks


#new try with 80 

# 2) set percentile threshold (80th)
p80 <- as.numeric(quantile(weekly_attacks$n_attacks, probs = 0.80, na.rm = TRUE))
p80

# If you want the threshold to be exactly 4 (discrete count rule),
# this makes the rule explicit and reproducible:
attack_threshold <- 4

# 3) classify weeks
weekly_attacks <- weekly_attacks %>%
  mutate(
    escalation_80 = ifelse(n_attacks >= attack_threshold, "Escalation", "Baseline")
  )

# 4) quick distribution check (optional)
summary(weekly_attacks$n_attacks)

# 5) plot weekly series + threshold + escalation marking
escalation_plot_80 <- ggplot(weekly_attacks, aes(x = week, y = n_attacks)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = attack_threshold, linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(color = escalation_80), size = 2) +
  scale_color_manual(values = c("Baseline" = "grey", "Escalation" = "red")) +
  labs(
    x = "Week",
    y = "Number of attacks on hospitals",
    title = "Weekly attacks on hospitals in Sudan",
    subtitle = "Escalation defined as weeks with ≥ 4 attacks (≈ 80th percentile)",
    color = ""
  ) +
  theme_minimal(base_family = "Times New Roman")

print(escalation_plot_80)
save_png(escalation_plot_80, "escalation_plot_80")

#plot cumulated attacks ----
cumulated_attacks <- ggplot(weekly_attacks, aes(x = n_attacks)) +
  stat_ecdf(geom = "step") +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Cumulative distribution of weekly hospital attacks",
    x = "Number of hospital attacks per week",
    y = "Cumulative share of weeks"
  ) +
  theme_minimal(base_family = "Times New Roman")

save_png(cumulated_attacks, "cumulated_attacks")

cumulated_attacks_zoomed <- ggplot(data = weekly_attacks, aes(x = n_attacks)) +
  stat_ecdf(geom = "step") +
  coord_cartesian(xlim = c(0, 10)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title = "Cumulative distribution of weekly hospital attacks (zoomed)",
    x = "Number of hospital attacks per week",
    y = "Cumulative share of weeks"
  ) +
  theme_minimal(base_family = "Times New Roman")

save_png(cumulated_attacks_zoomed, "cumulated_attacks_zoomed")



#Plotting InsecurityInsight Data -- put attacks on hospitals on map and show number of attacks on the hospitals ----

#sum(attacks_per_location_sf$n_attacks > 1) -- brauchen wir das? 

#plot repeated attacks on hospital on map (data: attacks_locations_sf)


key_places <- data.frame(
  name = c("Khartoum", "al-Fashir", "Nyala", "El Geneina"),
  lon = c(32.5599, 25.3500, 24.8800, 22.4400),
  lat = c(15.5007, 13.6300, 12.0500, 13.4500)
)

key_places_sf <- st_as_sf(key_places,
                          coords = c("lon", "lat"),
                          crs = 4326)

# project to same CRS as your map
key_places_sf <- st_transform(key_places_sf, st_crs(sudan_meters))

repeated_attacks_plot <- ggplot() +
  
  geom_sf(data = sudan_meters, fill = NA, color = "black", size = 0.6) +
  
  geom_sf(
    data = attacks_locations_sf,
    aes(color = n_attacks),
    size = 1.3,
    alpha = 0.85
  ) +
  
  # Key place markers (white circle with black border)
  geom_sf(
    data = key_places_sf,
    shape = 21,
    fill = NA,
    color = "grey",
    size = 1.0,
    stroke = 0.5
  ) +
  
  # Labels for key places
  geom_sf_text(
    data = key_places_sf,
    aes(label = name),
    size = 3,
    nudge_y = 60000,
    family = "Times New Roman"
  ) +
  
  scale_color_viridis_c(
    option = "rocket",
    direction = -1,
    name = "Number of attacks"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  
  labs(
    title = "Repeated attacks on hospitals in Sudan",
    subtitle = "Colors indicate number of attacks at the same location",
    caption = "Yellow: at least 1 attack, Purple: over 40 attacks; Data source: Insecurity Insight"
  )

print(repeated_attacks_plot)

save_png(repeated_attacks_plot, "repeated_attacks_plot")


#no plot but table (see how often several hospitals have been attacked)

attacks_per_location <- attacks_coords %>%
  st_drop_geometry() %>%
  dplyr::count(x, y, name = "n_attacks") %>%
  dplyr::arrange(desc(n_attacks))

head(attacks_per_location, 25)
summary(attacks_per_location$n_attacks)
mean(attacks_per_location$n_attacks)
median(attacks_per_location$n_attacks)

#plot weekly attacks in that escalation grid ----

# point at the repeated-attack coordinate
pt47 <- st_as_sf(
  data.frame(x = 3617883, y = 1746892),
  coords = c("x","y"),
  crs = st_crs(sudan_meters)   # same metric CRS
)

# get the grid cell (and its id) that contains the point
grid47 <- st_join(pt47, grid["grid_id"], join = st_within)

grid_id_47 <- grid47$grid_id[1]
grid_id_47


grid404_plot <- panel_master_lag %>%
  filter(grid_id == grid_id_47) %>%
  ggplot(aes(x = week, y = hospital_attacks)) +
  geom_line() +
  labs(
    title = "Weekly hospital attacks in the grid containing the most repeated coordinates",
    subtitle = paste0("Grid: ", grid_id_47, " | Location: (3617883, 1746892)"),
    x = "Week",
    y = "Number of hospital attacks"
  ) +
  theme_minimal(base_family = "Times New Roman")

save_png(grid404_plot, "grid404_plot")


#Daily attacks on hospitals by RSF and SAF ----
# plot
sudan_hospital_attacks_perp <- sudan_hospital_attacks_data %>%
  filter(`Reported Perpetrator Name` %in% c("Rapid Support Forces", "Sudanese Armed Forces"))

daily_counts <- sudan_hospital_attacks_perp %>%
  count(event_date, `Reported Perpetrator Name`)
#hier müsssen noch Farben geändert werden!!!!
p5 <- ggplot(daily_counts,
       aes(x = event_date, 
           y = n, 
           color = `Reported Perpetrator Name`)) +
  geom_line(size = 1) +
  labs(
    title = "Daily Attacks on hospitals: RSF vs SAF",
    x = "Date",
    y = "Number of Attacks",
    scale_color_manual(
      name = "Perpetrator",
      values = c(
        "SAF" = "#B22222",
        "RSF" = "#1F4E79",
        "RSF + SAF" = "#7A1FA2" )
  )) +
  theme_minimal(base_family = "Times New Roman")

print(p5)
save_png(p5, "p5")
#Who attacks where and what 
#filter for reported perpetrator and geometry
sudan_hospital_attacks_actor_sf <- sudan_hospital_attacks_sf %>%
  dplyr::select(
    actor = `Reported Perpetrator Name`,
    geometry
  )

#exclude NAs and NIs
sudan_hospital_attacks_actor_sf_filtered <- sudan_hospital_attacks_actor_sf %>%
  mutate(
    actor_clean = case_when(
      actor == "Sudanese Armed Forces" ~ "SAF",
      actor == "Rapid Support Forces" ~ "RSF",
      actor %in% c(
        "Rapid Support Forces, Sudanese Armed Forces",
        "Sudanese Armed Forces, Rapid Support Forces"
      ) ~ "RSF + SAF",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(actor_clean))

actor_attacks_plot_color_new <- ggplot() +
  geom_sf(
    data = sudan_meters,
    fill = NA,
    color = "black",
    size = 0.6
  ) +
  geom_sf(
    data = sudan_hospital_attacks_actor_sf_filtered,
    aes(color = actor_clean),
    size = 1.3,
    alpha = 0.85
  ) +
  scale_color_manual(
    name = "Reported perpetrator",
    values = c(
      "SAF" = "#B22222",
      "RSF" = "#1F4E79",
      "RSF + SAF" = "#7A1FA2" 
    ),
    breaks = c("SAF", "RSF", "RSF + SAF")
  ) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Attacks on hospitals in Sudan by conflict party",
    subtitle = "Red: SAF, Blue: RSF, Purple: jointly reported (RSF + SAF)",
    caption = "Data source: Insecurity Insight"
  )


print(actor_attacks_plot_color_new)

save_png(actor_attacks_plot_color_new, "actor_attacks_plot_color_new")



save_png(actor_attacks_plot_color_new, "actor_attacks_plot_color_new")

#mit grauen punkten für NoInformations ----
actor_attacks_plot_color <- ggplot() +
  geom_sf(
    data = sudan_meters,
    fill = NA,
    color = "black",
    size = 0.6
  ) +
  geom_sf(
    data = sudan_hospital_attacks_actor_sf,
    aes(color = actor),
    size = 1.3,
    alpha = 0.85
  ) +
  scale_color_manual(
    name = "Perpetrator",
    values = c(
      "Sudanese Armed Forces" = "#C00000",  
      "Rapid Support Forces" = "#1F4E79"    
    )
  ) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Attacks on hospitals in Sudan by conflict party",
    subtitle = "Red: SAF, Blue: RSF",
    caption = "Data source: Insecurity Insight"
  )

print(actor_attacks_plot_color)

save_png(actor_attacks_plot_color, "actor_attacks_plot_color")

# Weapons and actors ----
#which weapons are used? ----
colnames(sudan_hospital_attacks_sf)

#which weapons how often?
weapons_overall <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  dplyr::count(`Weapon Carried/Used`, name = "n_attacks") %>%
  dplyr::arrange(desc(n_attacks))

weapons_overall
#in percentage
weapons_overall <- weapons_overall %>%
  dplyr::mutate(
    share = round(n_attacks / sum(n_attacks) * 100, 1)
  )

weapons_overall

#check the number of weapons per actor 
actor_weapon_counts_overall <- sudan_hospital_attacks_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    actor_group = dplyr::case_when(
      grepl("Rapid Support Forces|RSF", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "RSF",
      grepl("Sudanese Armed Forces|SAF|Sudan Armed Forces", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "SAF",
      grepl("Police|Security", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "State (other)",
      grepl("Unknown|Unidentified", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "Unknown",
      TRUE ~ "Other armed actors",
      ),
    weapon_overall = dplyr::coalesce(`Weapon Carried/Used`, "Unknown")
  ) %>%
  dplyr::count(actor_group, weapon_overall, name = "n_attacks") %>%
  dplyr::arrange(actor_group, dplyr::desc(n_attacks))

actor_weapon_counts_overall

top5_by_actor <- actor_weapon_counts_overall %>%
  dplyr::group_by(actor_group) %>%
  dplyr::slice_max(n_attacks, n = 5, with_ties = FALSE) %>%
  dplyr::ungroup()

top5_by_actor


#group weapons, to see which actor uses which weapons
weapons_grouped <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  dplyr::mutate(
    weapon_group = dplyr::case_when(
      
      `Weapon Carried/Used` %in% c(
        "Shelling", "Artillery", "Mortar", "Rocket",
        "Missile", "Unspecified Explosive"
      ) ~ "Explosives / Heavy fire",
      
      `Weapon Carried/Used` %in% c(
        "Aerial Bomb: Plane", "Aerial Bomb: Drone",
        "Aerial Bomb: Missile", "Aerial Bomb: Plane, Artillery"
      ) ~ "Air-delivered weapons",
      
      `Weapon Carried/Used` == "Firearms" ~ "Firearms",
      
      `Weapon Carried/Used` == "Arson" ~ "Arson",
      
      `Weapon Carried/Used` == "Fist and Foot" ~ "Physical force",
      
      TRUE ~ "Unknown"
    )
  ) %>%
  dplyr::count(weapon_group, name = "n_attacks") %>%
  dplyr::mutate(
    share = round(n_attacks / sum(n_attacks) * 100, 1)
  ) %>%
  dplyr::arrange(desc(n_attacks))

weapons_grouped

#Add type of actor (RSF&SAF x type of weapon)----
attacks_actor_weapon <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  dplyr::mutate(
    
    actor_group = dplyr::case_when(
      grepl("Rapid Support Forces|RSF", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "RSF",
      grepl("Sudanese Armed Forces|SAF|Sudan Armed Forces", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "SAF",
      grepl("Police|Security", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "State (other)",
      grepl("Unknown|Unidentified", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "Unknown",
      TRUE ~ "Other armed actors"
    ),
    
    weapon_group = dplyr::case_when(
      `Weapon Carried/Used` %in% c(
        "Shelling", "Artillery", "Mortar", "Rocket",
        "Missile", "Unspecified Explosive"
      ) ~ "Explosives / Heavy fire",
      
      `Weapon Carried/Used` %in% c(
        "Aerial Bomb: Plane", "Aerial Bomb: Drone",
        "Aerial Bomb: Missile", "Aerial Bomb: Plane, Artillery"
      ) ~ "Air-delivered weapons",
      
      `Weapon Carried/Used` == "Firearms" ~ "Firearms",
      `Weapon Carried/Used` == "Arson" ~ "Arson",
      `Weapon Carried/Used` == "Fist and Foot" ~ "Physical force",
      
      TRUE ~ "Unknown"
    )
  )

actor_weapon_counts <- attacks_actor_weapon %>%
  dplyr::count(actor_group, weapon_group, name = "n_attacks") %>%
  dplyr::arrange(desc(n_attacks))

actor_weapon_counts

#Compare RSF and SAF, wepaon shares, incl. heatmap
actor_weapon_shares <- actor_weapon_counts %>%
  dplyr::group_by(actor_group) %>%
  dplyr::mutate(
    share_actor = round(n_attacks / sum(n_attacks) * 100, 1)
  ) %>%
  dplyr::ungroup()

actor_weapon_shares
print(actor_weapon_shares, n = Inf, width = Inf)

#heatmap for who x what

heatmap_weapon_shares <- ggplot(actor_weapon_shares,
       aes(x = weapon_group, y = actor_group, fill = share_actor)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    option = "rocket",
    direction = -1,
    name = "Share of attacks (%)"
  ) +
  labs(
    x = "Weapon category",
    y = "Actor",
    title = "Weapons used in attacks on hospitals by actor"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_png(heatmap_weapon_shares, "heatmap_weapon_shares")

#time periods of escalations of attacks
sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  select(Date) %>%
  head()

#aggregate attacks per week 
attacks_weekly <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  mutate(
    week = floor_date(Date, unit = "week", week_start = 1)
  ) %>%
  count(week, name = "n_attacks")

#escalation and baseline
#Show: actors and attacks in plot ----
sudan_hospital_attacks_sf %>% st_drop_geometry () %>% count (`Reported Perpetrator Name`, sort = TRUE)

attacks_weekly_actor <- sudan_hospital_attacks_sf %>%
  st_drop_geometry() %>%
  mutate(
    week = floor_date(Date, unit = "week", week_start = 1),
    actor_group = case_when(
      grepl("Rapid Support Forces",`Reported Perpetrator Name` , ignore.case = TRUE) ~ "RSF",
      grepl("Sudanese Armed Forces", `Reported Perpetrator Name`, ignore.case = TRUE) ~ "SAF",
      TRUE ~ "Other"
    )
  ) %>%
  count(week, actor_group)
#plot this
plot3 <- ggplot(
  attacks_weekly_actor,
  aes(x = week, y = n, color = actor_group)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c(
      "RSF"   = "#7B1E1E",  # dark red
      "SAF"   = "#1F4E79",  # dark blue
      "Other" = "grey60"    # grey
    ),
    breaks = c("RSF", "SAF", "Other")
  ) +
  labs(
    x = "Week",
    y = "Number of attacks",
    color = "Actor"
  ) +
  theme_minimal(base_family = "Times New Roman")

save_png(plot3, "plot3")
