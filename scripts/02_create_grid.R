#plot Sudan and create grids
#Plotting Sudan (as a whole, without grids)----
Sudan <- Geo_data1

print(Sudan)
plot(st_geometry(Sudan))

st_crs(Sudan)
Sudan <- st_set_crs(Sudan, 4326)
st_crs(Sudan)
sudan_meters <- st_transform(Sudan, 3857)
st_crs(sudan_meters)
st_bbox(sudan_meters)

#we're making grids now---- 
grid_size <- 30000   # 30 km

grid <- st_make_grid(
  sudan_meters,
  cellsize = grid_size,
  square = TRUE
)

grid <- st_sf(
  grid_id = 1:length(grid),
  geometry = grid
)

grid_sudan <- st_intersection(grid, sudan_meters)

nrow(grid_sudan) #There are 2424 grids

#create master-grid
grid_sf <- grid_sudan %>%
  mutate(grid_id = row_number())

#plotting of grids
spatial_unit_of_analysis <- ggplot() +
  geom_sf(data = grid_sudan, fill = NA, color = "grey70", size = 0.15) +
  geom_sf(data = sudan_meters, fill = NA, color = "black", size = 0.6) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "30 × 30 km Grid Cells in Sudan",
    subtitle = "Spatial units of analysis",
    caption = "Source: Author's own grid based on GADM boundary"
  )

print(spatial_unit_of_analysis)

ggsave(
  filename = "spatial_unit_of_analysis",
  plot = spatial_unit_of_analysis,
  width = 8,
  height = 6,
  dpi = 300
)

