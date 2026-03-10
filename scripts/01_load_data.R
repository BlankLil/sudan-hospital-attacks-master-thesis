#loaddata
ACLED_data <- read.csv("/Users/Lilli/Documents/R/MasterThesis/ACLED Data_2025-11-04.csv")
InsecurityInsight_data <- read_excel (("/Users/Lilli/Documents/R/MasterThesis/2016-2025-attacks-on-health-care-incident-data.xlsx"))
ConflictIntensity_data <- read.csv("/Users/Lilli/Documents/R/MasterThesis/ACLED Data_2025-11-04_ALL-ATTACKS.csv")
WHO_Hospital_data <- read_excel (("/Users/Lilli/Documents/R/MasterThesis/who-cds-gmp-2019-01-eng.xlsx"))
Geo_data1 <- st_read(("/Users/Lilli/Documents/R/MasterThesis/gadm41_SDN_0.shp")) 
pop_raster <- rast("/Users/Lilli/Documents/R/MasterThesis/sdn_pop_2023_CN_1km_R2025A_UA_v1.tif")

