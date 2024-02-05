library(flexdashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(readxl)

# Load data files
# ===============

# Load QOF Prevalence
df_qof_prev <- read.csv('D:/Data/NHSD/QOF/2023/PREVALENCE_2223.csv') %>% filter(GROUP_CODE %in% c('OB','HF'))

# Load QOF Achievement
df_qof_achv <- read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_EAST_OF_ENGLAND_2223.csv') %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_LONDON_2223.csv')) %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_MIDLANDS_2223.csv')) %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_NORTH_EAST_AND_YORKSHIRE_2223.csv')) %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_NORTH_WEST_2223.csv')) %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_SOUTH_EAST_2223.csv')) %>%
  bind_rows(read.csv('D:/Data/NHSD/QOF/2023/ACHIEVEMENT_SOUTH_WEST_2223.csv')) %>%
  filter(INDICATOR_CODE == 'SMOK004' & MEASURE %in% c('NUMERATOR','DENOMINATOR','PCAS')) %>%
  select(PRACTICE_CODE, MEASURE, VALUE)

df_qof_achv <- df_qof_achv %>% 
  pivot_wider(names_from = 'MEASURE', values_from = 'VALUE') %>%
  mutate(DENOMINATOR = DENOMINATOR + PCAS, .keep = 'unused')

# Load QOF Organisational Mapping
df_org_map <- read.csv('D:/Data/NHSD/QOF/2023/MAPPING_NHS_GEOGRAPHIES_2223.csv')

# Load GP Registration Data
df_reg_popn <- read.csv('D:/Data/NHSD/GPREGLSOA/20240101/gp-reg-pat-prac-lsoa-all.csv') %>% 
  mutate(PRACTICE_CODE, LSOA11CD = LSOA_CODE, REG_POPN = NUMBER_OF_PATIENTS, .keep = 'none')

# Load IMD 2019 Data
df_imd19 <- read.csv('D:/Data/GOV.UK/IMD/2019/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select(1, 6) %>%
  rename_with(.fn = ~c('LSOA11CD', 'IMD_SCORE'))

# Load ePCN Data
df_pcn_members <- read_excel(path = 'D:/Data/NHSD/EPCN/20240129/ePCN.xlsx', sheet = 'PCNDetails') %>%
  select(1, 2, 3, 6, 12) %>% 
  rename_with(.fn = ~c('PCN_CODE', 'PCN_NAME', 'SUBICB', 'CLOSE_DATE', 'POSTCODE')) %>%
  filter(!is.na(CLOSE_DATE))

# Load Postcode Data
df_pcd <- read.csv('D:/Data/OpenGeography/Lookups/PCD/20231122/ONSPD_NOV_2023_UK.csv') %>% 
  select(3, 12, 13, 43, 44) %>%
  rename_with(.fn = ~c('pcds', 'easting', 'northing', 'latitude', 'longitude'))

# Load ICB Shapefile
sf_icb <- st_read(dsn = 'D:/Data/OpenGeography/Shapefiles/ICB22', layer = 'ICB22') %>%
  st_transform(crs = 4326)

# Load LSOA 2011 Shapefile
sf_lsoa11 <- st_read(dsn = 'D:/Data/OpenGeography/Shapefiles/LSOA11', layer = 'LSOA11') %>%
  st_transform(crs = 4326) %>%
  filter(grepl('^E', LSOA11CD))

df_map_data <- df_qof_prev %>% 
  filter(GROUP_CODE =='HF') %>%
  left_join(df_org_map, by = 'PRACTICE_CODE') %>%
  select(ICB_ODS_CODE, ICB_ONS_CODE, ICB_NAME, REGISTER, PRACTICE_LIST_SIZE) %>%
  group_by(ICB_ODS_CODE, ICB_ONS_CODE, ICB_NAME) %>%
  summarise(REGISTER = sum(REGISTER),
            PRACTICE_LIST_SIZE = sum(PRACTICE_LIST_SIZE),
            .groups = 'keep') %>%
  ungroup() %>%
  mutate(PREVALENCE = REGISTER / PRACTICE_LIST_SIZE * 100,
         DISTANCE_TO_TARGET = 1.6 - PREVALENCE)
  

palPrevalence <- colorNumeric(palette = 'viridis', domain = pretty(df_map_data$PREVALENCE, n = 10))
palDistanceToTarget <- colorNumeric(palette = 'RdYlGn', domain = pretty(df_map_data$DISTANCE_TO_TARGET, n = 10), reverse = TRUE)
colorRampPalette

# Map 1: Prevalence of HF by ICB

map01 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = sf_icb %>% 
                left_join(df_map_data, by = c('ICB22CD' = 'ICB_ONS_CODE')),
              weight = 1.5,
              fillColor = ~palPrevalence(PREVALENCE),
              fillOpacity = 0.8,
              popup = ~sprintf('[%s] - %s<br>Heart Failure Prevalence = %.2f%%<br>[%d/%d]',
                                ICB_ODS_CODE, ICB_NAME, PREVALENCE, REGISTER, PRACTICE_LIST_SIZE),
              group = 'PREVALENCE') %>%
  addLegend(position = 'bottomright', 
            pal = palPrevalence, 
            values = pretty(df_map_data$PREVALENCE, n = 10),
            opacity = 0.8,
            group = 'PREVALENCE') %>%
  addPolygons(data = sf_icb %>% 
                left_join(df_map_data, by = c('ICB22CD' = 'ICB_ONS_CODE')),
              weight = 1.5,
              fillColor = ~palDistanceToTarget(DISTANCE_TO_TARGET),
              fillOpacity = 0.8,
              popup = ~sprintf('[%s] - %s<br>Heart Failure Prevalence = %.2f%%<br>[%d/%d]<br>Distance to Target = %.2f%%',
                               ICB_ODS_CODE, ICB_NAME, PREVALENCE, REGISTER, PRACTICE_LIST_SIZE, DISTANCE_TO_TARGET),
              group = 'DISTANCE_TO_TARGET') %>%
  addLegend(position = 'bottomright', 
            pal = palDistanceToTarget, 
            values = pretty(df_map_data$DISTANCE_TO_TARGET, n = 10),
            opacity = 0.8,
            group = 'DISTANCE_TO_TARGET') %>%
  addLayersControl(overlayGroups = c('PREVALENCE', 'DISTANCE_TO_TARGET'), 
                   options = layersControlOptions(collapsed = FALSE))
map01


df_qof_prev %>% 
  filter(GROUP_CODE =='HF') %>%
  left_join(df_org_map, by = 'PRACTICE_CODE') %>%
  select(ICB_ODS_CODE, ICB_ONS_CODE, ICB_NAME, REGISTER, PRACTICE_LIST_SIZE) %>%
  group_by(ICB_ODS_CODE, ICB_ONS_CODE, ICB_NAME) %>%
  summarise(REGISTER = sum(REGISTER),
            PRACTICE_LIST_SIZE = sum(PRACTICE_LIST_SIZE),
            .groups = 'keep') %>%
  ungroup() %>%
  mutate(PREVALENCE = REGISTER / PRACTICE_LIST_SIZE)


  