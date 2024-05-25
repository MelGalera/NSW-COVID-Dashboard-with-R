### This script is for creating maps and extracting data for maps


# page1 part4 extracting map shapes-LHD

# Extract shape 
#nswlgas <- read_sf("NOV21_NSW_LGA_POLYGON_shp/nsw_lga.shp") -- update
nswlgas <- sf::st_read("NOV21_NSW_LGA_POLYGON_shp/nsw_lga.shp")
nswlgas <- sf::st_transform(nswlgas, 4326)

# 1. Extracting shape files for LHD

# LGA to LHD mapping derived from pages at https://www.healthstats.nsw.gov.au/#/locations
lga2lhd <- read_csv("./data/NSW_LHD_LGA_names.csv")

# join LHD mapping to LGA boundaries by LGA name
nswlgas <- nswlgas %>%
  left_join(lga2lhd, by = c("ABB_NAME" = "LGA_name")) %>%
  filter(!is.na(LHD_name))

# group by LHD and remove interior boundaries of the union
nswlhds <- nswlgas %>% 
  group_by(LHD_name) %>% 
  summarise(geometry = st_union(geometry))

# reduce the detail of the boundaries to a more manageable level
nswlhds_simplified <- ms_simplify(nswlhds, keep = 0.1, keep_shapes = TRUE)


# Combine df_location_map with the polygon map
nswlhds_simplified_polys <- left_join(nswlhds_simplified, df_location_map)



# page1 part5 extracting map shapes-LGA

# 2. Extracting shape files for LGA

nswlgas_simplified <- ms_simplify(nswlgas, keep = 0.1, keep_shapes = TRUE)
nswlgas_simplified_2 <- nswlgas_simplified %>% 
  select(ABB_NAME, geometry)

nswlgas_simplified_2polys <- left_join(nswlgas_simplified_2, df_location_lga_comb, 
                                       by = c("ABB_NAME" = "LGA_NAME")) %>% 
  replace(is.na(.), 0)



# page1 part6 creating tables
# Create LHS table for rendering
lhd_cases_table <- df_location_map %>%
  select(LHD_name, cases_lastday, lhd_cases_thisweek, cumulated_cases) %>% 
  arrange(desc(cumulated_cases))
# Rename LHD column names
lhd_cases_table <- rename(lhd_cases_table, c(LHD = LHD_name, "Reporting Day" = cases_lastday,
                                             "Reporting Week" = lhd_cases_thisweek, "Total Cases" = cumulated_cases))


# Create LGA table for rendering
lga_cases_table <- df_location_lga_comb %>% 
  select(LGA_NAME, cases_lastday, lga_cases_thisweek, cumulated_cases) %>% 
  arrange(desc(cumulated_cases))

# Rename LGA column names
lga_cases_table <- rename(lga_cases_table, c(LGA = LGA_NAME, "Reporting Day" = cases_lastday,
                                             "Reporting Week" = lga_cases_thisweek, "Total Cases" = cumulated_cases))

