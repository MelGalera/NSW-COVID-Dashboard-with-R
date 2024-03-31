### This script is for getting NSW covid cases data and creating source datasets


# If accessing the latest version of data from data.nsw.gov.au
# DATA1: NSW COVID-19 cases by location
by_location_url <- ("https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/5d63b527-e2b8-4c42-ad6f-677f14433520/download/confirmed_cases_table1_location_agg.csv")

# DATA2: NSW COVID-19 cases by age range
by_agegroup_url <- ("https://data.nsw.gov.au/data/dataset/3dc5dc39-40b4-4ee9-8ec6-2d862a916dcf/resource/4b03bc25-ab4b-46c0-bb3e-0c839c9915c5/download/confirmed_cases_table2_age_group_agg.csv")


# Create location dataset with latest data
# df_location <- read.csv(url(by_location_url))
# Create agegroup dataset with latest data
# df_agegroup <- read.csv(url(by_agegroup_url))

# Note: the data repository has stopped updating with the last update on 31/10/2023
# Hence, the latest aggregated data were downloaded and can be loaded as below:

# Create location dataset with latest data
df_location <- read.csv("./data/confirmed_cases_table1_location_agg.csv")
# Create agegroup dataset with latest data
df_agegroup <- read.csv("./data/confirmed_cases_table2_age_group_agg.csv")


## PAGE 1 - CASE SUMMARIES

# page1 part1 all_nsw prep
# All NSW Calculation
df_location_nsw <- df_location %>% 
  mutate(notification_date = as.Date(notification_date)) %>% 
  select(notification_date, lhd_2010_name, confirmed_cases_count)

df_location_nsw3 <- df_location_nsw %>% 
  group_by(notification_date) %>% 
  summarise(cases = sum(confirmed_cases_count))

# Define the last recorded date
max_notif_date <- max(df_location_nsw$notification_date)

# Create lhd groupings
metro_lhd <- c("Central Coast", "Northern Sydney", "South Western Sydney", "South Eastern Sydney", 
               "Sydney", "Western Sydney", "Nepean Blue Mountains", "Illawarra Shoalhaven")

rural_lhd <- c("Hunter New England", "Murrumbidgee", "Western NSW", "Northern NSW", 
               "Southern NSW", "Mid North Coast", "Far West")



# page1 part2 info in map-LHD
# Create source dataset for cumulated cases per LHD (and new column for identifying metro or RR lhd)
df_location_mapCS <- df_location_nsw %>% 
  select(lhd_2010_name, confirmed_cases_count) %>% 
  group_by(lhd_2010_name) %>% 
  summarise(cumulated_cases = sum(confirmed_cases_count)) %>% 
  mutate(lhd_loc = case_when(lhd_2010_name %in% metro_lhd ~ "Metropolitan LHD",
                             lhd_2010_name %in% rural_lhd ~ "Rural and Regional NSW LHD",
                             lhd_2010_name == "Correctional settings" ~ "Correctional settings",
                             lhd_2010_name == "Hotel Quarantine" ~ "Hotel Quarantine")) %>% 
  select(lhd_loc, lhd_2010_name, cumulated_cases) %>% 
  arrange(lhd_loc, lhd_2010_name)


# Create source dataset for last-date cases per LHD
df_location_lastday <- df_location_nsw %>% 
  filter(notification_date == max_notif_date) %>% 
  select(lhd_2010_name, confirmed_cases_count) %>% 
  group_by(lhd_2010_name) %>% 
  summarise(cases_lastday = sum(confirmed_cases_count)) %>% 
  arrange(lhd_2010_name)

# Create source dataset for this week cases per LHD
df_location_thisweek <- df_location_nsw %>% 
  filter(notification_date > (max(notification_date) - 7) & 
           notification_date <= (max(notification_date))) %>% 
  select(lhd_2010_name, confirmed_cases_count) %>% 
  group_by(lhd_2010_name) %>% 
  summarise(lhd_cases_thisweek =sum(confirmed_cases_count)) %>% 
  arrange(lhd_2010_name)

# Combine LHD datasets
df_location_map <- df_location_mapCS %>% 
  filter(lhd_loc == "Metropolitan LHD" | lhd_loc == "Rural and Regional NSW LHD") %>% 
  left_join(df_location_lastday) %>% 
  left_join((df_location_thisweek)) %>% 
  rename(LHD_name = lhd_2010_name)


# page1 part3 info in map-LGA
# Create source dataset with cumulated cases per LGA
df_location_lga_CS <- df_location %>% 
  select(lga_name19, confirmed_cases_count) %>% 
  group_by(lga_name19) %>% 
  summarise(cumulated_cases = sum(confirmed_cases_count)) %>% 
  arrange(lga_name19)

# Create source dataset with last-date cases per LGA
df_location_lga_lastday <- df_location %>% 
  mutate(notification_date = as.Date(notification_date)) %>% 
  filter(notification_date == max(notification_date)) %>% 
  select(lga_name19, confirmed_cases_count) %>% 
  group_by(lga_name19) %>% 
  summarise(cases_lastday = sum(confirmed_cases_count)) %>% 
  arrange(lga_name19)

# Create source dataset for this week cases per LHD
df_location_lga_thisweek <- df_location %>% 
  mutate(notification_date = as.Date(notification_date)) %>% 
  filter(notification_date > (max(notification_date) - 7) & 
           notification_date <= (max(notification_date))) %>% 
  select(lga_name19, confirmed_cases_count) %>% 
  group_by(lga_name19) %>% 
  summarise(lga_cases_thisweek =sum(confirmed_cases_count)) %>% 
  arrange(lga_name19)

# Combine LGA datasets
df_location_lga_comb <- df_location_lga_CS %>% 
  left_join(df_location_lga_thisweek) %>% 
  left_join(df_location_lga_lastday)

df_location_lga_comb <- df_location_lga_comb %>% 
  replace(is.na(.), 0) %>% 
  mutate(LGA_NAME = str_replace(lga_name19, "\\s*\\([^\\)]+\\)", "")) %>% 
  select(-lga_name19)

