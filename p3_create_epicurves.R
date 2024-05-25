### This script is for extracting data and creating epicurves in Page 2


## PAGE 2 - LHD EPICURVES

# page2 preparation
# LHD Calculations 

# Create source for creating choices for selectInput in page2 
df_choices <- df_location_nsw %>% 
  group_by(lhd_2010_name) %>% 
  summarise(cases = sum(confirmed_cases_count, na.rm = TRUE)) %>% 
  mutate(lhd_loc = case_when(lhd_2010_name %in% metro_lhd ~ "Metropolitan LHD",
                             lhd_2010_name %in% rural_lhd ~ "Rural and Regional NSW LHD",
                             lhd_2010_name == "Correctional settings" ~ "Correctional settings",
                             lhd_2010_name == "Hotel Quarantine" ~ "Hotel Quarantine"
  )) %>% 
  mutate(location = case_when(lhd_loc == "Metropolitan LHD" ~ "LHD",
                              lhd_loc == "Rural and Regional NSW LHD" ~ "LHD",
                              lhd_loc %in% c("Correctional settings", "Hotel Quarantine") ~ "Non-LHD")
  ) %>% 
  select(location, lhd_loc, lhd_2010_name) %>% 
  add_row(location = c("All NSW", "LHD", "LHD"),
          lhd_loc = c("------", "Metropolitan LHD", "Rural and Regional NSW LHD"),
          lhd_2010_name = c("------", "All Metropolitan LHD", "All Rural and Regional LHD")) %>% 
  arrange(location, lhd_loc, lhd_2010_name)


# Create source dataset grouped by LHD epicurves
df_location_lhd <-  df_location_nsw %>% 
  group_by(notification_date, lhd_2010_name) %>% 
  summarise(cases = sum(confirmed_cases_count, na.rm = TRUE)) 

df_location_lhd_loc <- df_location_lhd %>% 
  mutate(lhd_loc = case_when(lhd_2010_name %in% metro_lhd ~ "Metropolitan LHD",
                             lhd_2010_name %in% rural_lhd ~ "Rural and Regional NSW LHD",
                             lhd_2010_name %in% c("Correctional settings", "Hotel Quarantine") ~ "Others"
  ))

# Create source dataset for metro-LHD epicurves
df_location_lhd_loc_metro <- df_location_lhd_loc %>% 
  filter(lhd_loc == "Metropolitan LHD") %>% 
  select(-lhd_loc)

# Create source dataset for rural and regional -LHD epicurves
df_location_lhd_loc_rural <- df_location_lhd_loc %>% 
  filter(lhd_loc == "Rural and Regional NSW LHD") %>% 
  select(-lhd_loc)

# Create function to change themes, scales and add labels in ggplots
plotdetails <- function(location_name, a, b) {
  plot_details <- list(theme_minimal(), 
                       theme(legend.position = 'none',
                             axis.text.y = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(a)),
                             axis.title = element_text(size = rel(1.1), face = 'bold'),
                             plot.title = element_text(size = rel(1.3), face = 'bold'),
                             legend.title = element_text(size = rel(1.1), face = 'bold'),
                             legend.text = element_text(size = rel(1.1)),
                             strip.text.x = element_text(size = rel(1.1))),
                       scale_x_date(date_labels = "%d %b %y",
                                    breaks = pretty_breaks(n = b)),
                       scale_y_continuous(breaks = pretty_breaks(n = 8),
                                          labels = comma),
                       labs(title = glue::glue("COVID-19 Epicurve(s) for confirmed incident cases in {location_name}"),
                            x = "Date", 
                            y = "Incident cases"))
  return(plot_details)}

