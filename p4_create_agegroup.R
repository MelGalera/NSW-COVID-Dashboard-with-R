### This script is for age group visualization (page3)


## PAGE 3 - AGE GROUP

# page3 part1 preparation
# Simplify age group names
df_agegroup <- df_agegroup %>% 
  mutate(notification_date = as.Date(notification_date)) %>%   
  mutate(Age_group = str_remove(age_group, "AgeGroup_")) %>% 
  select(-age_group) %>% 
  select(notification_date, Age_group, everything())

# Convert age group to factor
df_agegroup$Age_group <- as.factor(df_agegroup$Age_group)

# Create source dataset for summed cases per age group
df_agegroup_cases <- df_agegroup %>% 
  group_by(Age_group) %>%
  summarise(cases = sum(confirmed_cases_count)) %>% 
  mutate(percentage = percent(cases/sum(cases), accuracy = 0.1, trim = FALSE)) %>% 
  select(Age_group, cases, percentage)


# page3 part2 cumulative curves
# Define the age group
age_group_range <- c("0-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49",
                     "50-54", "55-59", "60-64", "65-69", "70+", "None")

# Pivot wider to get cumulative sum for each age group
pivoted_agegroup <- df_agegroup %>% 
  pivot_wider(names_from = Age_group, values_from = confirmed_cases_count) %>% 
  replace(is.na(.), 0) %>% 
  select(-confirmed_by_pcr)

# Compute for cumsum
cs_pivoted_agegroup <- pivoted_agegroup

for (i in 2:ncol(pivoted_agegroup)) {
  cs_pivoted_agegroup[, i ] <- cumsum(unlist(pivoted_agegroup[, i]))
}

# Pivot longer and create source dataset for agegroup cumsum
cs_agegroup <- cs_pivoted_agegroup %>% 
  pivot_longer(all_of(age_group_range), names_to = "Age_group", 
               values_to = "cumulative_cases") %>% 
  mutate(daynum = difftime(notification_date, min(notification_date), units = "days"))

