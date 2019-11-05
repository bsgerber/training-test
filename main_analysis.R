###############################################################################
#
# Opioid and Medicare Project
#
# John Shilka
# Bill Galanter
# Ben Gerber
#
# Aim: Investigate geographic variations in opioid prescriptions 2013-2017
#
# Data source:
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Part-D-Prescriber.html
#
# Created: 10/19/19
#
###############################################################################

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readxl)
library(janitor)
library(purrr)
library(maps)
library(officer)
library(flextable)

# Get map data for states in U.S.
us_states <- map_data("state")

# Set directory (will depend on computer accessing)
box_path <- "/Users/bgerber/Box Sync/Opioids and Medicare"
setwd(box_path)

# Get list of all data files (all Excel ending in xlsx)
box_path_data <- paste0(box_path, '/Data/PUF_Drug/')
file_list <- list.files(path = box_path_data, pattern = '*.xlsx')

# Read in descriptors of data files (first cell)
desc_list <- lapply(paste0(box_path_data, file_list), read_excel, sheet = 'Data', range = "A1")

# Read in actual data for all data files (skip first 2 rows)
data_list <- lapply(paste0(box_path_data, file_list), read_excel, sheet = 'Data', skip = 2, .name_repair = make_clean_names)

# Read in enrollment information for total Medicare beneficiaries by state
box_path_enroll <- paste0(box_path, '/Data/Enrollment_Dashboard/Enrollment Dashboard Data File_09_26_2019.xlsx')
enroll_headers <- read_excel(box_path_enroll, sheet = 'Hosp & Med Yearly Counts', skip = 4, col_names = FALSE)[1:2, ]
names(enroll_headers) <- str_remove(make_clean_names(paste(enroll_headers[2, ], enroll_headers[1, ], sep = "_")), "_na$")
enroll_data <- read_excel(box_path_enroll, sheet = 'Hosp & Med Yearly Counts', skip = 6, 
                          col_names = names(enroll_headers), col_types = c("guess", "guess", rep("numeric", length(names(enroll_headers)) - 2)))

# List of opioids of interest
opi_list <- toupper(c(
  "buprenorphine",
  "butorphanol",
  "codeine",
  "dihydrocodeine",
  "fentanyl",
  "hydrocodone",
  "meperidine",
  "methadone",
  "morphine",
  "oxycodone",
  "oxymorphone",
  "tapentadol",
  "tramadol"
))

# Create empty data frame to store mean, median, variance, etc. across states for prescribers
presc_df <- data.frame(
  opioid = character(),
  year = character(),
  n = integer(),
  mean = double(),
  sd = double(),
  median = double(),
  COV = double(),
  qtl25 = double(),
  qtl75 = double()
)

# Duplicate for claims
claim_df <- presc_df

###############################################################################
# Create output for each medication in the list
for (opi in opi_list) {

  cat("***", opi, "***")

  # Pull prescription records where generic names includes the opioid list (or specific opioid)
  sub_data_list <- data_list %>%
    purrr::map(~ .x %>%
      filter(grepl(paste(opi, collapse = "|"), generic_name)))
  
  # Check some results to see if filter is working (top 10 for number of prescribers for 2 states in 2017)
  sub_data_list[[5]] %>%
    filter(state_name == "Illinois" | state_name == "Massachusetts") %>%
    group_by(state_name) %>%
    top_n(n = 10, wt = number_of_prescribers) %>%
    arrange(state_name, desc(number_of_prescribers))
  
  # Put all years data into one data frame
  names(sub_data_list) <- str_extract(unlist(lapply(desc_list, names)), "(\\d)+")
  sub_data_years <- map_df(sub_data_list, ~as.data.frame(.x), .id="year")
  
  # Get sum of data across matched selected medicines by state for a given year
  state_sub_data <- sub_data_years %>%
    group_by(year, state_name) %>%
    summarise(total_prescribers = sum(number_of_prescribers, na.rm = TRUE),
              total_claims = sum(number_of_medicare_part_d_claims, na.rm = TRUE)) %>%
    print(n=300)
  
  # Declare total enrollees variable for each year
  enroll_data_long <- enroll_data %>%
    pivot_longer(cols = c(starts_with("total_")), values_to = "enrollees", names_to = "year") %>%
    mutate(year = sub("total_", "", year))
  
  # Join the data with enrollment data for that year
  joined_sub_data <- left_join(state_sub_data, enroll_data_long, by = c("state_name" = "state", "year" = "year")) %>%
    na.omit
  
  # Create variables to normalize prescribers and claims based on total enrollment
  joined_sub_data$prescribers_per_enrollee <- joined_sub_data$total_prescribers / joined_sub_data$enrollees
  joined_sub_data$claims_per_enrollee <- joined_sub_data$total_claims / joined_sub_data$enrollees
  
  # Store the results in a table (mean, median, cov, etc.)
  sum1_data <- joined_sub_data %>%
    group_by(year) %>%
    summarise(opioid = opi,
              n = n(),
              mean = mean(prescribers_per_enrollee),
              sd = sd(prescribers_per_enrollee),
              median = median(prescribers_per_enrollee),
              COV = sd/mean,
              qtl25 = quantile(prescribers_per_enrollee, probs = 0.25),
              qtl75 = quantile(prescribers_per_enrollee, probs = 0.75)
    )
  presc_df <- rbind(presc_df, sum1_data)
  
  sum2_data <- joined_sub_data %>%
    group_by(year) %>%
    summarise(opioid = opi,
              n = n(),
              mean = mean(claims_per_enrollee),
              sd = sd(claims_per_enrollee),
              median = median(claims_per_enrollee),
              COV = sd/mean,
              qtl25 = quantile(claims_per_enrollee, probs = 0.25),
              qtl75 = quantile(claims_per_enrollee, probs = 0.75)
    )
  claim_df <- rbind(claim_df, sum2_data)
  
  # Prepare state_sub_data for merging with state coordinate data for plotting (lower case required)
  joined_sub_data$state_name <- tolower(joined_sub_data$state_name)
  
  # Merge data with state-level map
  plot_data <- left_join(us_states, joined_sub_data, by = c("region" = "state_name"))
  
  # Plot a state map based on continuous data for prescribers per enrollee
  p1 <- ggplot(data = plot_data, mapping = aes(x = long, y = lat, group = group, fill = prescribers_per_enrollee)) +
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    labs(title = paste("Prescribers per enrollee:", opi)) + 
    theme_map() + 
    theme(legend.position = "bottom", legend.key.width=unit(3, "cm")) +
    labs(fill = "") +
    scale_fill_gradient(low = "white", high = "darkred") +
    facet_wrap(year ~ .)
  
  # Save plot as image
  ggsave(path = "Output/Figures", filename = paste0(opi, '_prescribers.png'), p1)
  
  # Plot a state map based on continuous data for claims per enrollee
  p2 <- ggplot(data = plot_data, mapping = aes(x = long, y = lat, group = group, fill = claims_per_enrollee)) +
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    labs(title = paste("Claims per enrollee:", opi)) + 
    theme_map() + 
    theme(legend.position = "bottom", legend.key.width=unit(3, "cm")) +
    labs(fill = "") +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    facet_wrap(year ~ .)
  
  # Save plot as image
  ggsave(path = "Output/Figures", filename = paste0(opi, '_claims.png'), p2)
}
###############################################################################

# Save summary table

for (yr in unique(presc_df$year)) {
  ft <- flextable(data = presc_df %>% filter(year == yr)) %>% 
    theme_zebra %>% 
    autofit
  
  read_docx() %>% 
    body_add_flextable(ft) %>% 
    print(target = paste0("Output/Tables/prescribers_", yr, ".docx"))
}

for (yr in unique(claim_df$year)) {
  ft <- flextable(data = claim_df %>% filter(year == yr)) %>% 
    theme_zebra %>% 
    autofit
  
  read_docx() %>% 
    body_add_flextable(ft) %>% 
    print(target = paste0("Output/Tables/claims_", yr, ".docx"))
}

# Additional evaluation of multiple opioids at one time (e.g., hydrocodone:oxycodone ratio)
opi <- toupper(c(
  "hydrocodone",
  "oxycodone"
))

# Pull prescription records where generic names includes the opioid list
sub_data_list <- data_list %>%
  purrr::map(~ .x %>%
               filter(grepl(paste(opi, collapse = "|"), generic_name)))

# Put all years data into one data frame
names(sub_data_list) <- str_extract(unlist(lapply(desc_list, names)), "(\\d)+")
sub_data_years <- map_df(sub_data_list, ~as.data.frame(.x), .id="year")

# Get sum of data across matched selected medicines by state for a given year
state_sub_data <- sub_data_years %>%
  mutate(opi_type = ifelse(grepl("HYDROCODONE", generic_name), "HYDROCODONE", "OXYCODONE")) %>%
  group_by(year, state_name, opi_type) %>%
  summarise(total_prescribers = sum(number_of_prescribers, na.rm = TRUE),
            total_claims = sum(number_of_medicare_part_d_claims, na.rm = TRUE)) %>%
  pivot_wider(names_from = opi_type, values_from = c(total_prescribers, total_claims)) %>%
  print(n=300)

# Create ratio variable of hydrocodone:oxycodone per state
state_sub_data$opi_ratio_prescr <- state_sub_data$total_prescribers_HYDROCODONE / (state_sub_data$total_prescribers_HYDROCODONE + state_sub_data$total_prescribers_OXYCODONE)
state_sub_data$opi_ratio_claims <- state_sub_data$total_claims_HYDROCODONE / (state_sub_data$total_claims_HYDROCODONE + state_sub_data$total_claims_OXYCODONE)

# Prepare state_sub_data for merging with state coordinate data for plotting (lower case required)
state_sub_data$state_name <- tolower(state_sub_data$state_name)

# Merge data with state-level map
plot_data <- left_join(us_states, state_sub_data, by = c("region" = "state_name"))

# Plot a state map for prescribers
p1 <- ggplot(data = plot_data, mapping = aes(x = long, y = lat, group = group, fill = opi_ratio_prescr)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Prescribers for Hydrocodone/(Hydrocodone+Oxycodone)") + 
  theme_map() + 
  theme(legend.position = "bottom", legend.key.width=unit(3, "cm")) +
  labs(fill = "") +
  scale_fill_gradient(low = "white", high = "darkred") +
  facet_wrap(year ~ .)

# Save plot as image
ggsave(path = "Output/Figures", filename = 'hydro_oxy_ratio_prescribers.png', p1)

# Plot a state map for claims
p2 <- ggplot(data = plot_data, mapping = aes(x = long, y = lat, group = group, fill = opi_ratio_claims)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Claims for Hydrocodone/(Hydrocodone+Oxycodone)") + 
  theme_map() + 
  theme(legend.position = "bottom", legend.key.width=unit(3, "cm")) +
  labs(fill = "") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  facet_wrap(year ~ .)

# Save plot as image
ggsave(path = "Output/Figures", filename = 'hydro_oxy_ratio_claims.png', p2)
