# Cleaning data
# Libraries
library(tidyverse)
library(here)

# Load data
bromeliads <- 
  readr::read_csv(here::here("data_raw",
                             "bromeliads.csv"))
detritus <- 
  readr::read_csv(here::here("data_raw",
                             "detritus.csv"))
communities <- 
  readr::read_csv(here::here("data_raw",
                             "communities.csv"))

## custom function
'%notin%' <- 
  Negate('%in%')


# Clean bromelliad data ---------------------------------------------------
dplyr::glimpse(bromeliads)

# Check all the unique values
unique(bromeliads$site)
## Replace F3 with F3-F5
bromeliads <- 
  bromeliads %>% 
  dplyr::mutate(site = ifelse(bromeliad_id_old %in% c("F3", "F4", "F5"),
                                      "F3-F5", site)) %>% 
  ## Rename bromeliad id column
  dplyr::rename(bromeliad_id = bromeliad_id_old)
  
unique(bromeliads$date_collected) 
unique(bromeliads$bromeliad_id)
unique(bromeliads$distance_from_ground_cm)
range(bromeliads$distance_from_ground_cm,
      na.rm = T)
unique(bromeliads$height_mm)
range(bromeliads$height_mm,
      na.rm = T)
unique(bromeliads$width_mm)
range(bromeliads$width_mm,
      na.rm = T)
unique(bromeliads$actual_volume_mL)
range(bromeliads$actual_volume_mL,
      na.rm = T)
unique(bromeliads$water_holding_capacity_mL)
range(bromeliads$water_holding_capacity_mL,
      na.rm = T)
unique(bromeliads$longest_leaf_length_mm)
range(bromeliads$longest_leaf_length_mm,
      na.rm = T)

# Make sure actual volume is less then water holding capacity
bromeliads$water_holding_capacity_mL > bromeliads$actual_volume_mL
## Few measurement errors, put NA instead
bromeliads <- 
  bromeliads %>% 
  dplyr::mutate(water_holding_capacity_mL = ifelse(water_holding_capacity_mL < actual_volume_mL,
                                                   NA, water_holding_capacity_mL))



# Clean detritus data -----------------------------------------------------
dplyr::glimpse(detritus)

# Check all the unique values
unique(detritus$bromeliad_id)
## Make sure no weird things with bromeliad ids
bromeliads %>% 
  dplyr::select(bromeliad_id) %>% 
  dplyr::filter(bromeliad_id %notin% unique(detritus$bromeliad_id)) %>% 
  print(n = 75)
detritus %>% 
  dplyr::select(bromeliad_id) %>% 
  dplyr::filter(bromeliad_id %notin% unique(bromeliads$bromeliad_id)) %>% 
  print(n = 75) ## Looks like better to join detritus on top of bromeliads

unique(detritus$`mesh size`)  
unique(detritus$bag_number) # remove column
unique(detritus$paper_mass_g)
range(detritus$paper_mass_g,
      na.rm = T)
unique(detritus$dry_mass_g)
range(detritus$dry_mass_g,
      na.rm = T)
unique(detritus$detritus_mass_g) ## column was there to already have data formatted

# Make sure dry mass of detritus will be positive
detritus$dry_mass_g > detritus$paper_mass_g ## ok well some measurement errors here

# Left join a modified detritus data to the bromeliad data
bromeliads <- 
  bromeliads %>% 
  dplyr::left_join(detritus %>% 
                      dplyr::mutate(detritus_mass_g = ifelse(dry_mass_g > paper_mass_g,
                                                             dry_mass_g - paper_mass_g,
                                                             NA)) %>% 
                      dplyr::select(-bag_number, -dry_mass_g, -paper_mass_g) %>% 
                      ## Remove NAs
                      dplyr::filter(!is.na(bromeliad_id)) %>% 
                      ## combine those bromeliads for which there were more than one bag
                      dplyr::group_by(bromeliad_id, `mesh size`) %>% 
                      dplyr::summarise_all(sum) %>% 
                      ## Make wider
                      tidyr::pivot_wider(names_from = 'mesh size',
                                         names_prefix = "detritus_g_",
                                         values_from = detritus_mass_g,
                                         values_fill = NA),
                    by = "bromeliad_id")

# Save joined data
readr::write_csv(bromeliads,
                 here::here("data_clean",
                            "bromeliads.csv"))
