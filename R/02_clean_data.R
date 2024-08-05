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


# Clean bromeliad data ---------------------------------------------------
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


# Clean community data ----------------------------------------------------
dplyr::glimpse(communities)

# Remove extra column
communities_clean <- 
  communities %>% 
  dplyr::select(bromeliad_id:n)

# Check if bromeliad ids are OK
communities$bromeliad_id[which(communities$bromeliad_id %notin% bromeliads$bromeliad_id)]
## One extra to fix
communities_clean <- 
  communities_clean %>% 
  dplyr::mutate(bromeliad_id = ifelse(bromeliad_id == "5",
                                      "B35", bromeliad_id))

# Check if typos or duplicates or tourists in taxonomy columns
## Class
unique(communities_clean$Class)
### Remove snail and tourist Enopla
communities_clean <- 
  communities_clean %>% 
  dplyr::filter(Class %notin% c("Gasteropoda", "Enopla"))
## Order
unique(communities_clean$Order)
### Remove tourist Hirudinae (thought what Genonermetes was) and Lepidoptera 
communities_clean <- 
  communities_clean %>% 
  dplyr::filter(Order %notin% c("Hirudinea", "Lepidoptera"))
## Family
unique(communities_clean$Family)
### Remove tourist Dolichoplana striata
communities_clean <- 
  communities_clean %>% 
  dplyr::filter(Family %notin% c("Geoplanidae"))
## Subamily
unique(communities_clean$Subfamily)
## Genus
unique(communities_clean$Genus)
## Species
unique(communities_clean$Species)
### Remove terrestrials and tourists
communities_clean <- 
  communities_clean %>% 
  dplyr::filter(!stringr::str_detect(string = Species,
                                     pattern = "14|15|28"))

# Now replace names and fill taxonomy based on DNA barcoding results

# Class Clitellata
communities_clean <- 
  communities_clean %>% 
  ## Enchytraeidae sp. 1
  dplyr::mutate(Order = ifelse(Class == "Clitellata" & Species == "sp1",
                "Haplotaxida", Order),
                Family = ifelse(Class == "Clitellata" & Species == "sp1",
                                "Enchytraeidae", Family)) %>% 
  ## Dichogaster andina
  dplyr::mutate(Order = ifelse(Class == "Clitellata" & Species == "sp2",
                               "Crassiclitella", Order),
                Family = ifelse(Class == "Clitellata" & Species == "sp2",
                                "Acanthodrilidae", Family),
                Subfamily = ifelse(Class == "Clitellata" & Species == "sp2",
                                "Benhamiinae", Subfamily),
                Genus = ifelse(Class == "Clitellata" & Species == "sp2",
                               "Dichogaster", Genus),
                Species = ifelse(Class == "Clitellata" & Species == "sp2",
                                 "andina", Species)) %>% 
  ## Crassiclitellata sp. 1
  dplyr::mutate(Order = ifelse(Class == "Clitellata" & Species == "sp3",
                               "Crassiclitellata", Order),
                Species = ifelse(Class == "Clitellata" & Species == "sp3",
                                "sp1", Species)) %>% 
  ## Clitellata sp. 1
  dplyr::mutate(Species = ifelse(Class == "Clitellata" & Species == "sp4",
                               "sp1", Species))

# Class Turbellaria
communities_clean <- 
  communities_clean %>% 
  # All same class and order
  dplyr::mutate(Class = ifelse(stringr::str_detect(string = Species,
                                                   pattern = "17|27|31"),
                               "Turbellaria", Class),
                Order = ifelse(stringr::str_detect(string = Species,
                                                   pattern = "17|27|31"),
                               "Tricladida", Order)) %>% 
  # Tricladida sp.1, sp. 2 and sp. 3
  dplyr::mutate(Species = ifelse(stringr::str_detect(string = Species,
                                                   pattern = "17"),
                                 "sp1", ifelse(
                                   stringr::str_detect(string = Species,
                                                       pattern = "31"),
                                   "sp2", ifelse(
                                     stringr::str_detect(string = Species,
                                                         pattern = "27"),
                                     "sp3", Species))))

# Subphyla Crustacea
communities_clean <- 
  communities_clean %>% 
  ## Podocopa sp. 1
  dplyr::mutate(Class = ifelse(Order == "Ostracoda" & !is.na(Order),
                               "Ostracoda", Class)) %>% 
  dplyr::mutate(Order = ifelse(Order == "Ostracoda",
                               "Podocopa", Order)) %>% 
  ## Copepoda sp. 1
  dplyr::mutate(Class = ifelse(Species %in% c("mystery4", "mystery_4"),
                               "Copepoda", Class),
                Species = ifelse(Species %in% c("mystery4", "mystery_4"),
                               "sp1", Species))

# Order Odonata
communities_clean <- 
  communities_clean %>% 
  ## Coenagrionidae sp. 1
  dplyr::mutate(Family = ifelse(Species == "sp1" & Order == "Odonata",
                                "Coenagrionidae", Family),
                Family = ifelse(Species == "sp2" & Order == "Odonata",
                                "Suborder Anisoptera", Family),
                Species = ifelse(Family == "Suborder Anisoptera" & !is.na(Family),
                                "sp1", Species))
# Order Coleoptera
communities_clean <- 
  communities_clean %>% 
  ## Curculionidae sp1 all good
  ## Dytiscidae sp. 1
  ### Here to parse between larvae and adults
  dplyr::mutate(stage = ifelse(Family == "Dytiscidae" & !is.na(Family) & 
                                 Species == "sp1",
                               "adult", "larva")) %>% 
  dplyr::mutate(Species = ifelse(Family == "Dytiscidae" & !is.na(Family),
                                 "sp1", Species)) %>% 
  ## Desmopachria sp. 1
  dplyr::mutate(Class = ifelse(Species == "mystery_6" & Class == "Crustacea",
                               "Hexapoda", Class),
                Order = ifelse(Species == "mystery_6",
                               "Coleoptera", Order),
                Family = ifelse(Species == "mystery_6",
                               "Dytiscidae", Family),
                Subfamily = ifelse(Species == "mystery_6",
                                   "Hydroporinae", Subfamily),
                Genus = ifelse(Species == "mystery_6",
                               "Desmopachria", Genus),
                Species = ifelse(Species == "mystery_6",
                               "sp1", Species)) %>% 
  ## Pyrophorus sp. 1 lost which bromeliad it was in...
  ## Hydrophilidae sp. 1 all good
  ## Hydrophilidae sp. 2
  dplyr::mutate(Class = ifelse(Species == "mystery_20",
                               "Hexapoda", Class),
                Order = ifelse(Species == "mystery_20",
                               "Coleoptera", Order),
                Family = ifelse(Species == "mystery_20",
                                "Hydrophilidae", Family),
                Species = ifelse(Species == "mystery_20",
                                 "sp2", Species)) %>% 
  ## Scirtidae sp. 1
  dplyr::mutate(Genus = ifelse(Family == "Scirtidae",
                               NA, Genus))

# Order Diptera
communities_clean <- 
  communities_clean %>% 
  ## Cecidomyiidae sp. 1
  dplyr::mutate(Class = ifelse(Species %in% c("mystery_1", "mystery1"),
                               "Hexapoda", Class),
                Order = ifelse(Species %in% c("mystery_1", "mystery1"),
                               "Diptera", Order),
                Family = ifelse(Species %in% c("mystery_1", "mystery1"),
                                "Cecidomyiidae", Family),
                Species = ifelse(Species %in% c("mystery_1", "mystery1"),
                                 "sp1", Species)) %>% 
  ## Ceratopogoninae sp. 1 all good
  ## Ceratopogoninae sp. 2 all good
  ## Ceratopogoninae sp. 3
  dplyr::mutate(Class = ifelse(Species == "mystery_29",
                               "Hexapoda", Class),
                Order = ifelse(Species == "mystery_29",
                               "Diptera", Order),
                Family = ifelse(Species == "mystery_29",
                                "Ceratopogonidae", Family),
                Subfamily = ifelse(Species == "mystery_29",
                                   "Ceratopogoninae", Subfamily),
                Species = ifelse(Species == "mystery_29",
                                 "sp3", Species)) %>% 
  ## Forcipomyiinae sp. 1 all good
  ## Forcipomyiinae sp. 2 all good
  ## Forcipomyiinae sp. 3 all good
  ## Forcipomyiinae sp. 4 all good
  dplyr::mutate(Class = ifelse(Species %in% c("mystery_7", "mystery7"),
                               "Hexapoda", Class),
                Order = ifelse(Species %in% c("mystery_7", "mystery7"),
                               "Diptera", Order),
                Family = ifelse(Species %in% c("mystery_7", "mystery7"),
                                "Ceratopogonidae", Family),
                Subfamily = ifelse(Species %in% c("mystery_7", "mystery7"),
                                   "Forcipomyiinae", Subfamily),
                Species = ifelse(Species %in% c("mystery_7", "mystery7"),
                                 "sp4", Species)) %>% 
  ## Chironominae has some pupae
  dplyr::mutate(stage = ifelse(stringr::str_detect(string = Species,
                                                   pattern = "pup"),
                               "pupa", stage)) %>% 
  ## Polypedilum sp. 1 all good
  ## Polypedilum sp. 2, annoyed here so I will do a clumsy workaround
  dplyr::mutate(Subfamily = ifelse(Genus == "Polypedilum" & !is.na(Genus),
                                   "temp", Subfamily),
                Genus = ifelse(Subfamily == "Chironominae" & !is.na(Subfamily),
                               "Polypedilum", Genus),
                Species = ifelse(Subfamily == "Chironominae" & !is.na(Subfamily),
                               "sp2", Species),
                Subfamily = ifelse(Genus == "Polypedilum" & !is.na(Genus),
                                   "Chironominae", Subfamily),
                Species = ifelse(Genus == "Polypedilum" & !is.na(Genus) & 
                                   Species == "pupa1",
                                 "sp2", Species)) %>% 
  ## Tanypodinae sp. 1 all good
  ## Corethrellidae sp. 1 all good
  ## Anopheles homonculus
  dplyr::mutate(Species = ifelse(Genus == "Anopheles" & !is.na(Genus),
                                 "homonculus", Species)) %>% 
  ## Culiseta sp. 1
  dplyr::mutate(Genus = ifelse(Genus == "Wyeomyia" & !is.na(Genus) 
                               & Species == "sp2",
                               "Culiseta", Genus)) %>% 
  dplyr::mutate(Species = ifelse(Genus == "Wyeomyia" & !is.na(Genus) 
                                 & Species == "sp2",
                               "sp1", Species)) %>% 
  ## Wyeomyia sp. 1
  dplyr::mutate(Species = ifelse(Genus == "Wyeomyia" & !is.na(Genus) 
                                 & Species == "sp3",
                                 "sp1", Species),
                Subfamily = ifelse(Genus == "Wyeomyia" & !is.na(Genus),
                                   "Culicinae", Subfamily)) %>% 
  ## Toxorhynchites haemorhoidalis
  dplyr::mutate(Species = ifelse(Genus == "Toxorhynchites" & !is.na(Genus),
                                 "haemorrhoidalis", Species)) %>% 
  ## Dolichopodidae sp. 1 all good
  ## Drosophilidae sp. 1 and sp. 2
  dplyr::mutate(Family = ifelse(Species %in% c("mystery_13", "mystery_21"),
                                "Drosophilidae", Family)) %>% 
  dplyr::mutate(Species = ifelse(Species == "mystery_13",
                                "sp1", Species)) %>% 
  dplyr::mutate(Species = ifelse(Species == "mystery_21",
                                 "sp2", Species)) %>%
  ## Limoniidae sp. 1
  dplyr::mutate(Family = ifelse(Family == "Tipulidae",
                                "Limoniidae", Family)) %>% 
  dplyr::mutate(Species = ifelse(Family == "Limoniidae",
                                  "sp1", Species)) %>% 
  ## Psychodidae sp. 1 all good
  ## Psychodidae sp. 2 all good
  ## Stratomyiidae sp. 1 and sp. 2
  dplyr::mutate(Family = ifelse(Species %in%  c("mystery_8", "mystery_22"),
                                "Stratomyiidae", Family),
                Species = ifelse(Species == "mystery_8",
                                 "sp1", Species),
                Species = ifelse(Species == "mystery_22",
                                 "sp2", Species))  %>% 
  ## Quichuana sp. 1
  dplyr::mutate(Genus = ifelse(Family == "Syrphidae" & !is.na(Family) 
                               & Species %in% c("sp1"),
                               "Quichuana", Genus)) %>% 
  ## Copestylum sp. 1
  dplyr::mutate(Genus = ifelse(Family == "Syrphidae" & !is.na(Family) 
                               & Species %in% c("sp2", "sp3"),
                               "Copestylum", Genus)) %>% 
  dplyr::mutate(Subfamily = ifelse(Genus == "Copestylum" & !is.na(Genus),
                                 "Eristalinae", Subfamily)) %>% 
  dplyr::mutate(Species = ifelse(Genus == "Copestylum" & !is.na(Genus),
                               "sp1", Species)) %>% 
  ## Some Culicidae as unidentified pupae
  dplyr::mutate(Species = ifelse(stringr::str_detect(string = Species,
                                                     pattern = "pupa"),
                                 "sp1", Species)) %>% 
  ## Some unidentified pupae in Diptera, somehow no notes of what they are
  dplyr::mutate(Species = ifelse(is.na(Species),
                                 "sp1", Species)) %>% 
  ## Drosophilidae sp.1  is pupa
  dplyr::mutate(stage = ifelse(!is.na(Family) & Family == "Drosophilidae" 
                               & Species == "sp1",
                               "pupa", stage)) %>% 
  ## We do not really know if non-insects are adults or larva
  dplyr::mutate(stage = ifelse(Class %in% c("Turbellaria", "Clitellata",
                                            "Ostracoda", "Copepoda"),
                               "unknown", stage))

# Double check unique species
communities_clean %>% 
  dplyr::select(Class:Species) %>% 
  dplyr::distinct()

# Culicidae and Drosophilidae need Order
communities_clean <- 
  communities_clean %>% 
  dplyr::mutate(Order = ifelse(Family %in% c("Drosophilidae", "Culicidae") 
                               & !is.na(Family),
                               "Diptera", Order))

# Save
readr::write_csv(communities_clean,
                 here::here("data_clean",
                            "communities.csv"))

