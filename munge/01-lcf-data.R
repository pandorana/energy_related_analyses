# Preprocessing script for LCF
lcf_data <- lcf_data %>% 
  select(case, p117t, CHFuel, a116, a114p, gorx, a049, incanon, p396p, sexhrp, a040:a042, a121, p304, a131, a070p, 
         a150:a156) %>%
  rename(dwelling_type = a116, fuel_consum = p117t, no_rooms = a114p, gov = gorx, hh_size = a049, income = incanon, 
         hrp_age = p396p, hrp_sex = sexhrp, tenure = a121, unemployed = p304, residency_length = a131) %>%
  mutate(
    dwelling_type = as.factor(dwelling_type) %>% fct_recode(
      "detached" = "1",
      "semi-detached" = "2",
      "terrace" = "3",
      "maisonette" = "4",
      "flat" = "5",
      "others" = "6"
    ) %>% fct_drop(), 
    gov = as.factor(gov) %>% fct_recode(
      "North East" = "1",
      "North West and Merseyside" = "2",
      "Yorkshire and the Humber" = "3",
      "East Midlands" = "4",
      "West Midlands" = "5",
      "Eastern" = "6",
      "London" = "7",
      "South East" = "8",
      "South West" = "9",
      "Wales" = "10",
      "Scotland" = "11",
      "Northern Ireland" = "12"
    ) %>% fct_drop(), 
    tenure = as.factor(tenure) %>% fct_recode(
      "Local authority rented unfurn" = "1",
      "Housing association" = "2",
      "Other rented unfurnished" = "3",
      "Rented furnished" = "4",
      "Owned with mortgage" = "5",
      "Owned by rental purchase" = "6",
      "Owned outright" = "7",
      "Rent free" = "8"
    ) %>% fct_drop(), 
    unemployed = as.factor(unemployed) %>% fct_recode(
      "not recorded" = "0",
      "unemployed" = "1"
    ) %>% fct_drop(), 
    hrp_sex = as.factor(hrp_sex) %>% fct_recode(
      "male" = "1",
      "female" = "2"
    ) %>% fct_drop(), 
    youngest = as.factor(ifelse(a040, "0-2 years",
                         ifelse(a041, "2-5 years",
                         ifelse(a042, "5-18 years",
                                      "no dependent children")))) %>% 
      fct_relevel(c("no dependent children","0-2 years","2-5 years","5-18 years")) %>% fct_drop(),
    over75 = as.factor(ifelse(a070p >= 75, "has", "none")) %>% fct_drop(),
    ch_type = as.factor(ifelse(a150,"elec",
                        ifelse(a151,"gas",
                        ifelse(a152,"oil",
                        ifelse(a153,"solid_fuel",
                        ifelse(a154,"solid_oil_fuel",
                        ifelse(a155,"calor_gas",
                        ifelse(a156,"other_gas",
                                    "no_record")))))))) %>%
      fct_relevel(c("elec","gas","oil","solid_fuel","solid_oil_fuel","calor_gas","other_gas","no_record")) %>% 
      fct_drop(),
    CHFuel = ifelse(CHFuel == 7, TRUE, FALSE)
  ) %>% # turning all categorical variabls to be factors
  select(-(a150:a156),-(a040:a042),-a070p) %>%
  select(case, fuel_consum, CHFuel, dwelling_type, no_rooms, gov, ch_type, everything())

# Data for analysis for energy consumption
lcf_consum <- lcf_data %>%
  select(-CHFuel) %>%
  filter(fuel_consum > 0 & # households that have fuel expenditure data
         dwelling_type != "flat") %>% # this is removed as it's singularity
  filter(between(fuel_consum, mean(fuel_consum) - 3*sd(fuel_consum), 
                              mean(fuel_consum) + 3*sd(fuel_consum))) %>% # remove outliers
  mutate(dwelling_type = dwelling_type %>% fct_drop(), # droped as no obsevation has these values
         tenure = tenure %>% fct_drop())

# Data for analysis for installation of renewable energy
lcf_renew <- lcf_data %>%
  select(-fuel_consum, -ch_type) %>%
  rename(renewable = CHFuel) %>%
  filter(!is.na(renewable))


# Data for exploratory analysis for proxies
lcf_explo <- lcf_expen %>% 
  select(-Perstyp2) %>% 
  spread(Person, pdamount) %>% 
  mutate(amount = rowSums(.[3:10], na.rm = TRUE)) %>% 
  select(case, COI_PLUS, amount) %>% 
  spread(COI_PLUS, amount) %>% 
  select(case, "5.3.1.3.2", "5.3.1.4.1", "5.5.1.1.1", "5.5.2.1.4", "9.1.1.1.1", "9.1.1.1.3") %>% 
  mutate(sum = rowSums(.[2:7], na.rm = TRUE)) %>%
  rename(elec_cookers = `5.3.1.3.2`, heaters = `5.3.1.4.1`, elec_tools = `5.5.1.1.1`, 
         elec_consumables = `5.5.2.1.4`, audio_equipment = `9.1.1.1.1`, accessories_audio_equip = `9.1.1.1.3`) %>%
  inner_join(lcf_data %>%
              select(case, fuel_consum, CHFuel) %>% 
              filter(fuel_consum > 0 & # households that have fuel expenditure data
                       !is.na(CHFuel)) %>% 
              rename(renewable = CHFuel), by = "case")
