# Preprocessing script for UKHLS
# remove prefix for predictors' names for combining by rows
names(a_hh) <- gsub("^a_","", names(a_hh))
names(b_hh) <- gsub("^b_","", names(b_hh))
names(c_hh) <- gsub("^c_","", names(c_hh))
names(d_hh) <- gsub("^d_","", names(d_hh))
names(e_hh) <- gsub("^e_","", names(e_hh))
names(f_hh) <- gsub("^f_","", names(f_hh))

ukhls_explo <- d_hh

# select the predictors (or create them if not existed) in each data set for combining by rows
a_hh <- a_hh %>% select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms, 
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)
b_hh <- b_hh %>% mutate(solar1 = rep(-8,n()), solar2 = solar1, solar3 = solar1, etariff = solar1) %>%
                 select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms, 
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)
c_hh <- c_hh %>% mutate(solar1 = rep(-8,n()), solar2 = solar1, solar3 = solar1, etariff = solar1) %>%
                 select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms,
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)
d_hh <- d_hh %>% select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms, 
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)
e_hh <- e_hh %>% mutate(solar1 = rep(-8,n()), solar2 = solar1, solar3 = solar1, etariff = solar1) %>%
                 select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms, 
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)
f_hh <- f_hh %>% mutate(solar1 = rep(-8,n()), solar2 = solar1, solar3 = solar1, etariff = solar1) %>%
                 select(hidp, xpduely, xpelecy, xpgasy, xpoily, xpsfly, solar1:solar3, gor_dv, hsbeds, hsrooms, 
                        fuelhave1:fuelhave4, hhsize, fihhmngrs_dv, nemp_dv, npens_dv, agechy_dv, tenure_dv, 
                        hsyrbuy, heatch, etariff)

ukhls_data_list <- list(a_hh, d_hh, rbind(a_hh,b_hh,c_hh,d_hh,e_hh,f_hh))
ukhls_data_prep <- function(data) {
  return(data <- data %>%
           rename(solar_elec = solar1, solar_heating = solar2, wind_tur = solar3, gov = gor_dv, 
                  no_bedroom = hsbeds, no_otherrooms = hsrooms, income = fihhmngrs_dv, employment = nemp_dv,
                  over65 = npens_dv, youngest = agechy_dv, tenure = tenure_dv, residency_length = hsyrbuy,
                  ch = heatch, elec = fuelhave1, gas = fuelhave2, oil = fuelhave3, other_fuel = fuelhave4) %>%
           mutate(
             gov = as.factor(gov) %>% fct_recode(
               "north east" = "1",
               "north west" = "2",
               "yorkshire and the humber" = "3",
               "east midlands" = "4",
               "west midlands" = "5",
               "east of england" = "6",
               "london" = "7",
               "south east" = "8",
               "south west" = "9",
               "wales" = "10",
               "scotland" = "11",
               "northern ireland" = "12",
               "missing" = "-9"
             ) %>% fct_drop(),
             tenure = as.factor(tenure) %>% fct_recode(
               "Owned outright" = "1",
               "Owned with mortgage" = "2",
               "Local authority rent" = "3",
               "Housing assoc rented" = "4",
               "Rented from employer" = "5",
               "Rented private unfurnished" = "6",
               "Rented private furnished" = "7",
               "Other" = "8",
               "Missing" = "-9"
             ) %>% fct_drop(),
             etariff = as.factor(etariff) %>% fct_recode(
               "yes - already buy" = "1",
               "yes - seriously considering" = "2",
               "no - neither" = "3",
               "considered in the past and rejected" = "4",
               "don't know" = "-1",
               "refused" = "-2",
               "inapplicable" = "-8",
               "missing" = "-9"
             ) %>% fct_drop(),
             ch = as.factor(ifelse(ch > 0, ch, rep(-1, length(ch)))) %>% fct_recode(
               "Y" = "1",
               "N" = "2",
               "DK" = "-1"
             ) %>% fct_drop(),
             elec = as.factor(ifelse(elec,"Y","N")) %>% fct_drop(), 
             gas = as.factor(ifelse(gas,"Y","N")) %>% fct_drop(), 
             oil = as.factor(ifelse(oil,"Y","N")) %>% fct_drop(), 
             other_fuel = as.factor(ifelse(other_fuel,"Y","N")) %>% fct_drop(),
             over65 = as.factor(ifelse(over65 > 0, "Y", "N")) %>% fct_drop(),
             youngest = as.factor(ifelse(youngest < 0, "no_dependent_child",
                                         ifelse(between(youngest,0,4), "0-4 years",
                                                ifelse(between(youngest,5,10), "5-10 years",
                                                       ifelse(between(youngest,11,15), "11-15 years",
                                                              "no_record"))))) %>% fct_drop(),
             residency_length = ifelse(residency_length > 0, 
                                       year(now()) - residency_length, 
                                       residency_length) # length of residency
           ) # turn all categorical variables into factors
  )
}
ukhls_data_list <- lapply(ukhls_data_list, ukhls_data_prep)
ukhls_data <- ukhls_data_list[[length(ukhls_data_list)]]

# Data for analysis for energy consumption
ukhls_consum_prep <- function(data) {
  return(data <- data %>%
           filter(xpduely > 0 | xpoily >0 | xpsfly > 0 | 
                    xpgasy > 0 | xpelecy > 0) %>% # observations that have reported energy consumption
           mutate(energy = ifelse(xpduely > 0, xpduely, 0) +
                           ifelse(xpoily > 0, xpoily, 0) +
                           ifelse(xpsfly > 0, xpsfly, 0) +
                           ifelse(xpgasy > 0, xpgasy, 0) +
                           ifelse(xpelecy > 0, xpelecy, 0)) %>% # sum up the energy consumption
           select(-(xpduely:xpsfly), -(solar_elec:wind_tur)) %>% select(hidp, energy, everything()) %>%
           filter(between(energy, 
                          mean(energy) - 3 * sd(energy), mean(energy) + 3 * sd(energy))) %>% # remove outliers
           filter(no_bedroom>0 & no_otherrooms>0 & hhsize>0 & income>=0 & 
                    employment>=0 & residency_length>=0) # remove observations have missing values
  )
}
ukhls_consum_list <- lapply(ukhls_data_list, ukhls_consum_prep)
ukhls_consum <- ukhls_consum_list[[length(ukhls_consum_list)]]

# Data for analysis for renewable energy
ukhls_renew_prep <- function(data) {
  return(data <- data %>%
           select(-(xpduely:xpsfly)) %>%
           filter(solar_elec > 0 | solar_heating > 0 | wind_tur > 0) %>% 
           mutate(renewable = ifelse(solar_elec <= 1 | solar_heating <= 1 | wind_tur <= 1, TRUE, FALSE),
                  solar_elec = ifelse(solar_elec <= 1, TRUE, FALSE),
                  solar_heating = ifelse(solar_heating <= 1, TRUE, FALSE),
                  wind_tur = ifelse(wind_tur <= 1, TRUE, FALSE)) %>% # length of residency
           select(hidp, renewable, everything()) %>%
           filter(no_bedroom>0 & no_otherrooms>0 & hhsize>0 & income>=0 & 
                    employment>=0 & residency_length>=0) %>% # remove observations have missing values
           mutate(tenure = fct_drop(tenure),
                  etariff = fct_drop(etariff))
  )
}
ukhls_renew_list <- lapply(ukhls_data_list, ukhls_renew_prep)
ukhls_renew <- ukhls_renew_list[[length(ukhls_renew_list)]]


# Data for exploratory analysis for proxies
ukhls_explo <- ukhls_explo %>%
  select(hidp,xpduely, xpoily, xpsfly, solar1:solar3, cduse1:cduse13, fuelduel, duelpay, xpelecy:gaspay) %>%
  filter(solar1 > 0 & solar2 > 0 & solar3 > 0 & cduse1 >= 0 & (xpelecy >= 0 | xpelecy == -8) &
           (xpgasy >= 0 | xpgasy == -8) & (xpduely > 0 | xpoily >0 | xpsfly > 0 | xpgasy > 0 | xpelecy > 0)) %>%
  mutate(
    energy = ifelse(xpduely > 0, xpduely, 0) +
             ifelse(xpoily > 0, xpoily, 0) +
             ifelse(xpsfly > 0, xpsfly, 0) +
             ifelse(xpgasy > 0, xpgasy, 0) +
             ifelse(xpelecy > 0, xpelecy, 0),
    renewable = ifelse(solar1 <= 1 | solar2 <= 1 | solar3 <= 1, TRUE, FALSE),
    solar1 = ifelse(solar1 <= 1, TRUE, FALSE),
    solar2 = ifelse(solar2 <= 1, TRUE, FALSE),
    solar3 = ifelse(solar3 <= 1, TRUE, FALSE),
    cduse1 = as.factor(cduse1) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse2 = as.factor(cduse2) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse3 = as.factor(cduse3) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse4 = as.factor(cduse4) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse5 = as.factor(cduse5) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse6 = as.factor(cduse6) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse7 = as.factor(cduse7) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse8 = as.factor(cduse8) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse9 = as.factor(cduse9) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse10 = as.factor(cduse10) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse11 = as.factor(cduse11) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse12 = as.factor(cduse12) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    cduse13 = as.factor(cduse13) %>% fct_recode(
      "not mentioned" = "0",
      "mentioned" = "1"
    ),
    fuelduel = as.factor(fuelduel) %>% fct_recode(
      "one bill" = "1",
      "separately" = "2",
      "inapplicable" = "-1",
      "inapplicable" = "-8",
      "refused" = "-2"
    ),
    duelpay = as.factor(duelpay) %>% fct_recode(
      "a fixed amount each month by standing order" = "1",
      "a monthly bill (by direct debit or other means)" = "2",
      "a quarterly bill (by direct debit or other means)" = "3",
      "a pre-payment (key/card or token) meter" = "4",
      "a fixed amount each month by standing order" = "5",
      "frequent cash payments (ie more frequent than once a month)" = "6",
      "a pre-payment (key/card or token) meter" = "7",
      "a pre-payment (key/card or token) meter" = "8",
      "inapplicable" = "-1",
      "inapplicable" = "-8",
      "inapplicable" = "97",
      "refused" = "-2"
    )  %>% fct_drop(),
    elecpay = as.factor(elecpay) %>% fct_recode(
      "a fixed amount each month by standing order" = "1",
      "a monthly bill (by direct debit or other means)" = "2",
      "a quarterly bill (by direct debit or other means)" = "3",
      "a pre-payment (key/card or token) meter" = "4",
      "a fixed amount each month by standing order" = "5",
      "frequent cash payments (ie more frequent than once a month)" = "6",
      "a pre-payment (key/card or token) meter" = "7",
      "a pre-payment (key/card or token) meter" = "8",
      "other" = "97",
      "inapplicable" = "-1",
      "inapplicable" = "-8"
    ) %>% fct_drop(),
    gaspay = as.factor(gaspay) %>% fct_recode(
      "a fixed amount each month by standing order" = "1",
      "a monthly bill (by direct debit or other means)" = "2",
      "a quarterly bill (by direct debit or other means)" = "3",
      "a pre-payment (key/card or token) meter" = "4",
      "a fixed amount each month by standing order" = "5",
      "frequent cash payments (ie more frequent than once a month)" = "6",
      "other" = "7",
      "a pre-payment (key/card or token) meter" = "8",
      "other" = "97",
      "inapplicable" = "-1",
      "inapplicable" = "-8"
    ) %>% fct_drop(),
    xpelecy = ifelse(xpelecy == -8, 0, xpelecy),
    xpgasy = ifelse(xpgasy == -8, 0, xpgasy)
  ) %>%
  rename(solar_elec = solar1, solar_heating = solar2, wind_tur = solar3, 
         color_tv = cduse1, video_recorder_or_dvd_player = cduse2, satellite_dish_or_sky_tv = cduse3,
         cable_tv = cduse4, fridge_freezer = cduse5, washing_machine = cduse6, tumble_dryer = cduse7,
         dish_washer = cduse8, microwave = cduse9, pc = cduse10, disc_player = cduse11, telephone = cduse12,
         mobile = cduse13, separate_or_combined_bills = fuelduel, payment_method = duelpay, elec_amount = xpelecy,
         elec_pay_method = elecpay, gas_amount = xpgasy, gas_pay_method = gaspay) %>%
  select(-c(xpduely,xpsfly,xpoily)) %>%
  select(hidp,energy, renewable,everything())

# Data for clustering
ukhls_predict <- ukhls_explo %>% inner_join(ukhls_renew %>% select(hidp, gov:etariff), by = "hidp")
  
# remove raw data sets
rm(list = c("a_hh","b_hh","c_hh","e_hh","f_hh"))
