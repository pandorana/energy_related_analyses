# Script for analysing correlation to usage of renewable energy on UKHLS

### renewable
# Building variables
ukhls_renew_building_mod <- glm(renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                                family = binomial, ukhls_renew)

# Socio-demorgraphic variables
ukhls_renew_socio_mod <- glm(renewable ~ hhsize + income + employment + over65 + youngest + tenure + 
                               residency_length, family = binomial, ukhls_renew)

# Heating behaviours
ukhls_renew_heating_mod <- glm(renewable ~ ch, family = binomial, ukhls_renew)

# Attitudes
ukhls_renew_attitude_mod <- glm(renewable ~ etariff, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic
ukhls_renew_building_socio_mod <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length, 
  family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern
ukhls_renew_building_socio_heating_mod <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
  hhsize + income + employment + over65 + youngest + tenure + residency_length + 
  ch, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern + Attitude
ukhls_renew_building_socio_heating_attitude_mod <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
  hhsize + income + employment + over65 + youngest + tenure + residency_length + 
  ch + etariff, family = binomial, ukhls_renew)
# Group Lasso for the all-combined model
set.seed(1)
X_ukhls <- model_matrix(ukhls_renew, renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
                          hhsize + income + employment + over65 + youngest + tenure + residency_length + 
                          ch + etariff) %>% .[,2:ncol(.)] %>% as.matrix()
Y_ukhls <- ukhls_renew["renewable"] %>% as.matrix()
I_ukhls <- c(rep(1,12), 2:11, rep(12,3), rep(13,3), 14, rep(15,2), rep(16,5))
data_ukhls <- list(x = X_ukhls, y = Y_ukhls)
if (is.na(match("ukhls_renew_all_fit", ls()))) { # cached this fit as it is too time consuming
  ukhls_renew_all_fit <- cvSGL(data_ukhls, I_ukhls, nlam = 100, alpha = 0)
  cache("ukhls_renew_all_fit") # delete this .RData file under cache/
}
# Model after Lasso regression
ukhls_renew_all_fit_mod <- glm(renewable ~ no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
                                 income + employment + over65 + tenure + residency_length + 
                                 etariff, family = binomial, ukhls_renew)


### solar_elec
# Building variables
ukhls_solar_elec_building_mod <- glm(solar_elec ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel,
                                family = binomial, ukhls_renew)

# Socio-demorgraphic variables
ukhls_solar_elec_socio_mod <- glm(solar_elec ~ hhsize + income + employment + over65 + youngest + tenure +
                               residency_length, family = binomial, ukhls_renew)

# Heating behaviours
ukhls_solar_elec_heating_mod <- glm(solar_elec ~ ch, family = binomial, ukhls_renew)

# Attitudes
ukhls_solar_elec_attitude_mod <- glm(solar_elec ~ etariff, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic
ukhls_solar_elec_building_socio_mod <- glm(
  solar_elec ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length,
  family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern
ukhls_solar_elec_building_socio_heating_mod <- glm(
  solar_elec ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length +
    ch, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern + Attitude
ukhls_solar_elec_building_socio_heating_attitude_mod <- glm(
  solar_elec ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length +
    ch + etariff, family = binomial, ukhls_renew)


### solar_heating
# Building variables
ukhls_solar_heating_building_mod <- glm(solar_heating ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel,
                                     family = binomial, ukhls_renew)

# Socio-demorgraphic variables
ukhls_solar_heating_socio_mod <- glm(solar_heating ~ hhsize + income + employment + over65 + youngest + tenure +
                                    residency_length, family = binomial, ukhls_renew)

# Heating behaviours
ukhls_solar_heating_heating_mod <- glm(solar_heating ~ ch, family = binomial, ukhls_renew)

# Attitudes
ukhls_solar_heating_attitude_mod <- glm(solar_heating ~ etariff, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic
ukhls_solar_heating_building_socio_mod <- glm(
  solar_heating ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length,
  family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern
ukhls_solar_heating_building_socio_heating_mod <- glm(
  solar_heating ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length +
    ch, family = binomial, ukhls_renew)

# Building + Socio-demorgraphic + Heating pattern + Attitude
ukhls_solar_heating_building_socio_heating_attitude_mod <- glm(
  solar_heating ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length +
    ch + etariff, family = binomial, ukhls_renew)


### renewable: wave a
# Building variables
ukhls_renew_building_mod_a <- glm(renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                                family = binomial, ukhls_renew_list[[1]])

# Socio-demorgraphic variables
ukhls_renew_socio_mod_a <- glm(renewable ~ hhsize + income + employment + over65 + youngest + tenure + 
                               residency_length, family = binomial, ukhls_renew_list[[1]])

# Heating behaviours
ukhls_renew_heating_mod_a <- glm(renewable ~ ch, family = binomial, ukhls_renew_list[[1]])

# Attitudes
ukhls_renew_attitude_mod_a <- glm(renewable ~ etariff, family = binomial, ukhls_renew_list[[1]])

# Building + Socio-demorgraphic
ukhls_renew_building_socio_mod_a <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length, 
  family = binomial, ukhls_renew_list[[1]])

# Building + Socio-demorgraphic + Heating pattern
ukhls_renew_building_socio_heating_mod_a <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch, family = binomial, ukhls_renew_list[[1]])

# Building + Socio-demorgraphic + Heating pattern + Attitude
ukhls_renew_building_socio_heating_attitude_mod_a <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch + etariff, family = binomial, ukhls_renew_list[[1]])
  

### renewable: wave d
# Building variables
ukhls_renew_building_mod_d <- glm(renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                                  family = binomial, ukhls_renew_list[[2]])

# Socio-demorgraphic variables
ukhls_renew_socio_mod_d <- glm(renewable ~ hhsize + income + employment + over65 + youngest + tenure + 
                                 residency_length, family = binomial, ukhls_renew_list[[2]])

# Heating behaviours
ukhls_renew_heating_mod_d <- glm(renewable ~ ch, family = binomial, ukhls_renew_list[[2]])

# Attitudes
ukhls_renew_attitude_mod_d <- glm(renewable ~ etariff, family = binomial, ukhls_renew_list[[2]])

# Building + Socio-demorgraphic
ukhls_renew_building_socio_mod_d <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length, 
  family = binomial, ukhls_renew_list[[2]])

# Building + Socio-demorgraphic + Heating pattern
ukhls_renew_building_socio_heating_mod_d <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch, family = binomial, ukhls_renew_list[[2]])

# Building + Socio-demorgraphic + Heating pattern + Attitude
ukhls_renew_building_socio_heating_attitude_mod_d <- glm(
  renewable ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel + 
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch + etariff, family = binomial, ukhls_renew_list[[2]])
