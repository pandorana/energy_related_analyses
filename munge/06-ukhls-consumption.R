# Script for analysing correlation to energy consumption on UKHLS

# Building variables
ukhls_consum_building_mod <- lm(energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                         ukhls_consum)

# Socio-demorgraphic variables
ukhls_consum_socio_mod <- lm(energy ~ hhsize + income + employment + over65 + youngest + tenure + residency_length, 
                      ukhls_consum)

# Heating behaviours
ukhls_consum_heating_mod <- lm(energy ~ ch, ukhls_consum)

# Attitudes
ukhls_consum_attitude_mod <- lm(energy ~ etariff, ukhls_consum)

# Building + Socio-demorgraphic & ANOVA
ukhls_consum_building_socio_mod <- lm(energy ~ gov + no_bedroom + no_otherrooms + hhsize + elec + gas + oil + 
                               other_fuel + income + employment + over65 + youngest + tenure + residency_length, 
                               ukhls_consum)
ukhls_consum_building_2_building_socio_aov <- anova(ukhls_consum_building_mod, ukhls_consum_building_socio_mod)

# Building + Socio-demorgraphic + heating & ANOVA
ukhls_consum_building_socio_heating_mod <- lm(
                               energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
                               hhsize + income + employment + over65 + youngest + tenure + residency_length + 
                               ch, ukhls_consum)
ukhls_consum_building_socio_2_building_socio_heating_aov <- anova(ukhls_consum_building_socio_mod, 
                                                                  ukhls_consum_building_socio_heating_mod)

# Building + Socio-demorgraphic + heating + attitude & ANOVA
ukhls_consum_building_socio_heating_attitude_mod <- lm(energy ~ gov + no_bedroom + no_otherrooms + 
                               hhsize + income + employment + over65 + youngest + tenure + residency_length + 
                               elec + gas + oil + other_fuel + ch + 
                               etariff, ukhls_consum)
ukhls_consum_building_socio_heating_2_building_socio_heating_attitude_aov <- anova(
  ukhls_consum_building_socio_heating_mod, 
  ukhls_consum_building_socio_heating_attitude_mod)


## wave a
# Building variables
ukhls_consum_building_mod_a <- lm(energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                                ukhls_consum_list[[1]])

# Socio-demorgraphic variables
ukhls_consum_socio_mod_a <- lm(energy ~ hhsize + income + employment + over65 + youngest + tenure + 
                                 residency_length, ukhls_consum_list[[1]])

# Heating behaviours
ukhls_consum_heating_mod_a <- lm(energy ~ ch, ukhls_consum_list[[1]])

# Attitudes
ukhls_consum_attitude_mod_a <- lm(energy ~ etariff, ukhls_consum_list[[1]])

# Building + Socio-demorgraphic & ANOVA
ukhls_consum_building_socio_mod_a <- lm(energy ~ gov + no_bedroom + no_otherrooms + hhsize + elec + gas + oil + 
                    other_fuel + income + employment + over65 + youngest + tenure + residency_length, 
                  ukhls_consum_list[[1]])
ukhls_consum_building_2_building_socio_aov <- anova(ukhls_consum_building_mod_a, ukhls_consum_building_socio_mod_a)

# Building + Socio-demorgraphic + heating & ANOVA
ukhls_consum_building_socio_heating_mod_a <- lm(
  energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch, ukhls_consum_list[[1]])
ukhls_consum_building_socio_2_building_socio_heating_aov <- anova(ukhls_consum_building_socio_mod_a, 
                                                                  ukhls_consum_building_socio_heating_mod_a)

# Building + Socio-demorgraphic + heating + attitude & ANOVA
ukhls_consum_building_socio_heating_attitude_mod_a <- lm(energy ~ gov + no_bedroom + no_otherrooms + 
                   hhsize + income + employment + over65 + youngest + tenure + residency_length + 
                   elec + gas + oil + other_fuel + ch + 
                   etariff, ukhls_consum_list[[1]])
ukhls_consum_building_socio_heating_2_building_socio_heating_attitude_aov <- anova(
  ukhls_consum_building_socio_heating_mod_a, 
  ukhls_consum_building_socio_heating_attitude_mod_a)

## wave d
# Building variables
ukhls_consum_building_mod_d <- lm(energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel, 
                                  ukhls_consum_list[[2]])

# Socio-demorgraphic variables
ukhls_consum_socio_mod_d <- lm(energy ~ hhsize + income + employment + over65 + youngest + tenure + 
                                 residency_length, ukhls_consum_list[[2]])

# Heating behaviours
ukhls_consum_heating_mod_d <- lm(energy ~ ch, ukhls_consum_list[[2]])

# Attitudes
ukhls_consum_attitude_mod_d <- lm(energy ~ etariff, ukhls_consum_list[[2]])

# Building + Socio-demorgraphic & ANOVA
ukhls_consum_building_socio_mod_d <- lm(energy ~ gov + no_bedroom + no_otherrooms + hhsize + elec + gas + oil + 
                      other_fuel + income + employment + over65 + youngest + tenure + residency_length, 
                    ukhls_consum_list[[2]])
ukhls_consum_building_2_building_socio_aov <- anova(ukhls_consum_building_mod_d, ukhls_consum_building_socio_mod_d)

# Building + Socio-demorgraphic + heating & ANOVA
ukhls_consum_building_socio_heating_mod_d <- lm(
  energy ~ gov + no_bedroom + no_otherrooms + elec + gas + oil + other_fuel +
    hhsize + income + employment + over65 + youngest + tenure + residency_length + 
    ch, ukhls_consum_list[[2]])
ukhls_consum_building_socio_2_building_socio_heating_aov <- anova(ukhls_consum_building_socio_mod_d, 
                                                                  ukhls_consum_building_socio_heating_mod_d)

# Building + Socio-demorgraphic + heating + attitude & ANOVA
ukhls_consum_building_socio_heating_attitude_mod_d <- lm(energy ~ gov + no_bedroom + no_otherrooms + 
             hhsize + income + employment + over65 + youngest + tenure + residency_length + 
             elec + gas + oil + other_fuel + ch + 
             etariff, ukhls_consum_list[[2]])
ukhls_consum_building_socio_heating_2_building_socio_heating_attitude_aov <- anova(
  ukhls_consum_building_socio_heating_mod_d, 
  ukhls_consum_building_socio_heating_attitude_mod_d)
