# Script for analysing correlation to energy consumption on LCF

# Building variables
lcf_consum_building_mod <- lm(log(fuel_consum) ~ dwelling_type + no_rooms + gov + ch_type, lcf_consum)

# Socio-demorgraphic variables
lcf_consum_socio_mod <- lm(log(fuel_consum) ~ hh_size + unemployed + income + hrp_age + residency_length + over75 + 
                      youngest + hrp_sex + tenure, lcf_consum)

# Building + Socio-demorgraphic variables
lcf_consum_building_socio_mod <- lm(log(fuel_consum) ~ dwelling_type + no_rooms + gov + ch_type + 
                              hh_size + unemployed + income + hrp_age + residency_length + 
                              over75 + youngest + hrp_sex + tenure, lcf_consum)
# ANOVA for the increasing of explanatory power of model
lcf_consum_building_2_building_socio_aov <- anova(lcf_consum_building_mod, lcf_consum_building_socio_mod)
