# Script for finding proxies for energy consumption and test their correlation with renewable energy on UKHLS

# linear models with formula 'energy ~ [any one variable]'
ukhls_proxy_solar_elec <- lm(energy ~ solar_elec, ukhls_explo)
ukhls_proxy_solar_heating <- lm(energy ~ solar_heating, ukhls_explo)
ukhls_proxy_wind_tur <- lm(energy ~ wind_tur, ukhls_explo)
ukhls_proxy_color_tv <- lm(energy ~ color_tv, ukhls_explo)
ukhls_proxy_video_recorder_or_dvd_player <- lm(energy ~ video_recorder_or_dvd_player, ukhls_explo)
ukhls_proxy_satellite_dish_or_sky_tv <- lm(energy ~ satellite_dish_or_sky_tv, ukhls_explo)
ukhls_proxy_cable_tv <- lm(energy ~ cable_tv, ukhls_explo)
ukhls_proxy_fridge_freezer <- lm(energy ~ fridge_freezer, ukhls_explo)
ukhls_proxy_washing_machine <- lm(energy ~ washing_machine, ukhls_explo)
ukhls_proxy_tumble_dryer <- lm(energy ~ tumble_dryer, ukhls_explo)
ukhls_proxy_dish_washer <- lm(energy ~ dish_washer, ukhls_explo)
ukhls_proxy_microwave <- lm(energy ~ microwave, ukhls_explo)
ukhls_proxy_pc <- lm(energy ~ pc, ukhls_explo)
ukhls_proxy_disc_player <- lm(energy ~ disc_player, ukhls_explo)
ukhls_proxy_telephone <- lm(energy ~ telephone, ukhls_explo)
ukhls_proxy_mobile <- lm(energy ~ mobile, ukhls_explo)
ukhls_proxy_separate_or_combined_bills <- lm(energy ~ separate_or_combined_bills, ukhls_explo)
ukhls_proxy_payment_method <- lm(energy ~ payment_method, ukhls_explo)
ukhls_proxy_elec_amount <- lm(energy ~ elec_amount, ukhls_explo)
ukhls_proxy_elec_pay_method <- lm(energy ~ elec_pay_method, ukhls_explo)
ukhls_proxy_gas_amount <- lm(energy ~ gas_amount, ukhls_explo)
ukhls_proxy_gas_pay_method <- lm(energy ~ gas_pay_method, ukhls_explo)

# model for checking explanatory power of combined variables
ukhls_proxy_all <- lm(energy ~ tumble_dryer + dish_washer + pc + elec_amount + gas_amount, ukhls_explo)

# model for testing correlation between types of renewable energy and proxies
ukhls_proxy_result <- glm(renewable ~ tumble_dryer + dish_washer + pc + elec_amount + gas_amount, 
                          family = binomial, ukhls_explo)
ukhls_proxy_solar_elec_result <- glm(solar_elec ~ tumble_dryer + dish_washer + pc + elec_amount + gas_amount, 
                          family = binomial, ukhls_explo)
ukhls_proxy_solar_heating_result <- glm(solar_heating ~ tumble_dryer + dish_washer + pc + elec_amount + gas_amount, 
                          family = binomial, ukhls_explo)
ukhls_proxy_wind_tur_result <- glm(wind_tur ~ tumble_dryer + dish_washer + pc + elec_amount + gas_amount, 
                                        family = binomial, ukhls_explo)
