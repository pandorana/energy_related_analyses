# Script for clustering analysis

# LCF - energy
set.seed(1)
lcf_clusters <- n_kmeans(lcf_consum$fuel_consum, 15)
lcf_cluster <- best_cluster(lcf_clusters)
lcf_consum_c <- cbind(lcf_consum, 
                      cluster = reorder_cluster(lcf_cluster, lcf_consum$fuel_consum)) %>%
                mutate(income = cut_number(income, 5),
                      hrp_age = cut_number(hrp_age, 5),
                      residency_length = cut_number(residency_length, 5))
lcf_correlation <- rcorr(lapply(lcf_consum_c, as.numeric) %>% .[3:length(.)] %>% as.data.frame() %>% as.matrix(), 
                       type = "spearman")
lcf_correlation_r <- lcf_correlation$r %>% .[nrow(.),] %>% .[1:(length(.)-1)]
lcf_correlation_p <- lcf_correlation$P %>% .[nrow(.),] %>% .[1:(length(.)-1)]

# LCF - other variables
set.seed(1)
lcf_clusters_other <- n_kmeans(lapply(lcf_consum %>% select(-(case:fuel_consum)), as.numeric) 
                                %>% as.data.frame() %>% as.matrix(), 15)
lcf_cluster_other <- best_cluster(lcf_clusters_other)
lcf_consum_c_other <- cbind(lcf_consum, 
                            cluster = reorder_cluster(lcf_cluster_other, lcf_consum$fuel_consum)) %>%
                      mutate(income = cut_number(income, 5),
                             hrp_age = cut_number(hrp_age, 5),
                             residency_length = cut_number(residency_length, 5))
lcf_correlation_other <- rcorr(cbind(lcf_consum_c_other$cluster, lcf_consum_c_other$fuel_consum %>% cut_number(5)),
                               type = "spearman")
lcf_correlation_r_other <- lcf_correlation_other$r[1,2]
lcf_correlation_p_other <- lcf_correlation_other$P[1,2]

# UKHLS - energy
set.seed(1)
ukhls_clusters <- n_kmeans(ukhls_predict %>% select(energy:wind_tur), 15)
ukhls_cluster <- best_cluster(ukhls_clusters)
ukhls_predict_c <- cbind(ukhls_predict, 
                         cluster = reorder_cluster(ukhls_cluster, ukhls_predict$energy)) %>%
                   mutate(elec_amount = cut_interval(elec_amount, 5),
                          gas_amount = cut_interval(gas_amount, 5),
                          income = cut_number(income, 5),
                          residency_length = cut_number(residency_length, 5))
ukhls_correlation <- rcorr(lapply(ukhls_predict_c, as.numeric) %>% .[3:length(.)] %>% as.data.frame() %>% 
                           as.matrix(), type = "spearman")
ukhls_correlation_r <- ukhls_correlation$r %>% .[nrow(.),] %>% .[1:(length(.)-1)]
ukhls_correlation_p <- ukhls_correlation$P %>% .[nrow(.),] %>% .[1:(length(.)-1)]


# UKHLS - other variables
set.seed(1)
ukhls_clusters_other <- n_kmeans(lapply(ukhls_predict %>% select(color_tv:etariff), as.numeric) %>%
                                 as.data.frame() %>% as.matrix(), 15)
ukhls_cluster_other <- best_cluster(ukhls_clusters_other)
ukhls_predict_c_other <- cbind(ukhls_predict, 
                               cluster = reorder_cluster(ukhls_cluster_other, ukhls_predict$energy)) %>%
                          mutate(elec_amount = cut_interval(elec_amount, 5),
                                 gas_amount = cut_interval(gas_amount, 5),
                                 income = cut_number(income, 5),
                                 residency_length = cut_number(residency_length, 5))
ukhls_correlation_other <- rcorr(lapply(ukhls_predict_c_other %>% mutate(energy = cut_number(energy, 5)), 
                                        as.numeric) %>% .[c(2:6,length(.))] %>% as.data.frame() %>% 
                                   as.matrix(), type = "spearman")
ukhls_correlation_r_other <- ukhls_correlation_other$r %>% .[nrow(.),] %>% .[1:(length(.)-1)]
ukhls_correlation_p_other <- ukhls_correlation_other$P %>% .[nrow(.),] %>% .[1:(length(.)-1)]
