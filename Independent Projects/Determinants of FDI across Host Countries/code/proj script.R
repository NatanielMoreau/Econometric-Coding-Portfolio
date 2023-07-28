# Prep --------------------------------------------------------------------
library(pacman)
p_load(tidyverse, skimr, sandwich, stargazer, lfe, fixest, ggthemes, gmm, broom, janitor, magrittr, stringr)

country_data <- read_csv("14375c91-77d7-4be2-88d0-99c9880a5fc3_Data.csv")

# data cleaning 
skim(country_data)

# Names
# consistent names 
country_data %<>% clean_names()

# better names 
country_data %<>% rename(fdi_in = foreign_direct_investment_net_inflows_percent_of_gdp_bx_klt_dinv_wd_gd_zs, 
                             nni_pCap = adjusted_net_national_income_per_capita_annual_percent_growth_ny_adj_nnty_pc_kd_zg, 
                             ca_pGDP = current_account_balance_percent_of_gdp_bn_cab_xoka_gd_zs, 
                             gdp_pCapGr = gdp_per_capita_growth_annual_percent_ny_gdp_pcap_kd_zg, 
                             gdp_ConLcu = gdp_constant_lcu_ny_gdp_mktp_kn, 
                             va_man_pGDP = manufacturing_value_added_percent_of_gdp_nv_ind_manf_zs, 
                             urban_pct = urban_population_percent_of_total_population_sp_urb_totl_in_zs, 
                             co2_pCap = co2_emissions_metric_tons_per_capita_en_atm_co2e_pc, 
                             f_fuel = fossil_fuel_energy_consumption_percent_of_total_eg_use_comm_fo_zs, 
                             bank_cta = bank_capital_to_assets_ratio_percent_fb_bnk_capa_zs, 
                             inflation = inflation_consumer_prices_annual_percent_fp_cpi_totl_zg, 
                             rir = real_interest_rate_percent_fr_inr_rinr, 
                             air_freight = air_transport_freight_million_ton_km_is_air_good_mt_k1, 
                             port_traf = container_port_traffic_teu_20_foot_equivalent_units_is_shp_good_tu, 
                             ware_build = time_required_to_build_a_warehouse_days_ic_wrh_durs, 
                             corrupt_rank = control_of_corruption_percentile_rank_cc_per_rnk, 
                             gov_rank = government_effectiveness_percentile_rank_ge_per_rnk)

# One time changes 
# country_data[country_data == ".."] = NA  # recode NAs
# country_data = head(country_data, -5)    # remove filler rows
country_data %<>% mutate(time = as_factor(time), 
                         country_code = as_factor(country_code), 
                         ca_pGDP = as.numeric(ca_pGDP), 
                         nni_pCap = as.numeric(nni_pCap), 
                         va_man_pGDP =as.numeric(va_man_pGDP), 
                         f_fuel =as.numeric(f_fuel),
                         bank_cta =as.numeric(bank_cta),
                         inflation =as.numeric(inflation),
                         rir =as.numeric(rir),
                         air_freight =as.numeric(air_freight),
                         port_traf =as.numeric(port_traf),
                         gini_index_si_pov_gini =as.numeric(gini_index_si_pov_gini),
                         international_tourism_number_of_arrivals_st_int_arvl=
                           as.numeric(international_tourism_number_of_arrivals_st_int_arvl),
                         ware_build =as.numeric(ware_build), 
                         fdi_t1 = if_else(index(fdi_in) > 1, lag(fdi_in,1),NA), 
                         fdi_t2 = if_else(index(fdi_in) > 2, lag(fdi_in,2),NA),
                         fdi_t3 = if_else(index(fdi_in) > 3, lag(fdi_in,3),NA))
                         
# Visualization -----------------------------------------------------------

# FDI over time 
country_data %>% filter(between(fdi_in, -10,10)) %>% 
  ggplot(aes(time, fdi_in)) +
  geom_boxplot(fill = "lightblue") + 
  theme_clean() + 
  labs(title = "Net FDI Inflows Over Time", 
       y = "FDI", 
       x = "Year", 
       caption = "Net FDI inflows were quite consistent across the 10 years used in this project. 
       FDI range was limited to -10,10 but continues from -40,230+.
       Data sourced from World Development Indicators (WDI) database by the World Bank.") + 
  theme(plot.title = element_text(hjust = .5), 
        panel.background = element_rect(fill = "grey97"), 
        panel.grid.major.y = element_line(color = "red")) 




# Regressions -------------------------------------------------------------
# Simple FE model: Time & country effects
# no dep lag
reg_fe_nlag = felm(fdi_in ~ gdp_pCapGr + bank_cta + ca_pGDP + gov_rank 
                   + nni_pCap + va_man_pGDP + urban_pct + co2_pCap + inflation + 
                     air_freight + port_traf
                     | country_code + time| 0, data = country_data)
#dep lag
reg_fe_lag = felm(log(fdi_in) ~ log(fdi_t1) + gdp_pCapGr+ bank_cta + ca_pGDP + gov_rank 
                  + nni_pCap + va_man_pGDP + urban_pct + co2_pCap + inflation + 
                    air_freight + port_traf 
                    | country_code + time| 0, data = country_data)

# HC robust SE
se_FE = sqrt(diag(vcovHC(reg_fe_nlag, type = "HC1")))

#table
stargazer(reg_fe_lag,reg_fe_nlag,se = se_FE ,
          type = "text",
          keep = c(1,2,3,4,5), out = "D:/Users/Nataniel/Documents/EC 582/Term Project/Project v2/Project 2/try.html")

# 2sls IV model
iv_fe = felm(log(fdi_in) ~ gdp_pCapGr + bank_cta +ca_pGDP + gov_rank 
                  + nni_pCap + va_man_pGDP + urban_pct + co2_pCap + inflation + 
                    air_freight + port_traf | country_code + time| 
               (log(fdi_t1) ~ + fdi_t3), data = country_data)

# More SEs
se_test = sqrt(diag(vcovHC.default(iv_fe, type = "HC4", cluster = ...)))

#table
stargazer(iv_fe, type = "text",keep = c(1,2,3,4,13), keep.stat = c("n","f", "adj.rsq"), 
          out = "D:/Users/Nataniel/Documents/EC 582/Term Project/Project v2/Project 2/iv_fe.html")

range(country_data$fdi_in)

$