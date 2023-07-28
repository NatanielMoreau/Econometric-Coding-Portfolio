library(pacman)
p_load(tidyverse,ggthemes, stargazer, Hmisc, sandwich, broom, psychometric)

atlas = read_csv("atlas.csv")

detach(atlas)

# 3 upward mobility means
tract_kfr_25 = mean(atlas$kfr_pooled_p25[atlas$state == 41 & atlas$county == 051 &
                                           atlas$tract == 006701], na.rm = T)

state_kfr_25 = wtd.mean(atlas$kfr_pooled_p25[atlas$state == 41], 
                        atlas$count_pooled[atlas$state == 41], na.rm = T)

us_kfr_25 = wtd.mean(atlas$kfr_pooled_p25, atlas$count_pooled, na.rm = T)

# percent difference
((tract_kfr_25-state_kfr_25)/state_kfr_25)*100

((tract_kfr_25-us_kfr_25)/us_kfr_25)*100

#4 upward mobility SDs

county_sd_kfr25 = sqrt(wtd.var(atlas$kfr_pooled_p25[atlas$county == 051], 
                               atlas$count_pooled[atlas$county == 051],na.rm = T))

state_sd_kfr25 = sqrt(wtd.var(atlas$kfr_pooled_p25[atlas$state == 41], 
                              atlas$count_pooled[atlas$state == 41],na.rm = T))

us_sd_kfr25 = sqrt(wtd.var(atlas$kfr_pooled_p25, 
                           atlas$count_pooled,na.rm = T))

#percent difference 

((county_sd_kfr25-state_sd_kfr25)/state_sd_kfr25)*100

((county_sd_kfr25-us_sd_kfr25)/us_sd_kfr25)*100

#5 downward mobility
#75 means
tract_kfr_75 = mean(atlas$kfr_pooled_p75[atlas$state == 41 & atlas$county == 051 &
                                           atlas$tract == 006701], na.rm = T)

state_kfr_75 = wtd.mean(atlas$kfr_pooled_p75[atlas$state == 41], 
                        atlas$count_pooled[atlas$state == 41], na.rm = T)

us_kfr_75 = wtd.mean(atlas$kfr_pooled_p75, atlas$count_pooled, na.rm = T)

#percent differences

((tract_kfr_75-state_kfr_75)/state_kfr_75)*100

((tract_kfr_75-us_kfr_75)/us_kfr_75)*100


#75 SDs
county_sd_kfr75 = sqrt(wtd.var(atlas$kfr_pooled_p75[atlas$county == 051], 
                               atlas$count_pooled[atlas$county == 051],na.rm = T))

state_sd_kfr75 = sqrt(wtd.var(atlas$kfr_pooled_p75[atlas$state == 41], 
                              atlas$count_pooled[atlas$state == 41],na.rm = T))

us_sd_kfr75 = sqrt(wtd.var(atlas$kfr_pooled_p75, 
                           atlas$count_pooled,na.rm = T))

#percent differences

((county_sd_kfr75-state_sd_kfr75)/state_sd_kfr75)*100

((county_sd_kfr75-us_sd_kfr75)/us_sd_kfr75)*100



#100 means
tract_kfr_100 = mean(atlas$kfr_pooled_p100[atlas$state == 41 & atlas$county == 051 &
                                           atlas$tract == 006701], na.rm = T)

state_kfr_100 = wtd.mean(atlas$kfr_pooled_p100[atlas$state == 41], 
                        atlas$count_pooled[atlas$state == 41], na.rm = T)

us_kfr_100 = wtd.mean(atlas$kfr_pooled_p100, atlas$count_pooled, na.rm = T)

#percent difference

((tract_kfr_100-state_kfr_100)/state_kfr_100)*100

((tract_kfr_100-us_kfr_100)/us_kfr_100)*100

#100 SDs
county_sd_kfr100 = sqrt(wtd.var(atlas$kfr_pooled_p100[atlas$county == 051], 
                               atlas$count_pooled[atlas$county == 051],na.rm = T))

state_sd_kfr100 = sqrt(wtd.var(atlas$kfr_pooled_p100[atlas$state == 41], 
                              atlas$count_pooled[atlas$state == 41],na.rm = T))

us_sd_kfr100 = sqrt(wtd.var(atlas$kfr_pooled_p100, 
                           atlas$count_pooled,na.rm = T))

#percent difference 

((county_sd_kfr100-state_sd_kfr100)/state_sd_kfr100)*100

((county_sd_kfr100-us_sd_kfr100)/us_sd_kfr100)*100

#6 

Multnomah = atlas %>% filter(state == 41 & county == 051)

inc_25_75_1 = lm(kfr_pooled_p75 ~ kfr_pooled_p25, data = Multnomah)
# regression 75 on 25
coeftest(inc_25_75_1, vcov. = vcovHC(inc_25_75_1, type = "HC1"))



stargazer(inc_25_75_1, type = "html", out = "D:/Users/Nataniel/Documents/EC 510/ Inc25v75.html", 
          dep.var.labels = ("75th pct"), 
          covariate.labels = ("25th pct"), 
          title = "Child HH Income 75th regressed on 25th", 
          style = "qje", 
          keep.stat = c("n", "rsq"))
#graph 75 vs 25
Multnomah %>% 
  ggplot(aes(kfr_pooled_p25, kfr_pooled_p75, size = count_pooled, alpha = .8)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se = F) + 
  theme_clean() + 
  scale_size_continuous(range = c(2,6)) + 
  labs(title = "Child HH Income By Parental Income Percentile: 75th v 25th ", 
       subtitle = "Multnomah County, OR", 
       x = "Parents at 25th",
       y = "Parents at 75tth", 
       caption = "obs. size = tract size") + 
  theme(plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5)) + 
  guides(alpha = "none", 
         size = "none")



#by race upward mob: W vs B only 
#county-level
county_wkfr_25 = wtd.mean(Multnomah$kfr_white_p25, Multnomah$count_white, na.rm = T)

county_bkfr_25 = wtd.mean(Multnomah$kfr_black_p25, Multnomah$count_black, na.rm = T)

#percent dif
((county_wkfr_25-county_bkfr_25)/county_bkfr_25)*100

#state-level
state_wkfr_25 = wtd.mean(atlas$kfr_white_p25[atlas$state == 41], 
                        atlas$count_white[atlas$state == 41], na.rm = T)

state_bkfr_25 = wtd.mean(atlas$kfr_black_p25[atlas$state == 41], 
                         atlas$count_black[atlas$state == 41], na.rm = T)
#percent dif
((state_wkfr_25-state_bkfr_25)/state_bkfr_25)*100

#US-level
us_wkfr_25 = wtd.mean(atlas$kfr_white_p25, atlas$count_white, na.rm = T)
us_bkfr_25 = wtd.mean(atlas$kfr_black_p25, atlas$count_black, na.rm = T)

#percentdiff
((us_wkfr_25-us_bkfr_25)/us_bkfr_25)*100

#by race sds

#county-level
county_sd_wkfr25 = sqrt(wtd.var(atlas$kfr_white_p25[atlas$county == 051], 
                               atlas$count_white[atlas$county == 051],na.rm = T))

county_sd_bkfr25 = sqrt(wtd.var(atlas$kfr_black_p25[atlas$county == 051], 
                               atlas$count_black[atlas$county == 051],na.rm = T))

#percent diff
((county_sd_wkfr25-county_sd_bkfr25)/county_sd_bkfr25)*100


#state-level
state_sd_wkfr25 = sqrt(wtd.var(atlas$kfr_white_p25[atlas$state == 41], 
                              atlas$count_white[atlas$state == 41],na.rm = T))

state_sd_bkfr25 = sqrt(wtd.var(atlas$kfr_black_p25[atlas$state == 41], 
                              atlas$count_black[atlas$state == 41],na.rm = T))

((state_sd_wkfr25-state_sd_bkfr25)/state_sd_bkfr25)*100

#us-level
us_sd_wkfr25 = sqrt(wtd.var(atlas$kfr_white_p25, 
                           atlas$count_white,na.rm = T))
us_sd_bkfr25 = sqrt(wtd.var(atlas$kfr_black_p25, 
                           atlas$count_black,na.rm = T))

#by race downward mobility

#75 means

county_wkfr_75 = wtd.mean(Multnomah$kfr_white_p75, Multnomah$count_white, na.rm = T)

county_bkfr_75 = wtd.mean(Multnomah$kfr_black_p75, Multnomah$count_black, na.rm = T)

((county_wkfr_75-county_bkfr_75)/county_bkfr_75)*100

state_wkfr_75 = wtd.mean(atlas$kfr_white_p75[atlas$state == 41], 
                        atlas$count_white[atlas$state == 41], na.rm = T)

state_bkfr_75 = wtd.mean(atlas$kfr_black_p75[atlas$state == 41], 
                        atlas$count_black[atlas$state == 41], na.rm = T)

((state_wkfr_75-state_bkfr_75)/state_bkfr_75)*100

us_wkfr_75 = wtd.mean(atlas$kfr_white_p75, atlas$count_white, na.rm = T)

us_bkfr_75 = wtd.mean(atlas$kfr_black_p75, atlas$count_black, na.rm = T)

range(Multnomah$kfr_black_p25, na.rm = T)
#75 SDs

county_sd_wkfr75 = sqrt(wtd.var(atlas$kfr_white_p75[atlas$county == 051], 
                               atlas$count_white[atlas$county == 051],na.rm = T))

county_sd_bkfr75 = sqrt(wtd.var(atlas$kfr_black_p75[atlas$county == 051], 
                                atlas$count_black[atlas$county == 051],na.rm = T))

((county_sd_wkfr75-county_sd_bkfr75)/county_sd_bkfr75)*100

state_sd_wkfr75 = sqrt(wtd.var(atlas$kfr_white_p75[atlas$state == 41], 
                              atlas$count_white[atlas$state == 41],na.rm = T))

state_sd_bkfr75 = sqrt(wtd.var(atlas$kfr_black_p75[atlas$state == 41], 
                              atlas$count_black[atlas$state == 41],na.rm = T))

((state_sd_wkfr75-state_sd_bkfr75)/state_sd_bkfr75)*100

us_sd_wkfr75 = sqrt(wtd.var(atlas$kfr_white_p75, 
                           atlas$count_white,na.rm = T))

us_sd_bkfr75 = sqrt(wtd.var(atlas$kfr_black_p75, 
                          atlas$count_black,na.rm = T))

#same race percent diffs 

#25th 
((county_wkfr_25-state_wkfr_25)/state_wkfr_25)*100

((county_bkfr_25-state_bkfr_25)/state_bkfr_25)*100

#75th
((county_wkfr_75-state_wkfr_75)/state_wkfr_75)*100

((county_bkfr_75-state_bkfr_75)/state_bkfr_75)*100


#25 v 75 by race 
Multnomah %>% 
  ggplot(aes(kfr_white_p25, kfr_white_p75, size = count_white, alpha = .8)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se = F) + 
  theme_clean() + 
  scale_size_continuous(range = c(2,6)) + 
  labs(title = "White Child HH Income By Parental Income Percentile: 75th v 25th", 
       subtitle = "Multnomah County, OR", 
       x = "Parents at 25th",
       y = "Parents at 75tth", 
       caption = "obs. size = tract size") + 
  theme(plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5)) + 
  guides(alpha = "none", 
         size = "none")

Multnomah %>% 
  ggplot(aes(kfr_black_p25, kfr_black_p75, size = count_black, alpha = .8)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se = F) + 
  theme_clean() + 
  scale_size_continuous(range = c(2,6)) + 
  labs(title = "Black Child HH Income By Parental Income Percentile: 75th v 25th", 
       subtitle = "Multnomah County, OR", 
       x = "Parents at 25th",
       y = "Parents at 75tth", 
       caption = "obs. size = tract size") + 
  theme(plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5)) + 
  guides(alpha = "none", 
         size = "none")


#8
# Correlation coeffs: 


#Single parenthood
Multnomah$kfr_pooled_std = (Multnomah$kfr_pooled_p25- 
                              mean(Multnomah$kfr_pooled_p25, na.rm = T))/
                              sd(Multnomah$kfr_pooled_p25, na.rm = T)

Multnomah$single_std = (Multnomah$singleparent_share2000- 
                          mean(Multnomah$singleparent_share2000, na.rm = T))/
                          sd(Multnomah$singleparent_share2000, na.rm = T)

reg2 = lm(kfr_pooled_std ~ single_std, data = Multnomah)

summary(reg2)
coeftest(reg2, vcov. = vcovHC(reg2, type = "HC1"))

confint(reg2, level = .05)

#School quality

Multnomah$test_score_std = (Multnomah$gsmn_math_g3_2013- 
                          mean(Multnomah$gsmn_math_g3_2013, na.rm = T))/
                          sd(Multnomah$gsmn_math_g3_2013, na.rm = T)

reg3 = lm(kfr_pooled_std ~ test_score_std, data = Multnomah)
summary(reg3)
coeftest(reg3, vcov. = vcovHC(reg3, type = "HC1"))



# job growth

Multnomah$job_growth_std= (Multnomah$ann_avg_job_growth_2004_2013- 
                              mean(Multnomah$ann_avg_job_growth_2004_2013, na.rm = T))/
  sd(Multnomah$ann_avg_job_growth_2004_2013, na.rm = T)

reg4 = lm(kfr_pooled_std ~ job_growth_std, data = Multnomah)
summary(reg4)
coeftest(reg4, vcov. = vcovHC(reg4, type = "HC1"))

confint(reg4, level = .95)

#jobs high pay 

Multnomah$high_pay = (Multnomah$jobs_highpay_5mi_2015- 
                             mean(Multnomah$jobs_highpay_5mi_2015, na.rm = T))/
  sd(Multnomah$jobs_highpay_5mi_2015, na.rm = T)

reg5 = lm(kfr_pooled_std ~ high_pay, data = Multnomah)
summary(reg5)*


  
#9 variation in correlation coeff by race 
Multnomah$kfr_white_std = (Multnomah$kfr_white_p25- 
                              mean(Multnomah$kfr_white_p25, na.rm = T))/
  sd(Multnomah$kfr_white_p25, na.rm = T)

Multnomah$kfr_black_std = (Multnomah$kfr_black_p25- 
                              mean(Multnomah$kfr_black_p25, na.rm = T))/
  sd(Multnomah$kfr_black_p25, na.rm = T)


#regression white 
reg_white = lm(kfr_white_std ~ single_std + test_score_std + job_growth_std + high_pay, data = Multnomah)

#regression black
reg_black = lm(kfr_black_std ~ single_std + test_score_std + job_growth_std + high_pay, data = Multnomah)

stargazer(reg_white, reg_black, type = "html", out = "D:/Users/Nataniel/Documents/EC 510/ WvB.html", 
          dep.var.labels = c("White Children", "Black Children"), 
          covariate.labels = c("Single Parent", "School Qual", "Job Growth", "High Paying Jobs"), 
          title = "HH Income (age 31-37) across Race", 
          style = "qje", 
          keep.stat = c("n", "rsq"),
          notes = "Parent In at 25th pct")
stargazer(reg_white, reg_black, type = "html", out = "D:/Users/Nataniel/Documents/EC 510/ WvBV0.html", 
          dep.var.labels = c("White Children", "Black Children"), 
          covariate.labels = c("Single Parent", "School Qual", "Job Growth"), 
          title = "HH Income (age 31-37) across Race", 
          style = "qje", 
          keep.stat = c("n", "rsq"),
          notes = "Parent In at 25th pct")
