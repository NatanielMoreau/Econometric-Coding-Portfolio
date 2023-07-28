# Prep --------------------------------------------------------------------
library(pacman)
p_load(tidyverse, sandwich, ggthemes, stargazer, broom)

# change names 
better_vroom = read.csv("2021C00-63.csv")
vroom = read.csv('Electric_Vehicle_Population_Data.csv')
vroom = rename(vroom, rng = "Electric.Range", 
               clean_elgb = "Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility")

# clean variables 
vroom = vroom %>% 
  filter(State == "WA",
         Model.Year != 2022, 
         Electric.Vehicle.Type == "Battery Electric Vehicle (BEV)") %>% 
        mutate(clean_elgb = ifelse(clean_elgb == "Clean Alternative Fuel Vehicle Eligible", TRUE, FALSE), 
         County = as_factor(County), 
         Model = as_factor(Model),
         party = ifelse(County=="King"|County=="Island"|County=="Snohomish"|County=="Thurston"|County=="Skagit"
                        |County=="Kitsap"|County=="Clallam"|County=="Clark"|County=="Jefferson"|County=="Pierce"
                        |County=="San Juan"|County=="Whatcom"|County=="Whitman",1,0))

# Visualization----------------------------------------------------------

# freq table of Makes 
t1 = table(vroom$Make)
t1 = data.frame(t1)
t3 = t1 %>% filter(Freq > 260)

# share Ev by county table
t2 = table(vroom$County)
t2 = data.frame(t2)
t2 = rename(t2, County = Var1)
t2$County = toupper(t2$County)


# total cars by county
by_county = better_vroom %>% 
  group_by(County) %>%
  summarize(total_car = sum(Total))

# clean & join
by_county = by_county[-c(1,41),]
by_county$County = substr(by_county$County,4,15)
view(by_county)
t2 = left_join(t2, by_county, by = "County")
t2 = t2 %>% mutate(shr_ev = Freq/total_car)

# visualizing data 
vroom %>%
  ggplot(aes(rng)) + 
  geom_density()

# Competitive manufacturers 
vroom1 =vroom %>% filter(Make =="AUDI"|Make == "BMW"|Make =="CHEVROLET"|Make =="FORD"
                         |Make =="KIA"|Make =="NISSAN"|Make =="TESLA"|
                          Make =="SMART"|Make =="VOLKSWAGEN"|Make =="VOLVO"|
                           Make == "HYUNDAI") %>% 
  mutate(Make = as_factor(Make), 
         Make = relevel(Make, "TESLA"))
  

# Range x Model year 
vroom1 %>% 
  ggplot(aes(Model.Year, rng))+ 
  geom_point(aes(color = Make), size = 2) + 
  labs(title = "Electric Range by Model Yr", 
       x = "Model Year", 
       y = "Range (mi)") + 
  theme_clean() + 
  theme(plot.title = element_text(hjust = .5, size = 13), 
        panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major.y = element_line(colour = "black"))

# Model popularity 
t3 %>% 
  ggplot(aes(Var1,Freq)) + 
  geom_col(fill = "cadetblue", alpha = .8) + 
  labs(title = "Popularity by Brand", 
       subtitle = "Brands below median (260) cars not included",
       x = "Model", 
       y = "count") +
  geom_text(aes(label = signif(Freq)), nudge_y = 550) +
  theme_clean() + 
  theme(panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major.y = element_line(colour = "black"), 
        plot.title = element_text(hjust = .5, size = 13))

# Percent ev of total cars
t2 %>% filter(Freq >500) %>% 
  ggplot(aes(County, shr_ev*100)) + 
  geom_col(fill = "cadetblue", alpha = .8) + 
  labs(title = "Pct share of EV's by County", 
       x = "County", 
       y = "% Share of EV's") + 
  geom_text(aes(label = signif(shr_ev*100,3)), nudge_y = .040)+
  theme_clean() +
  theme(panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major.y = element_line(colour = "black"), 
        plot.title = element_text(hjust = .5, size = 13))

# Bar of Tesla models
vroom %>% filter(Make == "TESLA", 
                 Model != "ROADSTER") %>% 
  ggplot(aes(Model)) + 
  labs(title = "Most popular Tesla Models") +
  geom_bar(fill = "cadetblue", alpha = .8) + 
  geom_text(aes(label = ..count..), stat = "count", nudge_y = -200) +
  theme(panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major.y = element_line(colour = "grey"), 
        plot.title = element_text(hjust = .5, size = 13))

# Regression Analysis -----------------------------------------------------
# car differences by party
reg1 = lm(party ~ clean_elgb + rng + Model.Year, data = vroom)
se_1 = sqrt(diag(vcovHC(reg1)))

reg2 = lm(clean_elgb ~ Make, data = vroom1)
se_2 = sqrt(diag(vcovHC(reg2)))

reg3 = lm()


stargazer(reg1, se = se_1 ,type = "html", 
          covariate.labels = c("Eligible Clean Fuel Alter.", "Range"), 
          dep.var.labels = "In democratic County",
          no.space = TRUE,
          keep.stat = c("n", "f"),
          style = "ajs" ,out = "D:/Users/Nataniel/Documents/EC 523/Blog draft 1/table1.html")

vroom2 =vroom %>% filter(Make =="NISSAN"| Make =="TESLA"|
                           Make =="SMART"|Make =="CHEVROLET") %>% 
  mutate(Make = as_factor(Make), 
         Make = relevel(Make, "TESLA"))

reg3 = lm(clean_elgb ~ Make, data = vroom2)
se_3 = sqrt(diag(vcovHC(reg3)))
stargazer(reg3, se = se_3,
          type = "text",
          covariate.labels = c("Chevrolet","SmartCar","Nissan","Tesla (c)"),
          no.space = TRUE,
          dep.var.labels = "Prob. Clean Fuel Alter.", 
          keep.stat = c("n", "f", "ser"), 
          style = "ajs", 
          out = "D:/Users/Nataniel/Documents/EC 523/Blog draft 1/table2.html")
