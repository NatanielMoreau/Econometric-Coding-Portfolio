library(pacman)
p_load(tidyverse, rdrobust, ggthemes, sandwich, magrittr, janitor, broom, stargazer)


# Data Cleaning -----------------------------------------------------------

#load data
grade_data <- read_csv("grade5.csv")

# subset enrollment between 20-60
grade_narrow = grade_data %>% filter(between(school_enrollment,20,60))

# subset enrollment 0-80
grade_narrow_2 = grade_data %>% filter(between(school_enrollment,0,80))

# Visualize Data ----------------------------------------------------------

# class size by enrollment scatter
grade_narrow %>% ggplot(aes(school_enrollment, classize)) + 
  stat_summary_bin(fun.y = "mean", bins = 20, size = 3, geom = "point")+
  # rdd cutoff
  geom_vline(xintercept = 40.5)+ 
  # separate rdd trends
  geom_smooth(aes(group = school_enrollment > 40.5,
                  ),method = "lm", formula = y~x, se = F) + 
  labs(title = "Class size by Enrollment", 
       y = "class size") + 
  #clean it up
  theme_clean() + 
  theme(plot.title = element_text(hjust = .5), 
        panel.background = element_rect(fill = "lightgrey"))

# Math scores by enrollment scatter
grade_narrow %>% ggplot(aes(school_enrollment, avgmath)) + 
  stat_summary_bin(fun.y = "mean", bins = 20, size = 3, geom = "point")+
  # rdd cutoff
  geom_vline(xintercept = 40.5)+
  # separate rdd trends
  geom_smooth(aes(group = school_enrollment > 40.5,
  ),method = "lm", formula = y~poly(x,2), se = F) + 
  labs(title = "Average Math Scores by Enrollment", 
       y = "math score") +
  #clean it up
  theme_clean() + 
  theme(plot.title = element_text(hjust = .5), 
        panel.background = element_rect(fill = "lightgrey"))

# Language scores by enrollment scatter
grade_narrow %>% ggplot(aes(school_enrollment, avgverb)) + 
  stat_summary_bin(fun.y = "mean", bins = 20, size = 3, geom = "point")+
  # rdd cutoff
  geom_vline(xintercept = 40.5)+
  # separate rdd trends
  geom_smooth(aes(group = school_enrollment > 40.5,
  ),method = "lm", formula = y~poly(x,2), se = F) + 
  labs(title = "Average Language Scores by Enrollment", 
       y = "verb score") +
  #clean it up
  theme_clean() + 
  theme(plot.title = element_text(hjust = .5), 
        panel.background = element_rect(fill = "lightgrey"))


# Regressions of visualized relationships ---------------------------------------

#add vars for Rdd
grade_narrow_2 %<>% mutate(above_40 = if_else(school_enrollment > 40,1,0), 
                           enroll_centered = school_enrollment - 40, 
                           above_40_centered = above_40 * enroll_centered) %>% 
  clean_names()

## class ~ enroll
rdd_class_enroll = lm(classize ~ above_40 + enroll_centered + above_40_centered, data = grade_narrow_2)

# cluster SE by school 
se_class_enroll = sqrt(diag(vcovCL(rdd_class_enroll, cluster = grade_narrow_2$schlcode)))

# results
stargazer(rdd_class_enroll,se = se_class_enroll, type = "text")

## math ~ enroll
rdd_math_enroll = lm(avgmath ~ above_40 + enroll_centered + above_40_centered, data = grade_narrow_2)

# cluster SE 
se_math_enroll = sqrt(diag(vcovCL(rdd_math_enroll, cluster = grade_narrow_2$schlcode)))

# results
stargazer(rdd_math_enroll,se = se_math_enroll, type = "text")

## lang ~ enroll
rdd_lang_enroll = lm(avgverb ~ above_40 + enroll_centered + above_40_centered, data = grade_narrow_2)

# cluster se
se_lang_enroll = sqrt(diag(vcovCL(rdd_lang_enroll, cluster = grade_narrow_2$schlcode)))

# results 
stargazer(rdd_lang_enroll,se = se_lang_enroll, type = "text")
