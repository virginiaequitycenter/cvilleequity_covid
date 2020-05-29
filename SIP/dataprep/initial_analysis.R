# Initial analysis

library(tidyverse)
library(kableExtra)
load("burden_work.Rdata")

summary(tract_data)

# burden bins
br_norm <- quantile(tract_data$index_norm, probs = c(0,.2,.4,.6,.8,1), na.rm=TRUE)
# br_rank <- quantile(tract_data$index_rank, probs = c(0,.2,.4,.6,.8,1), na.rm=TRUE)
tract_data <- tract_data %>% 
  mutate(normbin = cut(index_norm, breaks = br_norm, labels = c("Low", "Low-Mid", "Middle", "Mid-High", "High"))) 

table(tract_data$normbin)

# means by bin: to add to viz (as table or graph)
# normed version
quintile_table <- 
tract_data %>% group_by(normbin) %>% 
  filter(!is.na(normbin)) %>% 
  summarize(index = round(mean(index_norm, na.rm=TRUE),2),
            hhinc =  paste0("$",format(round(as.numeric(mean(hhincE)), 0), big.mark=",")), # SDH
            badeg = round(mean(badegreeE),1), # SDH
            rent = round(mean(rentE),1), # SDH
            white = round(mean(whiteE),1), # dis
            immig = round(mean(forbornE),1), # dis
            insur = round(mean(hlthinsE),1), # health
            life = round(mean(life_expE, na.rm=TRUE),1)) %>%  # health
  rename(`Composite Index` = normbin,
         `Average Index`= index,
         `Average HH Income` = hhinc,
         `% With Bachelor's` = badeg,
         `% Renting` = rent,
         `% Pop White` = white,
         `% Pop Immigrant` = immig,
         `% With Insurance` = insur,
         `Avg. Life Expectancy` = life) 
       
quintile_table


quintile_table %>%
  # kable()
  kable(format = "rst")

# ranked version
tract_data %>% group_by(rankbin) %>% 
  filter(!is.na(rankbin)) %>% 
  summarize(index = round(mean(index_rank, na.rm=TRUE),2),
            hhinc = round(mean(hhincE),0), # SDH
            badeg = round(mean(badegreeE),1), # SDH
            rent = round(mean(rentE),1), # SDH
            white = round(mean(whiteE),1), # dis
            immig = round(mean(forbornE),1), # dis
            insur = round(mean(hlthinsE),1), # health
            life = round(mean(life_expE, na.rm=TRUE),1)) %>%  # health
  kable(format = "rst")

