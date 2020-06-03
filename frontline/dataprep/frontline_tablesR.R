# VA PUMS, Industry data

library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(expss) # This deals with the labelling nightmare
library(DataExplorer)

pums <- read_dta("vapums/acs_1418_frontline.dta")
pums <- read_dta("acs_1418_frontline.dta")

# tab lfstat [fw=perwgt]
count(pums, lfstat)
count(pums, lfstat, wt = perwgt)

# tab flind1 [fw=perwgt] if lfstat == 1 
pums %>% filter(lfstat == 1 & !is.na(flind1)) %>% 
  group_by(flind1) %>% 
  tally(wt = perwgt) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = flind1, y = prop)) + geom_col()


# tab female flind1 [fw=perwgt] if lfstat == 1, col	
pums %>% filter(lfstat == 1 & !is.na(flind1)) %>% 
  group_by(flind1, female) %>% 
  tally(wt = perwgt) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = flind1, y = prop, fill = as.factor(female))) + geom_col()


#############################
# Sam's Exploration Sandbox #

# View(pums)
# 
# introduce(pums)
# plot_intro(pums)
# plot_missing(pums)
# plot_bar(pums)
# plot_correlation(na.omit(pums), maxcat = 5L)

# There is a decent bit of missing data. Not sure what this is all about. Ask Michele, particularly 
# about the missing sample weights because that doesn't quite work.
# MPC: these are household surveys that aren't matched to person surveys

pums_survey <-  pums %>%
  filter( !is.na(perwgt) ) %>%  # Why are there so many without sample weights?
   as_survey( 
    weights = perwgt
    ) 


### Front Line ###

pums_survey  %>%
  mutate(frontline = as.factor(flind)) %>%
  filter(lfstat == 1 & !is.na(frontline)) %>%
  group_by( frontline) %>%
  summarize(
    percent = survey_mean(),
    count = survey_total(),
    unweighted = unweighted(n())
  ) %>%
  mutate(
    percent = round(percent*100,2) # above tells me 'percent' not found
  )



### Breakdown Within Frontline ###
pums_survey  %>%
  filter(lfstat == 1 & !is.na(flind1)) %>%
   mutate(flind1 = as.factor(flind1)) %>%
   group_by( flind1) %>%
   summarize(
     percent = survey_mean(),
     count = survey_total(),
     unweighted = unweighted(n())
            ) %>%
  mutate(
    percent = round(percent*100,2)
  )

pums_survey  %>%
  filter(lfstat == 1 & !is.na(flind1)) %>%
  mutate(flind1 = as.factor(flind1)) %>%
  group_by( flind1) %>%
  summarize(
    percent = survey_mean(),
    count = survey_total(),
    unweighted = unweighted(n())
  ) %>%
  mutate(
    percent = round(percent*100,2) 
  )

#######################################
# Programatically generate the tables # 

### Relabel Data of Interest ###
check_these<- c("wbhao", "forborn", "agegrp", "educ", "female", "hins", "hhoc_b", "flind1")

lapply(check_these, function(x) {
  name <-  attr(pums %>% pull(x), "label") # is it titled already and do we like that label?
  labels <- data.frame(labels = rownames(as.data.frame(attr(pums %>% pull(x), "labels")))) # Are the Factors labelled and do we like them?
  list(x, name, labels)
  }
)


### Run the actual recoding ###
pums_cleaned <- apply_labels(pums, 
                            wbhao = "Race/Ethnicity",
                            
                            agegrp = "Age Group",
                            
                            female = "Gender",
                            female = num_lab("
                                    0 Male
                                    1 Female
                                             "),
                            
                            flind1 = num_lab("
                                    1 Grocery, Convenience, & Drug Stores           
                                    2 Public Transit          
                                    3 Trucking, Warehouse, & Postal Service         
                                    4 Building Cleaning Services
                                    5 Health Care
                                    6 Childcare & Social Services"),
                            
                            forborn = num_lab("0 Non-Immigrant
                                               1 Immigrant"),
                            
                            hins = "Health Insurance",
                            hins = num_lab("0 Uninsured 
                                            1 Insured"),
                            
                            educ = num_lab("
                                           1         LTHS
                                           2           HS
                                           3 Some college
                                           4      College
                                           5     Advanced"))

### Create New Survey Element ###
pums_cleaned_survey <-  pums_cleaned %>%
  filter( !is.na(perwgt) ) %>%  
  as_survey( 
     weights = perwgt
  ) 

### Automate Table Creation ### 

demo_input <- c("wbhao", "agegrp", "educ", "female", "forborn", "hins")
                   
all_demographics <- 
 map_df( demo_input,     
   # Restrict to Frontline Workers to get the total
         
   ~ pums_cleaned_survey  %>%
          rename("Level"=!!sym(.x)) %>%
          filter(lfstat == 1 & flind == 1) %>%
          mutate(Level = as.factor(Level)) %>%
          group_by(Level) %>%
          summarize(
            percent = survey_mean(),
            count = survey_total(),
            unweighted_count = unweighted(n())
            ) %>%
      mutate(
        percent = round(percent*100,2),
        total = sum(count),
       unweighted_total = sum(unweighted_count)
      ) %>%
      mutate(Job = "Total", Demographic = attr(pums_cleaned %>% pull(.x), "label"), varname = .x ) %>%
      select(Job, varname, Demographic, Level, percent, count, total,  unweighted_count, unweighted_total) 
 )
  
 
job_demographics <- 
  map_df( demo_input,     
    # Now Group By Job
    
    ~ pums_cleaned_survey  %>%
      rename("Level"=!!sym(.x), Job = flind1) %>%
      filter(lfstat == 1 & flind == 1) %>%
      mutate(Level = as.factor(Level),
             Job = as.factor(Job)) %>%
      group_by(Job, Level) %>%
      summarize(
        percent = survey_mean(),
        count = survey_total(),
        unweighted_count = unweighted(n())
      ) %>%
      group_by(Job) %>%
      mutate(
        percent = round(percent*100,2),
        total = sum(count),
        unweighted_total = sum(unweighted_count)
      )  %>%
      mutate( Demographic = attr(pums_cleaned %>% pull(.x), "label"), varname = .x ) %>%
      select(Job, varname, Demographic, Level, percent, count, total,  unweighted_count, unweighted_total)

     ) 

# add demographic breakdowns for total labor force for comparison
full_demographics <- 
  map_df( demo_input,     
          # Restrict to Frontline Workers to get the total
          
          ~ pums_cleaned_survey  %>%
            rename("Level"=!!sym(.x)) %>%
            filter(lfstat == 1) %>%
            mutate(Level = as.factor(Level)) %>%
            group_by(Level) %>%
            summarize(
              percent = survey_mean(),
              count = survey_total(),
              unweighted_count = unweighted(n())
            ) %>%
            mutate(
              percent = round(percent*100,2),
              total = sum(count),
              unweighted_total = sum(unweighted_count)
            ) %>%
            mutate(Job = "All Workers", Demographic = attr(pums_cleaned %>% pull(.x), "label"), varname = .x ) %>%
            select(Job, varname, Demographic, Level, percent, count, total,  unweighted_count, unweighted_total)
  ) %>% filter(!is.na(percent))

demographics <- bind_rows(all_demographics, job_demographics, full_demographics)

write.csv(demographics, file= "vapums/table_from_r.csv")


# exploration
# Working out some graph possibilities
fl <- read.csv("vapums/table_from_r.csv")
fl <- read.csv("table_from_r.csv")

fl4 <- fl %>% 
  filter(Demographic %in% c("Race/Ethnicity", "Age Group", "Education level", "Gender")) %>% 
  droplevels() %>% 
  mutate(Demographic = fct_relevel(Demographic, "Race/Ethnicity")) %>%
  mutate(Level = factor(Level, levels = c("Black", "Hispanic", "AAPI", "Other", "White", 
                             "16-24", "25-34", "35-54", "55-64", "65+",
                             "LTHS", "HS", "Some college", "College", "Advanced", 
                             "Female", "Male")))

pal5 <- brewer.pal(5, "PuBuGn")

fl4 %>%  
  filter(Job %in% c("Total", "All Workers")) %>% 
  ggplot(aes(x = Job, y = percent, fill = Level)) + geom_col() + 
  scale_x_discrete(labels=c("All Workers" = "All", "Total" = "Frontline")) +
  scale_fill_manual(values =c(pal5, pal5, pal5, pal5[c(2,4)])) +
  facet_wrap(~Demographic, nrow = 1) + guides(fill = guide_legend(nrow=5)) +
  theme(axis.title.x = element_blank(), legend.position="bottom", legend.title=element_blank())

# play with palettes
palg = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

qplot(x=1:8, y = 1, fill=factor(1:8), geom="tile") +
  scale_fill_manual(values = palg) +
  theme_void()+
  theme(legend.position="none")

paltmp <- colorRampPalette(palg)
paltmp(10)

qplot(x=1:10, y = 1, fill=factor(1:10), geom="tile") +
  scale_fill_manual(values = paltmp(10)) +
  theme_void()+
  theme(legend.position="none")

# switch to small multiples
fl4 <- fl4 %>% 
  mutate(Job = fct_reorder(Job, total))

fl4 %>% 
  ggplot(aes(x = Job, y = percent, fill = Level)) +
  geom_col(position="dodge") + coord_flip() + 
  labs(title = "Frontline Workers by Race", y = "Percent") +
  scale_fill_manual(values =c(palg[c(2,3,5,7,8)], palg[c(1,2,4,6,8)], palg[c(4,5,6,7,8)], palg[c(3,5)])) +
  facet_wrap(~Demographic) +
  theme(axis.title.y = element_blank(),
        legend.position="bottom")
# just doesn't translate well to case with different outcomes (Levels)

