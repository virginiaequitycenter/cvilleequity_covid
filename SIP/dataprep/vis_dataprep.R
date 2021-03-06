# Prep for Visualization 

# ....................................................
# 1. Libraries and Data ----
library(tidyverse)
library(viridis)
library(geojsonio)

load("burden_work.Rdata")


# ....................................................
# 2. Geography and labels ----
# Get the County Labels 
counties <- data.frame(COUNTYFP = as.character(c("003", "029", "540", "065", "079", "109", "113", "125", "137")),
                       CountyName = c("Albemarle County", 
                                      "Buckingham County",
                                      "Charlottesville",
                                      "Fluvanna County",
                                      "Greene County", 
                                      "Louisa County",
                                      "Madison County",
                                      "Nelson County",
                                      "Orange County"),
                       Ordering = c(2, 1, 4, 9, 7 , 6, 5, 3, 8)
                       # Ordering = c(8,2,9,3,7,4,6,1,5)
) 


# Get the Tract Labels
geo_labels <- 
  burden_geo@data %>%
  select(NAMELSAD, GEOID, COUNTYFP) %>%
  left_join(counties) %>%
  separate(NAMELSAD, c(NA, "NAMELSAD"), sep = "Census ") %>%
  arrange(NAMELSAD) %>%
  separate(CountyName, c("CountyName", NA), sep = " County") 

geo_labels


# ....................................................
# 2. Variable labels and join ----

# BURDEN
# Label the Variables
var_labels_burden <- data.frame( Domain = c(
  "density", 
  "nobb",    
  "nocar",   
  "nofood",  
  "room15",  
  "und18",
  "index"),
  Label = c(
    "Population Density",
    "No Broadband",
    "No Car Access",
    "Low Food Access",
    "1.5+ Ppl/Room",
    "Children Present",
    "Composite Score"
  )
)

# Join all Data Together
visdata_burden <- 
  burden %>% 
  gather(Domain, Number, -(GEOID )) %>% 
  separate(Domain, c("Domain", "Index"), sep = "_") %>% 
  mutate(Number = round(Number, 2)) %>%
  filter(!Index == "percent")  %>%
  left_join(. ,
            
            burden %>% 
              gather(Domain, Number, -(GEOID )) %>% 
              separate(Domain, c("Domain", "Index"), sep = "_") %>% 
              filter(Index == "percent") %>%
              rename(Percent = Number) %>%
              select(GEOID, Domain, Percent)
  ) %>% 
  left_join(geo_labels) %>%
  left_join(var_labels_burden) %>%
  arrange(Ordering, desc(GEOID) ) %>%
  mutate(Percent = 
           case_when(
             Domain == "density" ~ paste0(round(Percent,1), " ppl/sq. mi."),
             Domain == "room15" ~ paste0(round(Percent,2), "% Households"),
             Domain == "nocar" ~ paste0(round(Percent,2), "% Households"),
             Domain == "und18" ~ paste0(round(Percent,2), "% Families"),
             Domain == "nobb" ~ paste0(round(Percent,2), "% Households"),
             Domain == "nofood" ~ paste0(round(Percent,2), "% Households"),
             TRUE ~ paste0(Percent)
           ))

# this goes on the heat map and the map
visdata_burden %>%
  #  write.csv(. , file = "../Visualization_Scripts/SIP_Burden/data/burden_data.csv")
  write.csv(. , file = "../cvilleequity_covid/SIP/data/burden_data.csv")


# EASE
# Label the Variables
var_labels_ease <- data.frame( Domain = c(
  "sparsity", 
  "nocrowd",  
  "car",   
  "nochld",
  "bbcomp",    
  "food",
  "index"),
  Label = c(
    "Population Sparsity",
    "Less than 1.5 Ppl/Room",
    "Car Access",
    "No Children",
    "Broadband Access",
    "Food Access",
    "Composite Score"
  )
)

visdata_ease <- 
  ease %>% 
  gather(Domain, Number, -(GEOID )) %>% 
  separate(Domain, c("Domain", "Index"), sep = "_") %>% 
  mutate(Number = round(Number, 2)) %>%
  filter(!Index == "percent")  %>%
  left_join(. ,
            
            ease %>% 
              gather(Domain, Number, -(GEOID )) %>% 
              separate(Domain, c("Domain", "Index"), sep = "_") %>% 
              filter(Index == "percent") %>%
              rename(Percent = Number) %>%
              select(GEOID, Domain, Percent)
  ) %>% 
  left_join(geo_labels) %>%
  left_join(var_labels_ease) %>%
  arrange(Ordering, desc(GEOID) ) %>%
  mutate(Percent = 
           case_when(
             Domain == "sparsity" ~ paste0(round(Percent,1), " ppl/sq. mi."),
             Domain == "nocrowd" ~ paste0(round(Percent,2), "% Households"),
             Domain == "car" ~ paste0(round(Percent,2), "% Households"),
             Domain == "nochld" ~ paste0(round(Percent,2), "% Families"),
             Domain == "bbcomp" ~ paste0(round(Percent,2), "% Households"),
             Domain == "food" ~ paste0(round(Percent,2), "% Households"),
             TRUE ~ paste0(Percent)
           ))


# this goes on the heat map and the map
visdata_ease %>%
  write.csv(. , file = "../cvilleequity_covid/SIP/data/ease_data.csv")


# ....................................................
# 3. Generate SDH/context data ----
# This data goes in the tract charts
relabeler <- data.frame(Domain = c("badegreeE", "life_expE",  "hhincE", "whiteE"), 
                        Label = c("Have Bachelors Degree", "Average Life Expectancy", "Median Household Income", "% Pop White"),
                        Order = c(3, 2, 1, 4))
tract_facts <-
tract_data %>%
  select(GEOID, NAME, badegreeE, life_expE, hhincE, whiteE) %>%
  gather(Domain, Number, -c(GEOID, NAME)) %>%
  inner_join(relabeler) %>%
  mutate(COUNTYFP = substr(GEOID, 3, 5)) %>%
  inner_join(counties) %>%
  group_by(Domain) %>%
  mutate(AreaMedian = median(Number, na.rm= TRUE)) %>%
#  mutate(BelowArea = round((Number - AreaMedian)/AreaMedian*100,1) ) %>%
#  mutate(BelowArea = ifelse(BelowArea < 0, paste0(abs(BelowArea), "% Below Median"), paste0(abs(BelowArea), "% Above Median")  )) %>%
  group_by(CountyName, Domain) %>%
  mutate(CountyMedian = median(Number, na.rm= TRUE)) %>%
#  mutate(BelowCounty = round( (Number - CountyMedian)/CountyMedian*100,1)) %>%
 # mutate(BelowCounty = ifelse(BelowCounty < 0, paste0(abs(BelowCounty), "% Below Median"), paste0(abs(BelowCounty), "% Above Median"))) %>%
  select(GEOID, CountyName, NAME, Label, 
         `Tract` =  Number,
         `County` = CountyMedian, 
         `Region` =  AreaMedian,
         Order) %>%
  arrange(GEOID, Label) %>%
  gather(Stat, Value, -c(Domain, GEOID, CountyName, NAME, Label, Order)) %>% 
  mutate(Value =
           case_when(
             (Domain == "badegreeE"  ) ~ paste0(Value, "% of Adults"),
             (Domain == "life_expE") ~ paste0(Value, " Years"),
             (Domain == "hhincE") ~ paste0("$",format(round(as.numeric(Value), 0), big.mark=",")),
             (Domain == "whiteE"  ) ~ paste0(Value, "% of Pop")
           )
  )  %>%
  arrange(Order)
#  write.csv(. , file = "../Visualization_Scripts/SIP_Burden/data/tract_facts.csv")
  write.csv( tract_facts, file = "../data/tract_facts.csv")


# ....................................................
# 4. Generate disparity table data ----
# This data goes in the Quintile Charts
# REMOVED for separate analysis page

# burden bins
br_norm <- quantile(tract_data$index_norm, probs = c(0,.2,.4,.6,.8,1), na.rm=TRUE)

tract_data <- tract_data %>% 
  mutate(normbin = cut(index_norm, breaks = br_norm, labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")))


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

quintile_table %>%
  gather(Rows, values, -c(`Composite Index`)) %>%
  rename(Columns = `Composite Index`) %>%
#  write.csv(. , file = "../Visualization_Scripts/SIP_Burden/data/attributes.csv")
  write.csv(. , file = "../cvilleequity_covid/data/attributes.csv")


# ....................................................
# 5. Export geojson ----
# Get the color palette

viridis(27)[seq(2,27,3)] %>% rev()

# Export the burden data as geojson for the visualization into leaflets
burden_geo %>%
  geojson_write(. , file = "../cvilleequity_covid/data/tracts.geojson")

magdist %>% 
  geojson_write(., file = "../cvilleequity_covid/data/magdis.geojson")
