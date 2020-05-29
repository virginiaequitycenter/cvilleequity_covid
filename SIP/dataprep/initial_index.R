####################################################
# Greater Charlottesville Region COVID Burden/Risk
####################################################
# Generate indices
# Created: 05/08/2020 mpc
# Last updated: 05/29/2020 mpc

####################################################
# Structure
# 1. Libraries and Data
# 2. Select indicators, filter data: burden and ease
# 3. Create naive burden/ease index, sum min-max normalized
# 4. REMOVED: Create naive burden index, sum rank
# 5. Join indices to burden/ease, to tract_data, and explore
# 6. Quick exploratory map
# 7. Save work
# 8. REMOVED: Wrangle for Visualization 
####################################################


# ....................................................
# 1. Libraries and Data ----
library(tidyverse)
library(psych)
library(leaflet)
library(RColorBrewer)
library(tigris)
library(viridis)
library(geojsonio)

load("tract_data_work.Rdata")


# ....................................................
# 2. Select indicators, filter data ----
# remove Albemarle tract 109.03 
tract_data2 <- tract_data %>% 
  filter(GEOID != "51003010903")

# BURDEN
burden <- tract_data2 %>% 
  select(GEOID, densityE, room15E, bb_compE, lapop1share, nocarE, und18E, und6E, vmt_mar1E)  
alpha(burden[,2:8], check.keys=TRUE)

# order bb_compE, lapop1share: high=burden, low=ease
burden <- burden %>% 
  mutate(nobbE = 100-bb_compE,
         nofoodE = 100-lapop1share) %>% 
  select(-c(bb_compE, lapop1share, vmt_mar1E, und6E))
alpha(burden[,2:7], check.keys=TRUE)

# EASE
# order room15E, nocareE, und18E: high=ease, low=burden (reverse density below)
ease <- tract_data2 %>% 
  select(GEOID, densityE, room15E, bb_compE, lapop1share, nocarE, und18E)  

ease <- ease %>% 
  mutate(carE = 100-nocarE,
         nochldE = 100-und18E,
         nocrowdE = 100-room15E,
         sparsityE = densityE) %>% 
  rename(foodE = lapop1share,
         bbcompE = bb_compE) %>% 
  select(-c(nocarE, room15E, und18E, densityE)) %>% 
  select(GEOID, sparsityE, nocrowdE, carE, nochldE, bbcompE, foodE)
alpha(ease[,2:7], check.keys=TRUE)


# ....................................................
# 3. Create naive burden/ease index, sum min-max normalized ----
minmaxnorm <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}

# BURDEN
burden_norm <- modify_if(burden, is.numeric, minmaxnorm)
burden_norm$index_norm <- rowSums(burden_norm[,2:7])
names(burden_norm)[2:7] <- str_replace(names(burden_norm)[2:7], "E", "_norm")

ggplot(burden_norm, aes(x = index_norm)) + geom_density()

# EASE
# reverse density
ease_norm <- modify_if(ease, is.numeric, minmaxnorm)
ease_norm <- ease_norm %>% 
  mutate(sparsityE = 1-sparsityE) 
ease_norm$index_norm <- rowSums(ease_norm[,2:7])
names(ease_norm)[2:7] <- str_replace(names(ease_norm)[2:7], "E", "_norm")

ggplot(ease_norm, aes(x = index_norm)) + geom_density()


# # ....................................................
# # 4. Create naive burden index, sum rank ----
# ranknorm <- function(x){
#   return(rank(x, ties.method="min"))
# }
# 
# ranknorm1_10 <- function(x) {
#   return(((x - min(x)) /( max(x) - min(x) ))*9 + 1)
# }
# 
# burden_rank <- modify_if(burden, is.numeric, ranknorm)
# burden_rank$index_rank <- rowSums(burden_rank[,2:7])
# burden_rank <-  modify_if(burden_rank, is.numeric, ranknorm1_10)
# 
# names(burden_rank)[2:7] <- str_replace(names(burden_rank)[2:7], "E", "_rank")
# 
# ggplot(burden_rank, aes(x = index_rank)) + geom_density()
# 

# ....................................................
# 5. Join indices to burden/ease, to tract_data, and explore ----

# BURDEN
names(burden)[2:7] <- str_replace(names(burden)[2:7], "E", "_percent")

burden <- burden %>% 
  left_join(burden_norm, by = c("GEOID")) 
summary(burden)

# Join burden to tract_data
tract_data <- tract_data %>% 
  left_join(burden[,c(1,14)], by = c("GEOID"))

ggplot(tract_data, aes(x = hhincE, y = index_norm)) + geom_point()

# EASE
names(ease)[2:7] <- str_replace(names(burden)[2:7], "E", "_percent")

ease <- ease %>% 
  left_join(ease_norm, by = c("GEOID")) 
summary(ease)


# ....................................................
# 6. Quick exploratory map ----
# add geometry
burden_geo <- geo_join(tract, burden, by = "GEOID")

mycol <- colorRampPalette(brewer.pal(8, "YlGnBu"))(10)
pal <- colorNumeric(palette = mycol,
                    domain = burden_geo$index_rank)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addTiles() %>% # to show streets more prominently
  addPolygons(data = burden_geo, 
              fillColor = ~pal(burden_geo$index_norm), 
              fillOpacity = 0.5, 
              color = "white",
              weight = 2, 
              smoothFactor = 0.2, 
              popup = paste0(burden_geo$index_norm, "<br>",
                             burden_geo$COUNTYFP, "<br>",
                             burden_geo$NAMELSAD),
              highlight = highlightOptions(
                weight = 5,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addPolygons(data = magdist,
              color = "grey",
              weight = 1, 
              fill = FALSE) %>% 
  addLegend(pal = pal, 
            values = burden_geo$index_norm, 
            position = "bottomright", 
            opacity = 0.5,
            title = "SIP Burden Index")


# ....................................................
# 7. Save work ----
save.image("burden_work.Rdata")
# load("burden_work.Rdata")


# MOVED TO vis_dataprep.R
# ....................................................
# 8. Save for Visualization Work ----

# Get the County Labels 
counties <- data.frame(COUNTYFP = as.character(c("003", "029", "540", "065", "079", "109", "113", "125", "137")),
           CountyName = c("Albemarle County ", 
                          "Buckingham County",
                          "Charlottesville",
                          "Fluvanna County",
                          "Greene County", 
                          "Louisa County",
                          "Madison County",
                          "Nelson County",
                          "Orange County"),
           Ordering = c(2, 1, 4, 9,7 , 6, 5, 3, 8)
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


# Label the Variables. 

var_labels <- data.frame( Domain = c(
               "sparsity", 
               "nocrowd",  
               "car",   
               "nochld",
               "bbcomp",    
               "lapop1share",
               "index"),
              Label = c(
                "Population Sparsity",
                "1.5 Ppl/Room or less",
                "Car Access",
                "No Children",
                "Broadband Access",
                "Food Access",
                "Composite Score"
              )
)

  

# Join all Data Together. 
visdata <- 
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
  left_join(var_labels) %>%
   arrange(Ordering, desc(GEOID) )

visdata %>%
  write.csv(. , file = "../cvilleequity_covid/SIP/data/burden_data.csv")
  

# Get the color pallette

viridis(27)[seq(2,27,3)] %>% rev()

burden_geo %>%
  geojson_write(. , file = "../cvilleequity_covid/SIP/data/tracts.geojson")

