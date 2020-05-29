####################################################
# Greater Charlottesville Region COVID Burden/Risk
####################################################
# Generate indices
# Created: 05/08/2020 mpc
# Last updated: 05/13/2020 mpc

####################################################
# Structure
# 1. Libraries and Data
# 2. 2. Select indicators, filter data
# 3. Create naive burden index, sum min-max normalized
# 4. Create naive burden index, sum rank
# 5. Join indices to burden, to tract_data, and explore
# 6. Quick exploratory map
# 7. Save work
# 8. Save for Visualization Work
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

burden <- tract_data2 %>% 
  select(GEOID, densityE, room15E, bb_compE, lapop1share, nocarE, und6E, und18E, vmt_mar1E)  
alpha(burden[,2:8], check.keys=TRUE)

burden <- burden %>% 
  mutate(nobbE = 100-bb_compE,
         nofoodE = 100-lapop1share) %>% 
  select(-c(bb_compE, lapop1share, vmt_mar1E, und6E))
alpha(burden[,2:7], check.keys=TRUE)


# ....................................................
# 3. Create naive burden index, sum min-max normalized ----
minmaxnorm <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}

burden_norm <- modify_if(burden, is.numeric, minmaxnorm)
burden_norm$index_norm <- rowSums(burden_norm[,2:7])
names(burden_norm)[2:7] <- str_replace(names(burden_norm)[2:7], "E", "_norm")

ggplot(burden_norm, aes(x = index_norm)) + geom_density()


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
# 5. Join indices to burden, to tract_data, and explore ----
# Join to burden
names(burden)[2:7] <- str_replace(names(burden)[2:7], "E", "_percent")

burden <- burden %>% 
  left_join(burden_norm, by = c("GEOID")) 
summary(burden)

# Join burden to tract_data
tract_data <- tract_data %>% 
  left_join(burden[,c(1,14)], by = c("GEOID"))

ggplot(tract_data, aes(x = hhincE, y = index_norm)) + geom_point()


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
               "density", 
                "nobb",    
                "nocar",   
                "nofood",  
                "room15",  
                "und6",
               "index"),
              Label = c(
                "Population Density",
                "No Broadband",
                "No Car Access",
                "Low Food Access",
                ">1.5 Ppl/Room",
                "Children Aged <6",
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
  write.csv(. , file = "Visualization_Scripts/Development/burden_data.csv")
  

# Get the color pallette

viridis(27)[seq(2,27,3)] %>% rev()

burden_geo %>%
  geojson_write(. , file = "Visualization_Scripts/Development/tracts.geojson")

