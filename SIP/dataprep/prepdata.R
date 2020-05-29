####################################################
# Greater Charlottesville Region COVID Burden/Risk
####################################################
# Acquire/Prep Data
# Created: 05/02/2020 mpc
# Last updated: 05/14/2020 mpc

# Definitions
# * Burden: the challenges imposed by sheltering in place
# * Risk: potential for more adverse effects of pandemic (health, economic)
# * Indicators for analysis: dimensions along which to compare burden, risk regarding questions of equity

# Metrics for Burden: ACS
# * Density (with geometry)
# * Persons in household 
# * Children in household
# ** Multigenerational household (only available in 2010 decennial)
# * Internet access
# * No car household
# Self-sufficiency: ala orange dot - https://www.pvcc.edu/files/media/orange_dot_project_3.0.2018.online.pdf
#    empirically, this is just ACS data on families earning < $35k

# Metrics for Burden: other sources
# * Food access: https://www.ers.usda.gov/data-products/food-access-research-atlas/
# * Vehicle miles traveled (e.g., pre-covid transportation patterns): https://www.streetlightdata.com/VMT-monitor-by-county/#emergency-map-response

# Metrics for Risk: ACS
# * Age 65 and over
# * Renters
# * Health insurance
# * Occupation sector (vulnerable jobs)
# Metrics for Risk: other sources
# * Life expectancy: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html
# ** Unemployment?: 
#    laus: https://virginiaworks.com/local-area-unemployment-statistics-laus or https://www.bls.gov/lau/
#    unemployment claims: https://www.vec.virginia.gov/ui-claims-dashboard or https://www.vec.virginia.gov/node/11772 or https://www.vec.virginia.gov/sites/default/files/news-11772-Published%20County%20and%20City%20Week%20Ending%20March%2028%202020.xlsx

# Metrics for Analysis: ACS
# * Race/ethnicity
# * Immigrant status
# * Poverty, Income/poverty ratio
# * Median Income
# * Disability
# * Educational attainment

# Geography: Tracts in 
#  Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
####################################################
# Structure
# 1. Provide api key (if needed), identify variables
# 2. Define ACS variables, pull data
# 3. Reduce, derive calculated estimates, combine
# 4. Add other data sources
# 5. Get shapefiles
# 6. Summarize/Examine
# 7. Save
####################################################

# Load libraries
library(tidyverse)
library(tidycensus)
library(readxl)
library(tigris)
library(sf)


# ....................................................
# 1. Provide api key (if needed), identify variables ----

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key
# readRenviron("~/.Renviron")
# Variable view helper
# acs_var <- load_variables(2018, "acs5", cache = TRUE)
# acs_var <- load_variables(2018, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2018, "acs5/profile", cache = TRUE)
# dec_var <- load_variables(2010, "sf1", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - (Area)
##  - Two or more occupants per room -- B25014_007+B25014_013/B25014_001
##  - Presence of own children under 6/under 18 -- B11003_004+B11003_005+B11003_011+B11003_012+B11003_017+B11003_018/B11003_001 (0-17: B11003_003+B11003_010+B11003_016)
##  - Multigenerational households -- B11017_002/B11017_001
##  - Computer and Broadband subscription in household -- B28003_004/B28003_001
##  - Renter rates -- B25003_003/B25003_001 
##  - No vehicle -- B25044_003+B25044_010/B25044_001
##  - Occupation -- S2401
##  - Self sufficiency (family income) -- DP03_0076P ,DP03_0077P, DP03_0078P, DP03_0079P
##  - Percent white alone -- DP05_0077P
##  - Percent black or African American alone -- DP05_0078P
##  - Percent American Indian and Alaska Native alone -- DP05_0079P
##  - Percent Asian alone -- DP05_0080P
##  - Percent Native Hawaiian and Other Pacific Islander alone -- DP05_0081P
##  - Percent Some other race alone -- DP05_0082P
##  - Percent Two or more races -- DP05_0083P
##  - Percent Hispanic or Latino -- DP05_0071P
##  - Foreign born -- B05012_003/B05012_001
##  - Poverty rate -- S1701_C03_001
##  - Median HH Income -- S1901_C01_012	
##  - Median family income -- S1901_C02_012
##  - Percent with health insurance (Civilian noninstitutionalized population) -- S2701_C03_001	
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- S2704_C03_001
##  - With a disability -- B18101_004+B18101_007+B18101_010+B18101_013+B18101_016+B18101_019+B18101_023+B18101_026+B18101_029+B18101_032+B18101_035+B18101_038/B18101_001
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent incomes over 200% of poverty level -- C17002_008/C17002_001
##  - Age, 65 and over --S0101_C02_030


# ....................................................
# 2. Define ACS variables, pull data ----

# List of desired localities by FIPS
ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties
# - 003 Albemarle County  
# - 029 Buckingham County
# - 540 Charlottesville
# - 065 Fluvanna County
# - 079 Greene County 
# - 109 Louisa County
# - 113 Madison County
# - 125 Nelson County
# - 137 Orange County
ccode <- ccode %>% 
  mutate(fips = paste0("51", code))

# Get Data
year = 2018

# variables: define varlist
varlist_s = c("S1701_C03_001",   # povrate
              "S1901_C01_012",   # hhinc
              "S1901_C02_012",   # faminc
              "S2701_C03_001",   # hlthins
              "S2704_C03_001",   # pubins
              "S1501_C02_015",   # ba degree
              "S0101_C02_030")   # 65 and over

varlist_b = c("B01003_001", # totalpop
              "B25014_006", # 1.5-2 per room/owner
              "B25014_007", # 2 or more per room/owner
              "B25014_012", # 1.5-2 per room/renter
              "B25014_013", # 2 or more per room/renter
              "B25014_001", # total
              "B11003_004", # married, children under 6
              "B11003_005", # married, children under 6 and 6-17
              "B11003_011", # male hhr, children under 6
              "B11003_012", # male hhr, children under 6 and 6-17 
              "B11003_017", # female hhr, children under 6
              "B11003_018", # female hhr, children under 6 and 6-17
              "B11003_003", # married, children under 18
              "B11003_010", # male hhr, children under 18
              "B11003_016", # female hhr, children under 18
              "B11003_001", # total families
              # "B11017_002", # multigenerational hh (only in decennial)
              # "B11017_001", # total hh
              "B28003_004", # hh with computer and broadband
              "B28003_001", # total hh
              "B25003_003", # renter occupied
              "B25003_001", # total hh
              "B25044_003", # no vehicle/own
              "B25044_010", # no vehicle/rent
              "B25044_001", # no vehicle/den
              "B05012_003", # foreign born
              "B05012_001", # all
              "B18101_004", # m/u5 dis
              "B18101_007", # m/5-17 dis
              "B18101_010", # m/18-34 dis
              "B18101_013", # m/35-64 dis
              "B18101_016", # m/65-74 dis
              "B18101_019", # m/75+ dis
              "B18101_023", # f/u5 dis
              "B18101_026", # f/5-17 dis
              "B18101_029", # f/18-34 dis
              "B18101_032", # f/35-64 dis
              "B18101_035", # f/65-74 dis
              "B18101_038", # f/75+ dis
              "B18101_001", # total
              "C17002_008", # incpov ratio 2+
              "C17002_001") # incpov ratio/

varlist_d <- c("DP03_0076P", # percent less than 10K
               "DP03_0077P", # percent 10-15K
               "DP03_0078P", # percent 15-25K
               "DP03_0079P") # percent 25-35K


# pull variables
tract_data_s <- get_acs(geography = "tract",
                        variables = varlist_s,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = year, 
                        output = "wide")

tract_data_b <- get_acs(geography = "tract",
                        variables = varlist_b,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = year, 
                        output = "wide")

tract_data_d <- get_acs(geography = "tract",
                        variables = varlist_d,
                        state = "VA", 
                        county = region,
                        survey = "acs5",
                        year = year,
                        output = "tidy")
  
# pull tables (easier to just pull tables separately)
tract_race <- get_acs(geography = "tract", 
                      table = "DP05", 
                      state = "VA", 
                      county = region, 
                      survey = "acs5",
                      year = year)

tract_occ <- get_acs(geography = "tract", 
                      table = "S2401", 
                      state = "VA", 
                      county = region, 
                      survey = "acs5",
                      year = year)

# rename variables
names(tract_data_s) = c("GEOID", "NAME",
                        "povrateE", "povrateM",
                        "hhincE", "hhincM",
                        "famincE", "famincM",
                        "hlthinsE", "hlthinsM",
                        "pubinsE", "pubinsM",
                        "badegreeE", "badegreeM",
                        "over65E", "over65M")

names(tract_data_b) = c("GEOID", "NAME",  
                        "totalpopE", "totalpopM",
                        "oneroom_ownE", "oneroom_ownM",
                        "tworoom_ownE", "tworoom_ownM",
                        "oneroom_rentE", "oneroom_rentM",
                        "tworoom_rentE", "tworoom_rentM",
                        "room_denE", "room_denM",
                        "und6a_marrE", "und6a_marrM",
                        "und6b_marrE", "und6b_marrM",
                        "und6a_mhhE", "und6a_mhhM",
                        "und6b_mhhE", "und6b_mhhM",
                        "und6a_fhhE", "und6a_fhhM",
                        "und6b_fhhE", "und6b_fhhM",
                        "und18_marrE", "und18_marrM",
                        "und18_mhhE", "und18_mhhM",
                        "und18_fhhE", "und18_fhhM",
                        "und6_denE", "und6_denM",
                        # "multihhE", "multihhM",
                        # "multidenE", "multidenM",
                        "comp_bbE", "comp_bbM",
                        "comb_denE", "comp_denM",
                        "rent_occE", "rent_occM",
                        "rent_denE", "rent_denM",
                        "nocar_ownE", "nocar_ownM",
                        "nocar_rentE", "nocar_rentM",
                        "nocar_denE", "nocar_denM",
                        "for_bornE", "for_bornM",
                        "for_denE", "for_denM",
                        "dis_m5E", "dis_m5M",
                        "dis_m17E", "dis_m17M",
                        "dis_m34E", "dis_m34M",
                        "dis_m64E", "dis_m64M",
                        "dis_m74E", "dis_m74M",
                        "dis_m75E", "dis_m75M",
                        "dis_f5E", "dis_f5M",
                        "dis_f17E", "dis_f17M",
                        "dis_f34E", "dis_f34M",
                        "dis_f64E", "dis_f64M",
                        "dis_f74E", "dis_f74M",
                        "dis_f75E", "dis_f75M",
                        "dis_denE", "dis_denM",
                        "incpov_2E", "incpov_2M",
                        "incpov_denE", "incpov_denM")

                        
# ....................................................
# 3. Reduce, derive calculated estimates, combine ----

# Derive variables: tract_data_b
tract_data_b <- tract_data_b %>% 
  mutate(room15_sumE = oneroom_ownE+tworoom_ownE+oneroom_rentE+tworoom_rentE,
         room15_sumM = (oneroom_ownM/1.645)^2 + (tworoom_ownM/1.645)^2 + (oneroom_rentM/1.645)^2 + (tworoom_rentM/1.645)^2,
         room15_sumM = sqrt(room15_sumM)*1.645,
         room15E = round((room15_sumE/room_denE)*100, 1),
         room15M = moe_prop(room15_sumE, room_denE, room15_sumM, room_denM),
         room15M = round(room15M*100,1)) %>% 
  mutate(und6_sumE = und6a_marrE+und6b_marrE+und6a_mhhE+und6b_mhhE+und6a_fhhE+und6b_fhhE,
         und6_sumM = (und6a_marrM/1.645)^2 + (und6b_marrM/1.645)^2 + (und6a_mhhM/1.645)^2 +
           (und6b_mhhM/1.645)^2 + (und6a_fhhM/1.645)^2 + (und6b_fhhM/1.645)^2,
         und6_sumM = sqrt(und6_sumM)*1.645,
         und6E = round((und6_sumE/und6_denE)*100, 1),
         und6M = moe_prop(und6_sumE, und6_denE, und6_sumM, und6_denM),
         und6M = round(und6M*100,1)) %>% 
  mutate(und18_sumE = und18_marrE + und18_mhhE + und18_fhhE,
         und18_sumM = (und18_marrM/1.645)^2 + (und18_mhhM/1.645)^2 + (und18_fhhM/1.645)^2,
         und18_sumM = sqrt(und18_sumM)*1.645,
         und18E = round((und18_sumE/und6_denE)*100, 1),
         und18M = moe_prop(und18_sumE, und6_denE, und18_sumM, und6_denM),
         und18M = round(und18M*100,1)) %>% 
  # mutate(multigenE = round((multihhE/multidenE)*100,1),
  #        multigenM = moe_prop(multihhE, multidenE, multihhM, multidenM),
  #        multigenM = round(multigenM*100, 1)) %>% 
  mutate(bb_compE = round((comp_bbE/comb_denE)*100, 1),
         bb_compM = moe_prop(comp_bbE, comb_denE, comp_bbM, comp_denM),
         bb_compM = round(bb_compM*100, 1)) %>% 
  mutate(rentE = round((rent_occE/rent_denE)*100, 1),
         rentM = moe_prop(rent_occE, rent_denE, rent_occM, rent_denM),
         rentM = round(rentM*100, 1)) %>% 
  mutate(nocar_sumE = nocar_ownE + nocar_rentE,
         nocar_sumM = (nocar_ownM/1.645)^2 + (nocar_rentM/1.645)^2,
         nocar_sumM = sqrt(nocar_sumM)*1.645,
         nocarE = round((nocar_sumE/nocar_denE)*100, 1),
         nocarM = moe_prop(nocar_sumE, nocar_denE, nocar_sumM, nocar_denM),
         nocarM = round(nocarM*100,1)) %>% 
  mutate(forbornE = round((for_bornE/for_denE)*100, 1),
         forbornM = moe_prop(for_bornE, for_denE, for_bornM, for_denM),
         forbornM = round(forbornM*100, 1)) %>% 
  mutate(disab_sumE = dis_m5E+dis_m17E+dis_m34E+dis_m64E+dis_m74E+dis_m75E+
           dis_f5E+dis_f17E+dis_f34E+dis_f64E+dis_f74E+dis_f75E,
         disab_sumM = (dis_m5M/1.645)^2 + (dis_m17M/1.645)^2 + (dis_m34M/1.645)^2 +
           (dis_m64M/1.645)^2 + (dis_m74M/1.645)^2 + (dis_m75M/1.645)^2 + 
           (dis_f5M/1.645)^2 + (dis_f17M/1.645)^2 + (dis_f34M/1.645)^2 +
           (dis_f64M/1.645)^2 + (dis_f74M/1.645)^2 + (dis_f75M/1.645)^2,
         disab_sumM = sqrt(disab_sumM)*1.645,
         disabE = round((disab_sumE/dis_denE)*100, 1),
         disabM = moe_prop(disab_sumE, dis_denE, disab_sumM, dis_denM),
         disabM = round(disabM*100,1)) %>%
  mutate(incpov2E = round((incpov_2E/incpov_denE)*100,1),
         incpov2M = moe_prop(incpov_2E, incpov_denE, incpov_2M, incpov_denM),
         incpov2M = round(incpov2M*100, 1)) %>% 
  select(-c(room15_sumE, room15_sumM, und6_sumE, und6_sumM, und18_sumE, und18_sumM,
            nocar_sumE, nocar_sumM, disab_sumE, disab_sumM, oneroom_ownE:incpov_denM))

# Derive variables: tract_data_d
tract_data_d <- tract_data_d %>% 
  group_by(GEOID) %>% 
  summarize(und35kE = sum(estimate), 
            und35kM = moe_sum(moe, estimate)) %>% 
  ungroup() %>% 
  mutate(und35kE = round(und35kE, 1),
         und35kM = round(und35kM, 1))


# Reduce tables: tract_race
# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander 
#             combined due to very small values
tract_white <- tract_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

tract_ltnx <- tract_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)

tract_blailx <- tract_race %>% 
  filter(variable %in% c("DP05_0078P","DP05_0079P","DP05_0071P")) %>% 
  group_by(GEOID) %>% 
  summarize(blailxE = sum(estimate),
            blailxM = moe_sum(estimate, moe)) %>% 
  ungroup()

# Reduce tables: tract_occ
#   This list is super speculative -- and almost certainly not quite right
#   Tried to translate from mckinsey report: https://www.mckinsey.com/industries/public-sector/our-insights/lives-and-livelihoods-assessing-the-near-term-impact-of-covid-19-on-us-workers
#   And a second mckinsey report referenced inside this one (emphasis on likely job loss)
#   Definitely needs more research, possibly alternative (less evil) sources
#   a possiblity: https://adpemploymentreport.com/2020/April/NER/NER-April-2020.aspx
occlist <- c("S2401_C01_011", # community and social service occupations
             "S2401_C01_014", # arts, design, entertainment, sports and media occupations
             "S2401_C01_023", # food preparation and serving related occupations
             "S2401_C01_024", # building and grounds cleaning and maintenance occupations
             "S2401_C01_025", # personal care and service occupations
             "S2401_C01_026", # sales and office occupations
             "S2401_C01_032", # installation, maintenance, and repair occupations
             "S2401_C01_035", # transportation occupations
             "S2401_C01_036") # material moving occupations
tract_occnum <- tract_occ %>% 
  filter(variable %in% occlist) %>% 
  group_by(GEOID) %>% 
  summarize(occvE = sum(estimate),
            occvM = moe_sum(estimate, moe))

tract_occden <- tract_occ %>% 
  filter(variable == "S2401_C01_001") %>% 
  select(-variable) %>% 
  rename(occdenE = estimate,
         occdenM = moe)

tract_occ2 <- tract_occnum %>% 
  left_join(tract_occden) %>% 
  mutate(vuljobE = round((occvE/occdenE)*100, 1),
         vuljobM = moe_prop(occvE, occdenE, occvM, occdenM),
         vuljobM = round(vuljobM*100, 1)) %>% 
  select(-c(occvE, occvM, occdenE, occdenM))

# Combine ACS indicators
tract_data <- tract_data_s %>% 
  left_join(tract_data_b) %>% 
  left_join(tract_data_d) %>% 
  left_join(tract_occ2) %>% 
  left_join(tract_white) %>% 
  left_join(tract_black) %>% 
  left_join(tract_indig) %>% 
  left_join(tract_asian) %>% 
  left_join(tract_othrace) %>% 
  left_join(tract_multi) %>% 
  left_join(tract_ltnx) %>% 
  left_join(tract_blailx)

tract_data <- tract_data %>% 
  mutate(year = "2018") 

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

# clean up
rm(acs_var, tract_asian, tract_black, tract_data_b, tract_data_s, tract_data_d, 
   tract_occ, tract_occ2, tract_occden, tract_occnum, tract_indig, 
   tract_ltnx, tract_multi, tract_othrace, tract_blailx, tract_race, tract_white,
   varlist_b, varlist_s, varlist_d, occlist, year)

# ....................................................
# 4. Other data sources ----

# Metrics from other sources for Burden
# * Food access
# * Vehicle miles travelled

# Food access (research atlas)
# url <- "https://www.ers.usda.gov/webdocs/DataFiles/80591/DataDownload2015.xlsx?v=0"
# download.file(url, destfile="tempdata/fara.xlsx", mode = "wb", method="libcurl")
fara <- read_excel("tempdata/fara.xlsx", sheet = 3)

fara <- fara %>% 
  mutate(tract = CensusTract) %>% 
  separate(tract, into = c("fips", "tract"), 
           sep = c(5)) %>% 
  filter(fips %in% ccode$fips) %>% 
  select(CensusTract:POP2010, lapop1:lapop1share, fips:tract)
summary(fara)

# Vehicles miles travelled
# From https://www.streetlightdata.com/VMT-monitor-by-county/#emergency-map-response
# data requested 4/23 and received 4/25
vmt <- read_csv("tempdata/VMT Map Data 04-25-2020.csv")
vmt <- vmt %>% 
  mutate(fips = paste0(statefp10, countyfp10)) %>% 
  filter(fips %in% ccode$fips)

# plot interest
library(plotly)
p <- vmt %>% 
  ggplot(aes(x = ref_dt, y = county_vmt, color = county_name)) + 
  geom_line(size = 0.2)  
ggplotly(p)

# Compare Mar 1 estimate (Sunday before things hit; non-workday to focus on travel for goods/services/leisure)
#    to Mar 1-Mar 6 average (week before things hit, to capture weekend and weekdays)
#    to January average (well before, not sure how much this varies seasonally)
vmt_pre1 <- vmt %>% 
  filter(ref_dt == as.Date("2020-03-01")) %>% 
  rename(vmt_mar1 = county_vmt, vmt_jan = jan_avg_vmt) %>% 
  select(-ref_dt)
vmt_pre2 <- vmt %>% arrange(county_name) %>% 
  filter(ref_dt < as.Date("2020-03-07")) %>% 
  group_by(county_name) %>% 
  summarize(vmt_mar6 = mean(county_vmt))
vmt_pre <- vmt_pre1 %>% left_join(vmt_pre2)
# do these need to be normalized by population/hh? 
#   seems likely, but not clear
#   all versions highly correlated

# Metrics from other sources for Risk
# * Life expectancy
# ** Unemployment (county claims, incomplete)

# Life expectancy
# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/VA_A.CSV"
# download.file(url, destfile="tempdata/va_usasleep.csv", method="libcurl")

# read data and rename
life_exp <- read_csv("tempdata/va_usasleep.csv")
names(life_exp) <- c("geoid", "state", "county", "tract", "life_exp", "se", "flag")

life_exp <- life_exp %>% 
  filter(county %in% region) %>% # 5 missing tracts (80 of 85)
  rename(life_expE = life_exp,
         locality = county) %>% 
  mutate(life_expM = 1.64*se,
         year = "2018") %>% 
  select(-se, -flag) %>% 
  mutate(geoid = as.character(geoid))

# join additional data
tract_data <- tract_data %>% 
  left_join(fara, by = c("GEOID" = "CensusTract")) %>% 
  left_join(life_exp, by = c("GEOID" = "geoid")) %>% 
  left_join(vmt_pre, by = "fips")

tract_data <- tract_data %>% # divide travel by pop (still not sure if this is needed)
  mutate(vmt_mar1E = vmt_mar1/totalpopE, 
         vmt_mar6E = vmt_mar6/totalpopE,
         vmt_janE = vmt_jan/totalpopE)


# ....................................................
# 5. Get shapefiles ----

# get geometry: tract polygons
tract <- tracts(state = 'VA', county = region) # from tigris
# join coordinates to data
tract_data_geo <- geo_join(tract, tract_data, by = "GEOID")

# density
summary(as.numeric(tract_data_geo$ALAND))
tract_data_geo$densityE <- tract_data_geo$totalpopE/(as.numeric(tract_data_geo$ALAND)*3.86102e-7)
summary(tract_data_geo$densityE)

# add to non-geo file
den <- as.data.frame(subset(tract_data_geo, select = c(GEOID, densityE)))
tract_data <- tract_data %>% left_join(den)

# get magesterial districts (to overlay, not join)
magdist <- county_subdivisions(state = "VA", county = region)


# clean up
rm(life_exp, fara, vmt, vmt_pre, vmt_pre1, vmt_pre2, den, p)


# ....................................................
# 6. Summarize/Examine indicators ----
tract_data %>% select_at(vars(ends_with("E"))) %>% summary()

# Ablemarle tract 109.03 is UVA
#   highest poverty rate, and people per room, lowest hhinc, etc. (caveat or remove?)
# Life expectancy missing for 5 tracts

  
# ....................................................
# 7. Save ----
saveRDS(tract_data, file = "tract_data.RDS") 
saveRDS(tract_data_geo, file = "tract_data_geo.RDS")
save.image("tract_data_work.Rdata")

