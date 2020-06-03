# Adaptation of CEPR's Fronline Workers Analysis

Webpage: https://cepr.net/a-basic-demographic-profile-of-workers-in-frontline-industries/
GitHub: https://github.com/ceprdata/frontline-workers
Bay Area version: https://bayareaequityatlas.org/essential-workers
NYC version: https://comptroller.nyc.gov/reports/new-york-citys-frontline-workers/#Methodology

## Contents

1. csv_pva: initial person-level PUMS data for Virginia, downloaded from https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/

2. csv_hva: initial household-level PUMS data for Virginia, downloaded from https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/
  * data dictionary: https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html

3. cville_region_acs1418.do: stata do-file that reads in csv files, filters for TJPDC PUMAs, sorts observations, and joins person-level and household-level data, saves acs_1418_all.dta (creates psam_p51.dta and psam_h51.dta along the way)

4. frontline_recodes.do: stata do-file that recodes variables, creates *acs_1418_frontline.dta* (the file to read into R); changes include
  * Renamed (new name = initial pums name)
      * perwgt = pwgtp
      * state = st
  * Created (var name = brief desc, key vars only):
      * flind1 = frontline industry, six categories, 1-Grocery, Convenience, and Drug store, 2-Public Transit, 3-Trucking, Warehouse, and Postal Service, 4-Building Cleaning Services, 5-Healthcare, 6-Childcare, Homeless, Food, and Familiy Services
      * flind_d = fronline industry, nine categories, 1-Grocery, Convenience, and Drug store, 2-Public Transit, 3-Trucking, Warehouse, and Postal Service, 4-Building Cleaning Services, 5-Healthcare, 6-Childcare, Homeless, Food, and Familiy Services, 7-Agriculture, Forestry, and Fishing, 8-Utilities, 9-Construction
      * flind_dd = frontline industry with detail
      * lfstate = labor force status, 1-Employed, 2-Unemployed, 3-Not in labor force, 4-Armed forces
      * ftpt = full/part time, 1-full, 2-part
      * female = sex of respondent, 1-female, 0-not female
      * agep = age of respondent
      * age65 = respondent is 65 or more, 1-yes, 0-no (also age50, age60)
      * wbhao = race/ethnicity of respondent, 1-white, 2-black and multiracial black, 3-Hispanic, 4-Asian, Pacific Islander, 5-Native American
      * forborn = respondent not born in US, 1-yes, 0-no
      * citizen = respondent is citizen, 1-yes, 0-no
      * educ = respondent educational attainment, 1-Less than HS, 2-HS, 3-Some college, 4-College, 5-Advanced
      * renter = respondent rents, 1-yes, 0-no
      * hmown = respondent owns home, 1-yes, 0-no
      * poor = respondent in poverty, 1-yes, 0-no
      * pov200 = respondent income within 200% of poverty level, 1-yes, 0-no
      * hins = respondent has health insurance, 1-yes, 0-no
      * hiep = respondent health insurance provided by employer, 1-yes, 0-no
      * hhoc = presence of own children in hh, 1-yes, 0-no
      * hhsenior = presence of 65+ member in hh, 1-yes, 0-no
  * Additional do-files called (file name = key action): 
      * frontline_ind3d.do = generate consistent industry codes
      * frontline_socp.do = generate consistenty occupation codes (not used)
      * frontline_indocc.do = creates frontline worker variables, flind1, flind_d, flind_dd

5. frontline_tables.do: stata do-file generating tables of frontline workers by attributes, creates table_cvl.xls (didn't use)

6. frontline_tablesR.R: adds survey weights, creates estimates of demographic profiles for total labor force, frontline labor force, and by frontline industry, creates table_from_r.csv used in final markdown file (index.rmd)
