#### Libraries ----

library(tidyr)
library(readxl)
library(dplyr)
library(stringr)

#### Direct Emitter Data Import ---

# Reading in the total GHGRP direct emissions - we use this for steel also 
GHG_data <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                      sheet="Direct Emitters",
                      range =c("a4:w8442"),
                      col_names =TRUE) %>%
  rename(Facility = 'Facility Id',
         Name = 'Facility Name',
         Zip = 'Zip Code',
         Naics = 'Primary NAICS Code',
         Subpart ='Latest Reported Industry Type (subparts)')

#### Aluminum Data Import ----

# Reading Aluminum Data.  First just grabbing the facilities

al_total <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                      sheet="totalEmbyFac",
                      range =c("a2:l15"),
                      col_names = TRUE) %>%
  mutate(subpart = "Total")

al_f_c <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                      sheet="totalEmbyFac",
                      range =c("a18:l31"),
                      col_names = TRUE) %>%
  mutate(subpart = "F & C")

al_f <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                    sheet="totalEmbyFac",
                    range =c("a34:l47"),
                    col_names = TRUE)  %>%
  mutate(subpart = "F")

al_c <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a50:l63"),
                  col_names = TRUE)  %>%
  mutate(subpart = "C")

al_d <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a66:l79"),
                  col_names = TRUE) %>%
  mutate(subpart = "D")

al_tt <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a82:l95"),
                  col_names = TRUE) %>%
  mutate(subpart = "TT") 

#### All Al Data ---- 

al_facs <- rbind(al_total, al_f_c, al_f, al_c, al_d, al_tt) %>%
  mutate(unit = "Mt CO2e")

#### Al Facilities ----

al_facs_list <- unique(al_facs$FACILITY_ID)
  
al_GHG_all <- left_join(al_facs, GHG_data, by = "Facility")%>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "GHG_Emissions",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4)),
          Type = "Al")

# SubPart D Generation emissions

Gen_data <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                     sheet="totalEmbyFac",
                     range =c("a66:l79"),
                     col_names =TRUE,
)%>%
  rename(Facility = FACILITY_ID)%>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Gen_Emissions",
    names_prefix = "Year "
  )%>%
  mutate (Year = as.numeric(Year))


data2 <- left_join(data1,Gen_data, by = c("Facility", "Year"))

#### Steel Data Import ----

st_fac <- read_xlsx("data/GHGRP_Steel.xlsx",
                    sheet="q_FacilityEmissions",
                    range =c("a1:D1379"),
                    col_names =TRUE
                    )%>%
  rename(Facility = 'FLIGHT ID',
         Year = 'REPORTING_YEAR',
         Name = 'FACILITY_NAME',
         QEmissions ='Subpart_Q_Emissions')

fac_only <- st_fac %>%
  select(Facility)
 
##Now get the total emissions from the data 1 DF
  

data3 <- left_join(fac_only, GHG_data, by = "Facility")%>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "GHG_Emissions",
    names_prefix = "Year "
  ) %>%
  mutate (Year = as.numeric(substr(Year, 1,4)),
          Type = "IS")%>%
  select(-'FRS Id')

data4 <- left_join(data3, st_fac, by= c("Facility", "Year"))

#Make a  variablefor EAFs
#mutate (EAFCO2 = (EAF_CO2EMISSIONS_MT + EAF_DECARBVES_STACK_CO2EMIS_MT ),is.na(0)
#)




    
