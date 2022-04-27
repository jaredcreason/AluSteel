library(tidyr)
library(readxl)
library(dplyr)
library(stringr)

#Reading Aluminum Data.  First just grabbing the facilities
al_facs <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                         sheet="totalEmbyFac",
                         range =c("a66:a79"),
                         col_names =TRUE,
                         )%>%
  rename(Facility = FACILITY_ID)
  
#Now reading inthe total GHGRP direct emissions - we use this for steel also 
GHG_data <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                                   sheet="Direct Emitters",
                                   range =c("a4:w8442"),
                                   col_names =TRUE
                     )%>%
  rename(Facility = 'Facility Id',
         Name = 'Facility Name',
         Zip = 'Zip Code',
         Naics = 'Primary NAICS Code',
         Subpart ='Latest Reported Industry Type (subparts)')
 

data1 <- left_join(al_facs,GHG_data, by = "Facility")%>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "GHG_Emissions",
    names_prefix = "Year "
  ) %>%
  mutate (Year = as.numeric(substr(Year, 1,4)),
          Type = "Al")%>%
  select(-'FRS Id')

#SubPart D Generation emissions

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

####################################
# Steel facility data 
####################################
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




    
