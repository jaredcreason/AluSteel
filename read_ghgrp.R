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
         Subpart ='Latest Reported Industry Type (subparts)')%>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "TRDGHG",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4))) %>%
  
  select(Facility,Name,Address,City, State,Zip, County, Longitude, Latitude, Naics,Year,TRDGHG)

c_data <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                    sheet="c_subpart_level_information",
                    col_names =TRUE) %>%
  rename(Facility = FACILITY_ID,
         Year = REPORTING_YEAR,
         Combustion = GHG_QUANTITY)%>%
  group_by(Facility, Year)%>%
  summarize(Combustion=sum(Combustion))
 
tt_data <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                    sheet="tt_subpart_level_information",
                    col_names =TRUE) %>%
  rename(Facility = FACILITY_ID,
         Year = REPORTING_YEAR,
         Waste = GHG_QUANTITY) %>%
  group_by(Facility, Year)%>%
  summarize(Waste=sum(Waste))

         

#### Aluminum Data Import ----

#al_total <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
#                      sheet="totalEmbyFac",
#                      range =c("a2:l15"),
#                      col_names = TRUE) %>%
#  mutate(subpart = "Total")
  

#al_f_c <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
#                      sheet="totalEmbyFac",
#                      range =c("a18:l31"),
#                      col_names = TRUE) %>%
#  mutate(subpart = "F & C")

al_f <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                    sheet="totalEmbyFac",
                    range =c("a34:l47"),
                    col_names = TRUE)  %>%
    pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Process",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4)))%>%
  rename(Facility = FACILITY_ID)


al_c <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a50:l63"),
                  col_names = TRUE)  %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Combustion",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4)))%>%
  rename(Facility = FACILITY_ID)

al_d <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a66:l79"),
                  col_names = TRUE) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Elec_Gen",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4)))%>%
  rename(Facility = FACILITY_ID)

al_tt <- read_xlsx("data/AluminumFacilityEmissions_byGasbySubpart.xlsx",
                  sheet="totalEmbyFac",
                  range =c("a82:l95"),
                  col_names = TRUE) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Waste",
    names_prefix = "Year") %>%
  mutate (Year = as.numeric(substr(Year, 1,4)))%>%
  rename(Facility = FACILITY_ID)
  
#### All Al Data ---- 

#al_facs <- rbind(al_total, al_f_c, al_f, al_c, al_d, al_tt) %>%
#  mutate(unit = "Mt CO2e")

#### Al Facilities ----

#al_facs_list <- unique(al_tt$Facility)
  
#al_GHG_all <- left_join(al_facs, GHG_data, by = "Facility")%>%
#  pivot_longer(
#    cols = starts_with("20"),
#    names_to = "Year",
#    values_to = "GHG_Emissions",
#    names_prefix = "Year") %>%
#  mutate (Year = as.numeric(substr(Year, 1,4)),
#          Type = "Al")
#




data1 <- left_join(al_c,GHG_data, by = c("Facility", "Year"))
data2 <- left_join(data1,al_f, by = c("Facility", "Year"))
data3 <- left_join(data2,al_d, by = c("Facility", "Year"))
data4 <- left_join(data3,al_tt, by = c("Facility", "Year"))

al_data <- data4 %>%
  filter(Year != 2010) %>%
  select(Facility,Name,Address,City, State,Zip, County, Longitude, Latitude, Naics,
         Year,TRDGHG,Combustion,Process,Elec_Gen,Waste)
write.csv(al_data, "output/aluminum_data_ghgrp.csv")

#
#
#
#
#### Steel Data Import ----
#
#
#
#
st_fac <- read_xlsx("data/Qquestion_FLIGHT_IDs_Updated.xlsx",
                    sheet="q_FacilityEmissions_UPDATED",
                    range =c("a1:Z1501"),
                    col_names =TRUE
                    )%>%
  
  rename(Facility = 'FLIGHT ID',
         Year = 'REPORTING_YEAR',
         Name = 'FACILITY_NAME') %>%
  
  mutate(
    BOPF_CO2EMISSIONS_MT = if_else(is.na(BOPF_CO2EMISSIONS_MT),0,BOPF_CO2EMISSIONS_MT),
    DECARBVES_CO2EMISSIONS_MT = if_else(is.na(DECARBVES_CO2EMISSIONS_MT),0,DECARBVES_CO2EMISSIONS_MT),
    FLARE_CO2EMISSIONS_MT = if_else(is.na(FLARE_CO2EMISSIONS_MT),0,FLARE_CO2EMISSIONS_MT),
    COKEPUSHOP_CO2EMISSIONS_MT = if_else(is.na(COKEPUSHOP_CO2EMISSIONS_MT),0,COKEPUSHOP_CO2EMISSIONS_MT),
    EAF_CO2EMISSIONS_MT = if_else(is.na(EAF_CO2EMISSIONS_MT),0,EAF_CO2EMISSIONS_MT),
    EAF_DECARBVES_STACK_CO2EMIS_MT = if_else(is.na(EAF_DECARBVES_STACK_CO2EMIS_MT),0,EAF_DECARBVES_STACK_CO2EMIS_MT),
    DRF_CO2EMISSIONS_MT = if_else(is.na(DRF_CO2EMISSIONS_MT),0,DRF_CO2EMISSIONS_MT),
    NONRECOVCOB_CO2EMISSIONS_MT = if_else(is.na(NONRECOVCOB_CO2EMISSIONS_MT),0,NONRECOVCOB_CO2EMISSIONS_MT),
    TIF_CO2EMISSIONS_MT = if_else(is.na(TIF_CO2EMISSIONS_MT),0,TIF_CO2EMISSIONS_MT),
    SINTERPROC_CO2EMISSIONS_MT = if_else(is.na(SINTERPROC_CO2EMISSIONS_MT),0,SINTERPROC_CO2EMISSIONS_MT),
                         
    Process = BOPF_CO2EMISSIONS_MT + DECARBVES_CO2EMISSIONS_MT + 
                       FLARE_CO2EMISSIONS_MT + COKEPUSHOP_CO2EMISSIONS_MT + 
                       EAF_CO2EMISSIONS_MT + EAF_DECARBVES_STACK_CO2EMIS_MT + 
                       DRF_CO2EMISSIONS_MT + NONRECOVCOB_CO2EMISSIONS_MT + 
                       TIF_CO2EMISSIONS_MT + SINTERPROC_CO2EMISSIONS_MT) %>%
  
  left_join(GHG_data, by = c("Facility", "Year"))%>%
  filter(Year != 2010, Year !=2021)
  
###
#Separate into EAF, BOPF, and Other
###
#EAF

  eaf <- st_fac %>%
  filter(EAF_CO2EMISSIONS_MT>0) 
  
  eaf_c <- left_join(eaf,c_data, by = c("Facility", "Year"))
  eaf_tt <- left_join(eaf_c,tt_data, by = c("Facility", "Year")) %>%
    
  select(Facility,Name.x,Address,City, State,Zip, County, Longitude, Latitude, Naics,
         Year,TRDGHG,Combustion,Process, Waste)%>%
    rename(Name = Name.x)%>%
    mutate(Waste = if_else(is.na(Waste),0,Waste),
           Fac_Type="EAF")
  
  

  bof <- st_fac %>%
  filter(BOPF_CO2EMISSIONS_MT>0)
  
  bof_c <- left_join(bof,c_data, by = c("Facility", "Year"))
  bof_tt <- left_join(bof_c,tt_data, by = c("Facility", "Year")) %>%
    
    select(Facility,Name.x,Address,City, State,Zip, County, Longitude, Latitude, Naics,
           Year,TRDGHG,Combustion,Process, Waste)%>%
    rename(Name = Name.x)%>%
    mutate(Waste = if_else(is.na(Waste),0,Waste),
           Fac_Type= "BOF")
  
  
  oth <- st_fac %>%
    filter(EAF_CO2EMISSIONS_MT==0, BOPF_CO2EMISSIONS_MT==0)
  
  oth_c <- left_join(oth,c_data, by = c("Facility", "Year"))
  oth_tt <- left_join(oth_c,tt_data, by = c("Facility", "Year")) %>%
    
    select(Facility,Name.x,Address,City, State,Zip, County, Longitude, Latitude, Naics,
           Year,TRDGHG,Combustion,Process, Waste)%>%
    rename(Name = Name.x)%>%
    mutate(Waste = if_else(is.na(Waste),0,Waste),
          Fac_Type = "Other")
  
  Iron_steel_dat <- rbind(eaf_tt,bof_tt,oth_tt) %>%
    mutate(unit = "Mt CO2e")

  write.csv(Iron_steel_dat, "output/iron_steel_data_ghgrp.csv")
  
   
  
  
  #####STOP 
  ###RANDOM SCRIBBLINGS BELOW
  
  
  
    Coke = COKEPUSHOP_CO2EMISSIONS_MT = NONRECOVCOB_CO2EMISSIONS_MT + FLARE_CO2EMISSIONS_MT,
    Taconite = TIF_CO2EMISSIONS_MT,
    BOPF = BOPF_CO2EMISSIONS_MT + DECARBVES_CO2EMISSIONS_MT + FLARE_CO2EMISSIONS_MT,
    Sinter = SINTERPROC_CO2EMISSIONS_MT,
    Lime = s,
    Waste = tt)


 
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




    
