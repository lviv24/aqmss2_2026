# =====================================================
# AQMSS II PROJECT
# Lesley Viveros
# =====================================================
library(foreign)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
base_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final"
base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
paths <- list(
  statecap_mx = file.path(base_path, "statecap/mexico"),
  statecap_br = file.path(base_path, "statecap/brazil"),
  leg_mx      = file.path(base_path, "legislature/mexico"),
  leg_br      = file.path(base_path, "legislature/brazil"),
  hom_mx      = file.path(base_path, "female_homicides/mexico"),
  hom_br      = file.path(base_path, "female_homicides/brazil"),
  regression  = file.path(base_path, "regression")
)

invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
# =====================================================
# Mexico Female Homicide Rates (DV) ---------------------------------------------------
#INEGI Mortality Microdata 2018-2023
# =====================================================

# STATE LOOKUP
state_lookup <- data.frame(
  ENT_RESID = sprintf("%02d",1:32),
  state = c(
    "Aguascalientes",
    "Baja California",
    "Baja California Sur",
    "Campeche",
    "Coahuila",
    "Colima",
    "Chiapas",
    "Chihuahua",
    "Ciudad de Mexico",
    "Durango",
    "Guanajuato",
    "Guerrero",
    "Hidalgo",
    "Jalisco",
    "Estado de Mexico",
    "Michoacan",
    "Morelos",
    "Nayarit",
    "Nuevo Leon",
    "Oaxaca",
    "Puebla",
    "Queretaro",
    "Quintana Roo",
    "San Luis Potosi",
    "Sinaloa",
    "Sonora",
    "Tabasco",
    "Tamaulipas",
    "Tlaxcala",
    "Veracruz",
    "Yucatan",
    "Zacatecas"
  )
)
#------------------------------------------------------------------------------
#Folder with INEGI DBF MX HOMICIDE FILES (2018-2023)
#------------------------------------------------------------------------------

data_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/female_homicides"

#------------------------------------------------------------------------------
#Finding all DBF Files (case sensitive)
#------------------------------------------------------------------------------

dbf_files <- list.files(
  path = data_path,
  pattern = "mxfemhom\\.[dD][bB][fF]$",
  full.names = TRUE
)
#------------------------------------------------------------------------------
# ensuring we have all the files 
#------------------------------------------------------------------------------

list.files(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/female_homicides"
)
#[1] "18mxfemhom.DBF"                   "19mxfemhom.DBF"                  
#[3] "20mxfemhom.dbf"                   "21mxfemhom.dbf"                  
#[5] "22mxfemhom.dbf"                   "23mxfemhom.dbf"  

#Sort chronologically & Check
dbf_files <- sort(dbf_files)
dbf_files

# =====================================================
# PROCESS ALL YEARS (2018-2023)
# =====================================================

mexico_homicide_panel <- purrr::map_dfr(dbf_files, function(file){
  
  cat("Processing:", basename(file), "\n")
  
  deaths <- read.dbf(file, as.is = TRUE)
  
  # Female homicide deaths
  female_homicides <- deaths %>%
    filter(SEXO == 2) %>%
    filter(
      (CAUSA_DEF >= "X85" & CAUSA_DEF <= "X99") |
        (CAUSA_DEF >= "Y00" & CAUSA_DEF <= "Y09")
    )
  
  # Extract year from filename
  file_year <- as.numeric(
    paste0(
      "20",
      substr(basename(file), 1, 2)
    )
  )
  
  # Aggregate to state
  female_homicides %>%
    group_by(ENT_RESID) %>%
    summarise(
      female_homicides = n(),
      .groups = "drop"
    ) %>%
    mutate(year = file_year)
  
})

# =====================================================
# ADD STATE NAMES
# =====================================================

mexico_homicide_panel$ENT_RESID <-
  sprintf("%02s", mexico_homicide_panel$ENT_RESID)

mexico_homicide_panel <- mexico_homicide_panel %>%
  left_join(
    state_lookup,
    by = "ENT_RESID"
  )

# Remove state 99 observations
mexico_homicide_panel <- mexico_homicide_panel %>%
  filter(
    !is.na(state),
    ENT_RESID != "99"
  )

# =====================================================
# CLEAN VARIABLE NAMES
# =====================================================

mexico_homicide_panel <- mexico_homicide_panel %>%
  rename(
    state_code = ENT_RESID
  )

# =====================================================
# ADD COUNTRY IDENTIFIER
# =====================================================

mexico_homicide_panel <- mexico_homicide_panel %>%
  mutate(country = "Mexico")

# =====================================================
# REORDER COLUMNS
# =====================================================

mexico_homicide_panel <- mexico_homicide_panel %>%
  select(
    country,
    state_code,
    state,
    year,
    female_homicides
  ) %>%
  arrange(
    state,
    year
  )

# =====================================================
# INSPECT PANEL
# =====================================================

glimpse(mexico_homicide_panel)

head(mexico_homicide_panel)

nrow(mexico_homicide_panel)

# Number of unique states
length(unique(mexico_homicide_panel$state))

# Years included
sort(unique(mexico_homicide_panel$year))

#CHECK 
table(mexico_homicide_panel$year)
#2018 2019 2020 2021 2022 2023 
#32   32   32   32   32   32
length(unique(mexico_homicide_panel$state))
#[1] 32
nrow(mexico_homicide_panel)
#[1] 192

# =====================================================
# MX FEMALE POPULATION (to create female homicide rates/state & year)
# =====================================================
path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final"
mxpop <- readr::read_csv(
  file.path(path, "mxpop1950_2070.csv")
)

glimpse(mxpop)
head(mxpop)
names(mxpop)
mxpop %>%
  filter(ANIO >= 2018, ANIO <= 2023) %>%
  filter(SEXO == "Mujeres") %>%
  group_by(ENTIDAD_FEDERATIVA, ANIO) %>%
  summarise(female_population = sum(POBLACION))

#Save as an object
female_pop <- mxpop %>%
  filter(
    ANIO >= 2018,
    ANIO <= 2023,
    SEXO == "Mujeres"
  ) %>%
  group_by(ENTIDAD_FEDERATIVA, ANIO) %>%
  summarise(
    female_population = sum(POBLACION),
    .groups = "drop"
  ) %>%
  rename(
    state = ENTIDAD_FEDERATIVA,
    year = ANIO
  )
sort(unique(female_pop$state))
sort(unique(mexico_homicide_panel$state))

# Accents in state names in population dataset, recode to match fem hom data
female_pop <- female_pop %>%
  mutate(
    state = recode(
      state,
      "Ciudad de México" = "Ciudad de Mexico",
      "Estado de México" = "Estado de Mexico",
      "Michoacán" = "Michoacan",
      "Nuevo León" = "Nuevo Leon",
      "Querétaro" = "Queretaro",
      "San Luis Potosí" = "San Luis Potosi",
      "Yucatán" = "Yucatan"
    )
  )
# VERIFY 
setdiff(
  sort(unique(female_pop$state)),
  sort(unique(mexico_homicide_panel$state))
)
#character(0)

setdiff(
  sort(unique(mexico_homicide_panel$state)),
  sort(unique(female_pop$state))
)
character(0)

# MERGE FEM HOM & FEM POP DATASETS (MX)
mexico_panel <- mexico_homicide_panel %>%
  left_join(
    female_pop,
    by = c("state", "year")
  )

# check 
sum(is.na(mexico_panel$female_population))
#[1] 0

#====================================================================
# CREATE MX FEMALE HOMICIDE RATES 
#====================================================================
mexico_panel <- mexico_panel %>%
  mutate(
    female_homicide_rate =
      (female_homicides / female_population) * 100000
  )
# CHECK 
nrow(mexico_panel)
#[1] 192

length(unique(mexico_panel$state))
#[1] 32

table(mexico_panel$year)
# 2018 2019 2020 2021 2022 2023 
# 32   32   32   32   32   32 

summary(mexico_panel$female_homicide_rate)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.08194  1.68374  2.60748  4.50147  6.21180 22.82081 

#CHECK for DUPLICATES
female_pop %>%
  count(state, year) %>%
  filter(n > 1)

mexico_homicide_panel %>%
  count(state, year) %>%
  filter(n > 1)

#CREATE MEXICO FEMALE HOMICIDE RATES PANEL 
# =====================================================
if (!dir.exists(paths$hom_mx)) {
  dir.create(paths$hom_mx, recursive = TRUE, showWarnings = FALSE)
}

output_file <- file.path(paths$hom_mx, "mexico_female_homicide_rates_2018_2023.csv")

write_csv(mexico_panel, output_file)

#-------------------------------------------------------------------------------
# Brazil Female Homicide Rates -------------------------------------------------
# Atlas de Violencia 
#-------------------------------------------------------------------------------

brafemhom <- readr::read_csv(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/female_homicides/brafemhom89_23.csv"
)

names(brafemhom)

unique(brafemhom$`Região ID`)
# [1] 11 12 13 14 15 16 17 21 22 23 24 25 26 27 28 29 31 32 33 35 41 42 43 50 51 52 53
# This is the official Brazilian UF(Unidade da Federação) codes used by IBGE, DATASUS, and SIM.

#Create a lookup table 
uf_lookup <- tibble::tribble(
  ~uf_code, ~state,
  11, "Rondonia",
  12, "Acre",
  13, "Amazonas",
  14, "Roraima",
  15, "Para",
  16, "Amapa",
  17, "Tocantins",
  21, "Maranhao",
  22, "Piaui",
  23, "Ceara",
  24, "Rio Grande do Norte",
  25, "Paraiba",
  26, "Pernambuco",
  27, "Alagoas",
  28, "Sergipe",
  29, "Bahia",
  31, "Minas Gerais",
  32, "Espirito Santo",
  33, "Rio de Janeiro",
  35, "Sao Paulo",
  41, "Parana",
  42, "Santa Catarina",
  43, "Rio Grande do Sul",
  50, "Mato Grosso do Sul",
  51, "Mato Grosso",
  52, "Goias",
  53, "Distrito Federal"
)

# BUILDING PANEL 
library(dplyr)
library(lubridate)

brazil_homicide_panel <- brafemhom %>%
  mutate(
    year = year(`Período`),
    uf_code = `Região ID`,
    female_homicides = Valor
  ) %>%
  left_join(
    uf_lookup,
    by = "uf_code"
  ) %>%
  select(
    state,
    uf_code,
    year,
    female_homicides
  ) %>%
  arrange(
    state,
    year
  )

#Check panel 
nrow(brazil_homicide_panel)
#[1] 945

length(unique(brazil_homicide_panel$state))
#[1] 27

sort(unique(brazil_homicide_panel$year))
#[1] 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004
#[17] 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
#[33] 2021 2022 2023

head(brafemhom$`Período`)
#[1] "1989-01-15 UTC" "1990-01-15 UTC" "1991-01-15 UTC" "1992-01-15 UTC"
#[5] "1993-01-15 UTC" "1994-01-15 UTC"

# Any missing values?
colSums(is.na(brazil_homicide_panel))
#  state          uf_code             year female_homicides 
#0                0                0                0 

# One observation per state-year?
brazil_homicide_panel %>%
  count(state, year) %>%
  filter(n > 1)
## A tibble: 0 × 3

# Any states not matched?
brazil_homicide_panel %>%
  filter(is.na(state))
## A tibble: 0 × 4

#------------------------------------------------------------------------------
#FILTER FOR YEARS 2018-2023, Create 2018-2023 Brazil Female Homicide Panel 
#------------------------------------------------------------------------------
brazil_homicide_panel <- brazil_homicide_panel %>%
  filter(year >= 2018, year <= 2023)

#Check 2018-2023 panel 
nrow(brazil_homicide_panel)
# [1] 162
length(unique(brazil_homicide_panel$state))
#[1] 27

sort(unique(brazil_homicide_panel$year))
# [1] 2018 2019 2020 2021 2022 2023

table(brazil_homicide_panel$year)
#2018 2019 2020 2021 2022 2023 
#27   27   27   27   27   27

#------------------------------------------------------------------------------
# Brazil Female Population 
# IBGE population projection by sex and simple age 2000-2070
#------------------------------------------------------------------------------
library(readxl)

brapop <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/brapop2000_2070.xlsx",
  skip = 4
)

# inspect
glimpse(brapop)
names(brapop)

#1st row of dataframe is the header row, convert to column names and remove 1st row

# Save first row as names
new_names <- as.character(brapop[1, ])

# Apply names
names(brapop) <- new_names

# Remove header row
brapop <- brapop[-1, ]

#CHECK! 
names(brapop)[1:10]
# [1] "IDADE" "SEXO"  "CÓD."  "SIGLA" "LOCAL" "2000"  "2001"  "2002"  "2003"  "2004" 

unique(brapop$SEXO)
#[1] "Ambos"    "Homens"   "Mulheres"

head(unique(brapop$LOCAL), 20)
# [1] "Brasil"              "Norte"               "Nordeste"           
#[4] "Sudeste"             "Sul"                 "Centro-Oeste"       
#[7] "Rondônia"            "Acre"                "Amazonas"           
#[10] "Roraima"             "Pará"                "Amapá"              
#[13] "Tocantins"           "Maranhão"            "Piauí"              
#[16] "Ceará"               "Rio Grande do Norte" "Paraíba"            
#[19] "Pernambuco"          "Alagoas" 

# determine rows for ages 
head(unique(brapop$IDADE), 30)
# [1] "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15"
# [17] "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
tail(unique(brapop$IDADE), 30)
# [1] "61" "62" "63" "64" "65" "66" "67" "68" "69" "70" "71" "72" "73" "74" "75" "76"
# [17] "77" "78" "79" "80" "81" "82" "83" "84" "85" "86" "87" "88" "89" "90"
table(brapop$IDADE %in% c("Total","TOTAL","Todas"))
# FALSE 
# 9009 

# There is one row per age, no total row. Must sum all ages for women for each 
# state-year female population count. 

sort(unique(brapop$SIGLA))
# [1] "AC" "AL" "AM" "AP" "BA" "BR" "CE" "CO" "DF" "ES" "GO" "MA" "MG" "MS" "MT" "ND"
# [17] "NO" "PA" "PB" "PE" "PI" "PR" "RJ" "RN" "RO" "RR" "RS" "SC" "SD" "SE" "SP" "SU"
# [33] "TO"

# Must exclude aggregate regions;
#BR  = Brazil
#NO  = Norte
#ND  = Nordeste
#SD  = Sudeste
#SU  = Sul
#CO  = Centro-Oeste
# Only need the 26 states of Brazil + DF (Distrito Federal). 

#------------------------------------------------------------------------------
# CREATE FEMALE POPULATION PANEL (2018-2023)
#------------------------------------------------------------------------------

library(tidyr)

brazil_female_pop <- brapop %>%
  filter(SEXO == "Mulheres") %>%
  filter(
    !SIGLA %in% c(
      "BR","NO","ND","SD","SU","CO"
    )
  ) %>%
  group_by(LOCAL) %>%
  summarise(
    `2018` = sum(as.numeric(`2018`), na.rm = TRUE),
    `2019` = sum(as.numeric(`2019`), na.rm = TRUE),
    `2020` = sum(as.numeric(`2020`), na.rm = TRUE),
    `2021` = sum(as.numeric(`2021`), na.rm = TRUE),
    `2022` = sum(as.numeric(`2022`), na.rm = TRUE),
    `2023` = sum(as.numeric(`2023`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = `2018`:`2023`,
    names_to = "year",
    values_to = "female_population"
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  rename(
    state = LOCAL
  )

# VERIFY  
nrow(brazil_female_pop)

# [1] 162
# Correct because 27 states x 6 years = 162 

length(unique(brazil_female_pop$state))
#[1] 27

sort(unique(brazil_female_pop$year))
# [1] 2018 2019 2020 2021 2022 2023

#------------------------------------------------------------------------------
#Before merging Brazil female homicide panel & population panel, check state names match

sort(unique(brazil_female_pop$state))
# [1] "Acre"                "Alagoas"             "Amapá"              
#[4] "Amazonas"            "Bahia"               "Ceará"              
#[7] "Distrito Federal"    "Espírito Santo"      "Goiás"              
#[10] "Maranhão"            "Mato Grosso"         "Mato Grosso do Sul" 
#[13] "Minas Gerais"        "Pará"                "Paraíba"            
#[16] "Paraná"              "Pernambuco"          "Piauí"              
#[19] "Rio de Janeiro"      "Rio Grande do Norte" "Rio Grande do Sul"  
#[22] "Rondônia"            "Roraima"             "Santa Catarina"     
#[25] "São Paulo"           "Sergipe"             "Tocantins"
sort(unique(brazil_homicide_panel$state))
#[1] "Acre"                "Alagoas"             "Amapa"              
#[4] "Amazonas"            "Bahia"               "Ceara"              
#[7] "Distrito Federal"    "Espirito Santo"      "Goias"              
#[10] "Maranhao"            "Mato Grosso"         "Mato Grosso do Sul" 
#[13] "Minas Gerais"        "Para"                "Paraiba"            
#[16] "Parana"              "Pernambuco"          "Piaui"              
#[19] "Rio de Janeiro"      "Rio Grande do Norte" "Rio Grande do Sul"  
#[22] "Rondonia"            "Roraima"             "Santa Catarina"     
#[25] "Sao Paulo"           "Sergipe"             "Tocantins"   

# STANDARDIZE STATE NAMES BEFORE MERGING 
library(stringi)
library(stringr)

# removes accents
clean_brazil_states <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |>
    str_trim()
}

# Apply to both datasets
brazil_homicide_panel <- brazil_homicide_panel %>%
  mutate(state = clean_brazil_states(state))
brazil_female_pop <- brazil_female_pop %>%
  mutate(state = clean_brazil_states(state))

#Verify alignment 
setdiff(
  sort(unique(brazil_homicide_panel$state)),
  sort(unique(brazil_female_pop$state))
)
# character(0)

setdiff(
  sort(unique(brazil_female_pop$state)),
  sort(unique(brazil_homicide_panel$state))
)
# character(0)


#---------------------------------------------------------------------------------
#MERGE 

brazil_panel <- brazil_homicide_panel %>%
  left_join(
    brazil_female_pop,
    by = c("state", "year")
  )

#---------------------------------------------------------------------------------
#HOMICIDE RATE BRAZIL 
#---------------------------------------------------------------------------------
#Check 
sum(is.na(brazil_panel$female_population))
#[1] 0

brazil_panel %>%
  filter(is.na(female_population))
## A tibble: 0 × 5

#Compute homicide rates (Female homicide rate per 100,000 women)
brazil_panel <- brazil_panel %>%
  mutate(
    female_homicide_rate =
      (female_homicides / female_population) * 100000
  )

#Checks for panel structural integrity 

summary(brazil_panel$female_homicide_rate)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.454   3.510   4.345   4.614   5.392  18.266 

brazil_panel %>%
  filter(!is.finite(female_homicide_rate))
# A tibble: 0 × 6

brazil_panel %>%
  count(year)
#  year     n
#<dbl> <int>
#1  2018    27
#2  2019    27
#3  2020    27
#4  2021    27
#5  2022    27
#6  2023    27
length(unique(brazil_panel$state))
#[1] 27

brazil_panel %>%
  group_by(year) %>%
  summarise(
    mean_rate = mean(female_homicide_rate),
    min_rate = min(female_homicide_rate),
    max_rate = max(female_homicide_rate)
  )
## A tibble: 6 × 4
#    year mean_rate min_rate max_rate
#<dbl>     <dbl>    <dbl>    <dbl>
#1  2018      5.40     1.96    18.3 
#2  2019      4.57     1.74    10.8 
#3  2020      4.58     1.69    11.1 
#4  2021      4.44     1.45     7.16
#5  2022      4.41     1.58     9.34
#6  2023      4.28     1.65     9.05

# INTERPRETATION
# Mean_rate= average female homicide rate across all 27 states
# Min_rate= safest state that year(state with lowest homicide rate)
# Max_rate= most violent state that year(state with highest homicide rate)

# Overall trend: nationally there is a substantial drop in 
# female homicide rates from 2018 to 2019, then rates stabilized at a slightly 
# lower level. 2018 appears to be a peak year of female homicide concentration.

# There is persistent inequality, the decline in Brazil's female homicide rates
# is driven mainly by reductions in the most violent states, not equal 
# improvements across states.

# =====================================================
# BRAZIL FEMALE HOMICIDE RATE SUMMARY (STATE-YEAR PANEL)

brazil_year_summary <- brazil_panel %>%
  group_by(year) %>%
  summarise(
    mean_rate = mean(female_homicide_rate, na.rm = TRUE),
    min_rate  = min(female_homicide_rate, na.rm = TRUE),
    max_rate  = max(female_homicide_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

brazil_year_summary

write_csv(
  brazil_year_summary,
  "brazil_female_homicide_rate_summary_2018_2023.csv"
)

# =====================================================
# Mexico Female Legislature % (IV) ----------------------------------------
# =====================================================
install.packages("janitor")
library(janitor)
library(stringr)
library(readxl)

leg_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature"
#-------------------------------------------------------------------------------
#2018 MX IV
#-------------------------------------------------------------------------------

#inspect names 
excel_sheets(
  file.path(leg_path, "mxleg18.xlsx")
)

excel_sheets(
  file.path(leg_path, "mxleg19.xlsx")
)

excel_sheets(
  file.path(leg_path, "mxleg20.xlsx")
)

excel_sheets(
  file.path(leg_path, "mxleg22.xlsx")
)

excel_sheets(
  file.path(leg_path, "mxleg23.xlsx")
)

#inspect sheets 
excel_sheets(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg18.xlsx"
)

#will use sheet 2.1 (contains the legislators in each state congress by sex and party) 
mxleg18 <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg18.xlsx",
  sheet = "2.1"
)

# Figure out where data starts, Auguascalientes = first state (row 10)
mxleg18 <- read_excel(
  file.path(leg_path,"mxleg18.xlsx"),
  sheet = "2.1"
)

tmp <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg18.xlsx",
  sheet = "2.1",
  col_names = FALSE
)

View(tmp)

leg18_raw <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg18.xlsx",
  sheet = "2.1",
  skip = 9,
  col_names = FALSE
)

names(leg18_raw) <- paste0("V",1:ncol(leg18_raw))

#pulling first block & Inspecting columns
leg18_raw[1:5,1:10]

names(leg18_raw) <- paste0("V",1:ncol(leg18_raw))

leg18_raw[1:5,1:10]
# V1 = state, V2 = total legislators, V3 = men, V4 = women
## A tibble: 5 × 10
#V1                   V2       V3 V4       V5    V6 V7       V8    V9 V10            
#<chr>                <lgl> <dbl> <lgl> <dbl> <dbl> <lgl> <dbl> <dbl> <chr>          
#1 Aguascalientes       NA       27 NA        3     2 NA        6     6 Aguascalientes 
#2 Baja California      NA       25 NA        2     0 NA        7     6 Baja California
#3 Baja California Sur  NA       21 NA        5     3 NA        0     1 Baja Californi…
#4 Campeche             NA       35 NA        4     7 NA        3     3 Campeche       
#5 Coahuila de Zaragoza NA       25 NA        1     1 NA        5     4 Coahuila de Za…

# Create legislature panel 
leg18 <- leg18_raw %>%
  select(
    state = V1,
    total_legislators = V2,
    men = V3,
    women = V4
  )
#remove Mexico total row
leg18 <- leg18 %>%
  filter(
    state != "Estados Unidos Mexicanos"
  )
#Keep only 32 states
leg18 <- leg18 %>%
  slice(1:32)


names(leg18_raw) <- paste0("V",1:ncol(leg18_raw))

dim(leg18_raw)

leg18_raw[1:10,1:15]

glimpse(leg18_raw)


#Build 2018 legislature dataset
leg18 <- leg18_raw %>%
  transmute(
    state = V1,
    
    total_legislators = V3,
    
    women =
      V6 +
      V9 +
      V12 +
      V15 +
      V18 +
      V21 +
      V24 +
      V27 +
      V30 +
      V33 +
      V36 +
      V45,
    
    men =
      V5 +
      V8 +
      V11 +
      V14 +
      V17 +
      V20 +
      V23 +
      V26 +
      V29 +
      V32 +
      V35 +
      V44
  )
#VERIFY 
leg18 %>%
  mutate(check = men + women - total_legislators) %>%
  count(check)
## A tibble: 2 × 2
#check     n
#1     0    32
#2    NA     6

#CREATE IV 2018 
leg18 <- leg18 %>%
  mutate(
    pct_women_legislature =
      100 * women / total_legislators,
    year = 2018
  )
#CHECK 
summary(leg18$pct_women_legislature)

mean(leg18$pct_women_legislature)

range(leg18$pct_women_legislature)
leg18 %>%
  filter(
    state %in% c(
      "México",
      "Ciudad de México",
      "Veracruz de Ignacio de la Llave",
      "Michoacán de Ocampo",
      "Coahuila de Zaragoza"
    )
  ) %>%
  select(state, women, total_legislators, pct_women_legislature)

#Find source of NA's
leg18 %>%
  filter(is.na(pct_women_legislature)) %>%
  select(state, everything())
leg18 %>%
  filter(
    is.na(state) |
      str_detect(state, "^Nota") |
      str_detect(state, "^INEGI")
  )
#Remove them 
library(stringr)

leg18 <- leg18 %>%
  filter(
    !is.na(state),
    !str_detect(state, "^Nota"),
    !str_detect(state, "^INEGI")
  )
#Verify removal of NA values
nrow(leg18)
#[1] 32

#Recreate percentage 
leg18 <- leg18 %>%
  mutate(
    pct_women_legislature =
      women / total_legislators * 100,
    year = 2018
  )

#Check 
summary(leg18$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 36.00   44.66   49.67   49.37   52.33   70.00 

mean(leg18$pct_women_legislature, na.rm = TRUE)
#[1] 49.37399

range(leg18$pct_women_legislature, na.rm = TRUE)
#[1] 36 70

leg18 %>%
filter(is.na(pct_women_legislature))
# A tibble: 0 × 6
# ℹ 6 variables: state <chr>, total_legislators <dbl>, women <dbl>, men <dbl>,
#   pct_women_legislature <dbl>, year <dbl>

# 2018 female legislature % interpretation: on average women held about 49.4% 
# of seats in state legislatures across Mexico's 32 federal states in 2018- 
# indicating near gender parity at state legislative level. Since the median and 
# mean are almost the same it means that the distribution is symmetric and not 
# heavily skewed by extreme states. The range demonstrates substantial variation
# across states with the lowest performing state at 36% and highest at 70%. 

#-------------------------------------------------------------------------------
# 2019 MX IV (FIXED)
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

mxleg19_raw <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg19.xlsx",
  sheet = 3,
    col_names = FALSE
)
head(mxleg19_raw, 20)
names(mxleg19_raw)
glimpse(mxleg19_raw)
# Clean + standardize
leg19 <- mxleg19_raw %>%
  filter(...3 == "0") %>%                    # keep Total row only
  filter(...2 != "Estados Unidos Mexicanos") %>%  # drop national aggregate
  transmute(
    state = ...2,
    total_legislators = parse_number(...5),
    men = parse_number(...6),
    women = parse_number(...7),
    pct_women_legislature = women / total_legislators * 100,
    year = 2019
  )
# check 
nrow(leg19)
length(unique(leg19$state))
summary(leg19$pct_women_legislature)

leg19 %>%
  arrange(desc(pct_women_legislature)) %>%
  select(state, pct_women_legislature)

# checks
nrow(leg19)
#[1] 32
length(unique(leg19$state))
#[1] 32
summary(leg19$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 36.67   46.59   50.00   49.95   52.19   70.00 

sort(leg19$state)

#SAVE 2018 & 2019 
leg18 <- leg18 %>% select(state, year, pct_women_legislature)
leg19 <- leg19 %>% select(state, year, pct_women_legislature)

#------------------------------------------------------------------------------
#2020 MX IV 
#------------------------------------------------------------------------------
mxleg20_raw <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg20.xlsx",
  sheet = "2",
  skip = 9,
  col_names = FALSE
)

str(mxleg20_raw)

leg20 <- mxleg20_raw %>%
  rename(
    state = ...2,
    group_code = ...3,
    group = ...4,
    total_legislators = ...5,
    men = ...6,
    women = ...7
  ) %>%
  filter(group_code == 0) %>%
  filter(state != "Estados Unidos Mexicanos") %>%
  mutate(
    total_legislators = as.numeric(gsub("[^0-9]", "", total_legislators)),
    women = as.numeric(gsub("[^0-9]", "", women)),
    men = as.numeric(men),
    pct_women_legislature = (women / total_legislators) * 100,
    year = 2020
  ) %>%
  select(
    state,
    total_legislators,
    men,
    women,
    pct_women_legislature,
    year
  )

str(leg20)
summary(leg20$pct_women_legislature)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 30.00   46.36   50.00   49.83   52.33   70.00 

nrow(leg20)
# [1] 32

#Save 2020 
leg20 <- leg20 %>% select(state, year, pct_women_legislature)


#-------------------------------------------------------------------------------
# 2022 MX IV
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(readr)


mxleg22_raw <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg22.xlsx",
  sheet = 3,
  skip = 4,
  col_names = FALSE
)

View(mxleg22_check)

library(readxl)
library(dplyr)


# Build legislature dataset

leg22 <- mxleg22_raw %>%
  filter(grepl("^[0-9]{2}$", ...1)) %>%
  transmute(
    state = ...2,
    total_legislators = parse_number(...3),
    men = parse_number(...4),
    women = parse_number(...5),
    pct_women_legislature = women / total_legislators * 100,
    year = 2022
  )

#verify 
summary(leg22$women)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#11.00   14.00   17.00   18.26   21.50   37.00       1 

# Checks
nrow(leg22)
#[1] 32

length(unique(leg22$state))
#[1] 32

summary(leg22$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#47.22   50.00   53.12   54.56   60.00   66.67       1  

#-------------------------------------------------------------------------------
# 2023 MX IV
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(readr)

mxleg23_raw <- read_excel(
  "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature/mxleg23.xlsx",
  sheet = 2,
  skip =4,
    col_names = FALSE
)

View(mxleg23_raw)


# inspect
head(mxleg23_raw)
names(mxleg23_raw)
str(mxleg23_raw)
names(mxleg23_raw)

mxleg23_raw[1:20, ]

leg23 <- mxleg23_raw %>%
  transmute(
    state = ...2,
    total_legislators = parse_number(...3),
    men = parse_number(...4),
    women = parse_number(...5)
  ) %>%
  filter(!is.na(state)) %>%
  mutate(
    pct_women_legislature =
      (women / total_legislators) * 100,
    year = 2023
  )

leg23 %>%
  filter(is.na(total_legislators))

## A tibble: 1 × 6
#state            total_legislators   men women pct_women_legislature  year
#<chr>                        <dbl> <dbl> <dbl>                 <dbl> <dbl>
#1 Ciudad de México                NA    NA    NA                    NA  2023

# check panel size
nrow(leg23)
length(unique(leg23$state))

summary(leg23$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#45.45   50.51   53.66   54.79   60.00   65.00       1 

leg23 %>%
  arrange(desc(pct_women_legislature))

# save only variables needed later
leg23 <- leg23 %>%
  select(
    state,
    year,
    pct_women_legislature
  )
leg23 %>%
  filter(state == "Ciudad de México")

#-------------------------------------------------------------------------------
# 2021 MX IV (Interpolated from 2020 and 2022)
#-------------------------------------------------------------------------------

leg21 <- leg20 %>%
  select(state, pct_women_legislature) %>%
  rename(pct2020 = pct_women_legislature) %>%
  left_join(
    leg22 %>%
      select(state, pct_women_legislature) %>%
      rename(pct2022 = pct_women_legislature),
    by = "state"
  ) %>%
  mutate(
    pct_women_legislature = (pct2020 + pct2022) / 2,
    year = 2021
  ) %>%
  select(
    state,
    year,
    pct_women_legislature
  )

# checks
nrow(leg21)
# [1] 32

length(unique(leg21$state))
# [1] 32

summary(leg21$pct_women_legislature)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 44.00   48.97   51.67   52.19   54.86   63.75       1 

leg21 %>%
  arrange(desc(pct_women_legislature))

#-----------------------------------------------------------------------------
#COMBINE ALL 
#-----------------------------------------------------------------------------

#Check states names 
sort(unique(leg18$state))
sort(unique(leg19$state))
sort(unique(leg20$state))
sort(unique(leg21$state))
sort(unique(leg22$state))
sort(unique(leg23$state))

#compare years for name matches
setdiff(leg19$state, leg20$state)
#character(0)
setdiff(leg20$state, leg19$state)
#character(0)

#Check # of states match across years (32)
bind_rows(
  leg18,
  leg19,
  leg20,
  leg21,
  leg22,
  leg23
) %>%
  count(year)
## A tibble: 6 × 2
#year     n
##<dbl> <int>
#1  2018    32
#2  2019    32
#3  2020    32
#4  2021    32
#5  2022    32
#6  2023    32

mxleg_panel <- bind_rows(
leg18,
leg19,
leg20,
leg21,
leg22,
leg23 %>% select(state, year, pct_women_legislature)
)

# CHECKS
length(unique(mxleg_panel$state))
  #[1] 32
sort(unique(mxleg_panel$state))
#  [1] "Aguascalientes"                  "Baja California"                
#  [3] "Baja California Sur"             "Campeche"                       
#  [5] "Chiapas"                         "Chihuahua"                      
#  [7] "Ciudad de México"                "Coahuila de Zaragoza"           
#  [9] "Colima"                          "Durango"                        
#  [11] "Guanajuato"                      "Guerrero"                       
#  [13] "Hidalgo"                         "Jalisco"                        
#  [15] "México"                          "Michoacán de Ocampo"            
#  [17] "Morelos"                         "Nayarit"                        
#  [19] "Nuevo León"                      "Oaxaca"                         
#  [21] "Puebla"                          "Querétaro"                      
#  [23] "Quintana Roo"                    "San Luis Potosí"                
#  [25] "Sinaloa"                         "Sonora"                         
#  [27] "Tabasco"                         "Tamaulipas"                     
#  [29] "Tlaxcala"                        "Veracruz de Ignacio de la Llave"
#  [31] "Yucatán"                         "Zacatecas"

nrow(mxleg_panel)
#[1] 192 (32 states × 6 years)

table(mxleg_panel$year)
#2018 2019 2020 2021 2022 2023 
# 32   32   32   32   32   32 
length(unique(mxleg_panel$state))

#------------------------------------------------------------------------
# Brazil Female Legislature % (IV) ----------------------------------------
# TSE Election Data
#------------------------------------------------------------------------

leg_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature"

# -----------------------------------------------------
# STATE LOOKUP
# -----------------------------------------------------

uf_lookup <- tibble::tribble(
  ~SG_UF, ~state,
  "RO","Rondonia",
  "AC","Acre",
  "AM","Amazonas",
  "RR","Roraima",
  "PA","Para",
  "AP","Amapa",
  "TO","Tocantins",
  "MA","Maranhao",
  "PI","Piaui",
  "CE","Ceara",
  "RN","Rio Grande do Norte",
  "PB","Paraiba",
  "PE","Pernambuco",
  "AL","Alagoas",
  "SE","Sergipe",
  "BA","Bahia",
  "MG","Minas Gerais",
  "ES","Espirito Santo",
  "RJ","Rio de Janeiro",
  "SP","Sao Paulo",
  "PR","Parana",
  "SC","Santa Catarina",
  "RS","Rio Grande do Sul",
  "MS","Mato Grosso do Sul",
  "MT","Mato Grosso",
  "GO","Goias",
  "DF","Distrito Federal"
)

# 2018 (2018-2021)
braleg18 <- read_delim(
  file.path(leg_path, "braleg18.csv"),
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

names(braleg18)

unique(braleg18$DS_CARGO)

unique(braleg18$DS_SIT_TOT_TURNO)

unique(braleg18$DS_GENERO)

unique(braleg18$SG_UF)

#Check rows
braleg18 %>%
  filter(DS_CARGO == "DEPUTADO ESTADUAL") %>%
  count(DS_SIT_TOT_TURNO, sort = TRUE)
# A tibble: 5 × 2
#DS_SIT_TOT_TURNO     n
#<chr>            <int>
#1 SUPLENTE         13225
#2 NÃO ELEITO        2657
#3 #NULO#            1056
#4 ELEITO POR QP      875
#5 ELEITO POR MÉDIA   160

#Create 
leg18_br <- braleg18 %>%
  filter(
    DS_CARGO %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"),
    str_detect(DS_SIT_TOT_TURNO, "^ELEITO")
  ) %>%
  group_by(SG_UF) %>%
  summarise(
    women = sum(DS_GENERO == "FEMININO", na.rm = TRUE),
    total_legislators = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_women_legislature =
      100 * women / total_legislators,
    year = 2018
  ) %>%
  left_join(uf_lookup, by = "SG_UF")

#2022
braleg22 <- read_delim(
  file.path(leg_path, "braleg22.csv"),
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

unique(braleg22$DS_CARGO)

unique(braleg22$DS_SIT_TOT_TURNO)

unique(braleg22$DS_GENERO)

#Check rows
braleg22 %>%
  filter(DS_CARGO == "DEPUTADO ESTADUAL") %>%
  count(DS_SIT_TOT_TURNO, sort = TRUE)
# A tibble: 5 × 2
#DS_SIT_TOT_TURNO     n
#<chr>            <int>
#1 SUPLENTE         10136
#2 NÃO ELEITO        4731
#3 #NULO              835
#4 ELEITO POR QP      832
#5 ELEITO POR MÉDIA   203

#Create
leg22_br <- braleg22 %>%
  filter(
    DS_CARGO %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"),
    str_detect(DS_SIT_TOT_TURNO, "^ELEITO")
  ) %>%
  group_by(SG_UF) %>%
  summarise(
    women = sum(DS_GENERO == "FEMININO", na.rm = TRUE),
    total_legislators = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_women_legislature =
      100 * women / total_legislators,
    year = 2022
  ) %>%
  left_join(uf_lookup, by = "SG_UF")

# CHECKS
nrow(leg18_br)
# 27
nrow(leg22_br)
# 27

table(leg18_br$year)
# 2018 
# 27 
table(leg22_br$year)
# 2022 
# 27 

summary(leg18_br$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   12.50   15.87   15.24   19.10   33.33 
summary(leg22_br$pct_women_legislature)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.167  12.599  18.519  17.317  20.833  29.167 

# =============================================================================
# LEGISLATURE FIXES
#   (A) Brazil: processes the 2014 TSE file and rebuilds the step function
#       with institutionally correct term timing (elected Oct year X ->
#       in office from Feb X+1).
#   (B) Mexico: patches Ciudad de Mexico in the ref-2022 wave (reported in
#       the party-breakdown cuadro: 66 total, 33 women) and fills ref-2023
#       by carry-forward (the CDMX II Legislatura, elected June 2021, sits
#       through 2024, so Dec-2022 and Dec-2023 are the same body).
#   (C) Mexico: replaces the 2021 midpoint interpolation with an
#       election-calendar fill. The June 2021 midterms renewed 30 of 32
#       state congresses, so Dec-2021 composition equals Dec-2022 for those
#       states (backward fill). Quintana Roo renewed its congress in June
#       2022, so its Dec-2021 body is the one observed in Dec-2020
#       (forward fill). Coahuila renewed in 2020 and 2023, so backward fill
#       from 2022 is also correct for it.
#
#===================================================

library(dplyr)
library(readr)
library(stringr)

leg_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/legislature"

# =============================================================================
# (A) BRAZIL 2014 ELECTION -> COMPOSITION IN OFFICE DURING 2018
# =============================================================================

# The 2014 file uses the same harmonized TSE layout as 2018/2022, so the
# identical filter applies: elected state/district deputies, where
# DS_SIT_TOT_TURNO begins with 'ELEITO' (covers QP and MEDIA).
braleg14 <- read_delim(
  file.path(leg_path, "braleg14.csv"),
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE)

leg14_br <- braleg14 %>%
  filter(
    DS_CARGO %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"),
    str_detect(DS_SIT_TOT_TURNO, "^ELEITO")
  ) %>%
  group_by(SG_UF) %>%
  summarise(
    women             = sum(DS_GENERO == "FEMININO", na.rm = TRUE),
    total_legislators = n(),
    .groups = "drop"
  ) %>%
  mutate(pct_women_legislature = 100 * women / total_legislators) %>%
  left_join(uf_lookup, by = "SG_UF")

# Validation against reference values computed independently from this file:
# 27 UFs, 1,059 elected deputies (the constitutional total), national 11.24%,
# Amapa 33.33 (max), Amazonas 4.17 (min), Distrito Federal 20.83.
stopifnot(
  nrow(leg14_br) == 27,
  sum(leg14_br$total_legislators) == 1059,
  abs(100 * sum(leg14_br$women) / sum(leg14_br$total_legislators) - 11.24) < 0.01,
  abs(leg14_br$pct_women_legislature[leg14_br$SG_UF == "AP"] - 33.33) < 0.01,
  abs(leg14_br$pct_women_legislature[leg14_br$SG_UF == "DF"] - 20.83) < 0.01
)
cat("Brazil 2014 file validated: 1,059 deputies, national 11.2% women.\n")

# Step function with correct term timing:
#   composition during 2018      <- 2014 election (in office 2015-2018)
#   composition during 2019-2022 <- 2018 election (in office Feb 2019-)
#   composition during 2023      <- 2022 election (in office Feb 2023-)
br_leg <- bind_rows(
  leg14_br %>% transmute(state, year = 2018, pct_women_legislature),
  bind_rows(lapply(2019:2022, function(y)
    leg18_br %>% transmute(state, year = y, pct_women_legislature))),
  leg22_br %>% transmute(state, year = 2023, pct_women_legislature)
)

stopifnot(nrow(br_leg) == 27 * 6, sum(is.na(br_leg$pct_women_legislature)) == 0)
cat("Brazil legislature step function complete: 162 state-years, no NAs.\n")

# =============================================================================
# (B) MEXICO: CIUDAD DE MEXICO  (ref-2022 and ref-2023 waves)
# =============================================================================

# ref-2022: the simple cuadro returns NA for CDMX, but the party-breakdown
# cuadro in the same workbook reports the Total row: 66 legislators,
# 33 men, 33 women -> 50.0%. The value is patched directly with its source
# documented here. (Source: CNPLE ref-2022 workbook, cuadro with
# 'Personas legisladoras ... por grupo parlamentario', CDMX Total row.)
leg22 <- leg22 %>%
  mutate(pct_women_legislature = if_else(
    state == "Ciudad de M\u00e9xico" & is.na(pct_women_legislature),
    100 * 33 / 66,
    pct_women_legislature))

# ref-2023: CDMX is ND (census nonresponse). The II Legislatura elected in
# June 2021 sits through 2024, so the Dec-2023 composition is the same body
# observed in Dec-2022; the 2022 value is carried forward.
cdmx_2022 <- leg22 %>%
  filter(state == "Ciudad de M\u00e9xico") %>%
  pull(pct_women_legislature)
stopifnot(length(cdmx_2022) == 1, !is.na(cdmx_2022))

leg23 <- leg23 %>%
  mutate(pct_women_legislature = if_else(
    state == "Ciudad de M\u00e9xico" & is.na(pct_women_legislature),
    cdmx_2022,
    pct_women_legislature))

stopifnot(sum(is.na(leg22$pct_women_legislature)) == 0,
          sum(is.na(leg23$pct_women_legislature)) == 0)
cat("CDMX patched: 2022 = 50.0 (from party cuadro), 2023 carried forward.\n")

# =============================================================================
# (C) MEXICO: REF-2021 FILL FROM THE ELECTION CALENDAR
# =============================================================================

# The June 2021 midterms renewed 30 of 32 state congresses, so for those
# states the legislature observed on Dec 31, 2022 is the same body that sat
# on Dec 31, 2021 -> backward fill from leg22. Quintana Roo renewed its
# congress in June 2022, so its Dec-2021 body is the one observed in
# Dec-2020 -> forward fill from leg20. Coahuila (renewed 2020 and 2023) had
# no change between Dec-2020 and Dec-2022, so backward fill is correct.
# This replaces the previous 2020/2022 midpoint interpolation entirely.
leg21 <- leg22 %>%
  select(state, pct_women_legislature) %>%
  left_join(leg20 %>% select(state, pct2020 = pct_women_legislature),
            by = "state") %>%
  mutate(
    pct_women_legislature = if_else(state == "Quintana Roo",
                                    pct2020, pct_women_legislature),
    year = 2021) %>%
  select(state, year, pct_women_legislature)

stopifnot(nrow(leg21) == 32, sum(is.na(leg21$pct_women_legislature)) == 0)
cat("ref-2021 filled from election calendar (backward from 2022;",
    "Quintana Roo from 2020).\n")

# =============================================================================
# REBUILD THE MEXICAN PANEL WITH THE FIXED YEARS
# =============================================================================

mxleg_panel <- bind_rows(
  leg18, leg19, leg20, leg21, leg22, leg23
) %>%
  select(state, year, pct_women_legislature)

stopifnot(nrow(mxleg_panel) == 192,
          sum(is.na(mxleg_panel$pct_women_legislature)) == 0,
          all(table(mxleg_panel$year) == 32))
cat("Mexican legislature panel rebuilt: 192 state-years, no NAs.\n")
#---------------------------------------------------------------------------
# STATE CAPACITY MEDIATOR
# Mexico (State Government Expenditure per Capita)
#---------------------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)

path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final"
path_cap <- file.path(path, "statecap")

years <- 2018:2023

# LOAD 31 STATES
# =====================================================
state_list <- lapply(years, function(y) {
  
  file_name <- paste0("mxstatecap", substr(y, 3, 4), ".csv")
  
  df <- read.csv(
    file.path(path_cap, file_name),
    stringsAsFactors = FALSE
  )
  
  df$year <- y
  df
})

states_raw <- bind_rows(state_list)

# LOAD CDMX (ALL YEARS 2018–2023)
# =====================================================
cdmx_list <- lapply(years, function(y) {
  
  file_name <- paste0("cdmxsc", substr(y, 3, 4), ".csv")
  
  df <- read.csv(
    file.path(path_cap, file_name),
    stringsAsFactors = FALSE
  )
  
  df$year <- y
  df
})

cdmx_raw <- bind_rows(cdmx_list)


# COMBINE ALL EXPENDITURE DATA
statecap_raw <- bind_rows(states_raw, cdmx_raw)


# KEEP TOTAL EXPENDITURE ONLY
statecap_total <- statecap_raw %>%
  filter(
    TEMA == "Egresos",
    DESCRIPCION_CATEGORIA == "Total de egresos"
  ) %>%
  group_by(CVE_ENT, year) %>%
  summarise(
    total_expenditure = sum(VALOR, na.rm = TRUE),
    .groups = "drop"
  )

# STATE NAME MAPPING
# =====================================================
state_names <- c(
  "1"="Aguascalientes","2"="Baja California","3"="Baja California Sur",
  "4"="Campeche","5"="Coahuila","6"="Colima","7"="Chiapas",
  "8"="Chihuahua","9"="Ciudad de Mexico","10"="Durango",
  "11"="Guanajuato","12"="Guerrero","13"="Hidalgo","14"="Jalisco",
  "15"="Estado de Mexico","16"="Michoacan","17"="Morelos",
  "18"="Nayarit","19"="Nuevo Leon","20"="Oaxaca","21"="Puebla",
  "22"="Queretaro","23"="Quintana Roo","24"="San Luis Potosi",
  "25"="Sinaloa","26"="Sonora","27"="Tabasco","28"="Tamaulipas",
  "29"="Tlaxcala","30"="Veracruz","31"="Yucatan","32"="Zacatecas"
)

statecap_total <- statecap_total %>%
  mutate(state = state_names[as.character(CVE_ENT)])

# POPULATION (DENOMINATOR)
# =====================================================
mxpop <- read_csv(file.path(path, "mxpop1950_2070.csv"))

total_pop <- mxpop %>%
  filter(ANIO >= 2018, ANIO <= 2023) %>%
  group_by(ENTIDAD_FEDERATIVA, ANIO) %>%
  summarise(population = sum(POBLACION), .groups = "drop") %>%
  rename(state = ENTIDAD_FEDERATIVA,
         year = ANIO) %>%
  mutate(
    state = recode(
      state,
      "Ciudad de México" = "Ciudad de Mexico",
      "Estado de México" = "Estado de Mexico",
      "Michoacán" = "Michoacan",
      "Nuevo León" = "Nuevo Leon",
      "Querétaro" = "Queretaro",
      "San Luis Potosí" = "San Luis Potosi",
      "Yucatán" = "Yucatan"
    )
  )

# =====================================================
# STATE CAPACITY PANEL
# =====================================================
mxstate_capacity_panel <- statecap_total %>%
  left_join(total_pop, by = c("state", "year")) %>%
  mutate(
    state_capacity = total_expenditure / population,
    log_state_capacity = log(state_capacity)
  ) %>%
  select(
    state, year,
    total_expenditure,
    population,
    state_capacity,
    log_state_capacity
  ) %>%
  arrange(state, year)


# CHECKS
nrow(mxstate_capacity_panel)
#[1] 192
length(unique(mxstate_capacity_panel$state))
#[1] 32
table(mxstate_capacity_panel$year)
# 2018 2019 2020 2021 2022 2023 
#  32   32   32   32   32   32 
sum(is.na(mxstate_capacity_panel$state_capacity))
#[1] 0

# =====================================================
# Brazil State Capacity  --------------------------------------------------
# =====================================================

# inspect one file 
library(readr)
library(dplyr)
library(stringr)

path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/statecap"

test <- read_delim(
  file.path(path, "brastatecap18.csv"),
  delim = ";",
  skip = 3,
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

names(test)
glimpse(test)
head(test)

# Load all files (2018-2023)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/statecap"

years <- 2018:2023

br_list <- lapply(years, function(y) {
  
  file <- paste0("brastatecap", substr(y, 3, 4), ".csv")
  
  df <- read_delim(
    file.path(path, file),
    delim = ";",
    skip = 3,
    locale = locale(encoding = "Latin1"),
    show_col_types = FALSE
  )
  
  df %>%
    mutate(
      year = y,
      Valor = as.character(Valor)   
    )
})

br_raw <- bind_rows(br_list)

#keep only total expenditure
br_total <- br_raw %>%
  mutate(
    Valor = str_replace_all(Valor, "\\.", ""),
    Valor = str_replace_all(Valor, ",", "."),
    Valor = as.numeric(Valor)
  ) %>%
  filter(
    Coluna == "Despesas Empenhadas",
    Conta == "Total Geral da Despesa"
  ) %>%
  group_by(UF, year) %>%
  summarise(
    total_expenditure = sum(Valor, na.rm = TRUE),
    .groups = "drop"
  )

#==========================================================================
# CONTROLS ----------------------------------------------------------------
#==========================================================================
#MX GDP/CAPITA

path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/gdp"

mxgdp <- readr::read_csv(
  file.path(path, "mxgdp.csv")
)

#Inspect
names(mxgdp)
#[1] "Descriptores" "2003"         "2004"         "2005"         "2006"        
#[6] "2007"         "2008"         "2009"         "2010"         "2011"        
#[11] "2012"         "2013"         "2014"         "2015"         "2016"        
#[16] "2017"         "2018"         "2019"         "2020"         "2021"        
#[21] "2022"         "2023<R>"      "2024<P>"  
glimpse(mxgdp)
head(mxgdp)
head(mxgdp$Descriptores, 40)

#Extract state GDP 
mx_gdp <- mxgdp %>%
  filter(
    grepl(
      "Millones de pesos a precios de 2018\\|PIBPM",
      Descriptores
    )
  ) %>%
  slice(-1) %>%   # remove national total
  mutate(
    state_name = str_split_fixed(Descriptores, "\\|", 3)[,3],
    state_name = str_remove(state_name, "<C1>")
  )

#verify 
head(mx_gdp$state_name)
unique(mx_gdp$state_name)                            
length(unique(mx_gdp$state_name))
#[1] 32

#Convert to panel format 
mx_gdp_long <- mx_gdp %>%
  select(state_name, `2003`:`2024<P>`) %>%
  pivot_longer(
    cols = -state_name,
    names_to = "year",
    values_to = "gdp"
  ) %>%
  mutate(
    year = as.numeric(str_extract(year, "\\d{4}"))
  )

head(mx_gdp_long)
# A tibble: 6 × 3
#state_name      year     gdp
#<chr>          <dbl>   <dbl>
#1 Aguascalientes  2003 176577.
#2 Aguascalientes  2004 182405.
#3 Aguascalientes  2005 185651.
#4 Aguascalientes  2006 200318.
#5 Aguascalientes  2007 219092.
#6 Aguascalientes  2008 218473.

#Standardize state names 
mx_gdp_long <- mx_gdp_long %>%
  mutate(state_name = recode(state_name,
                             "Ciudad de México" = "Ciudad de Mexico",
                             "Coahuila de Zaragoza" = "Coahuila",
                             "Michoacán de Ocampo" = "Michoacan",
                             "Nuevo León" = "Nuevo Leon",
                             "Querétaro" = "Queretaro",
                             "San Luis Potosí" = "San Luis Potosi",
                             "Yucatán" = "Yucatan",
                             "México" = "Mexico"
  ))

# Only need 2018-2023 data, get rid of other years
mx_gdp_long <- mx_gdp_long %>%
  filter(year >= 2018 & year <= 2023)

#CHECK
n_distinct(mx_gdp_long$state_name)
#[1] 32
table(mx_gdp_long$year)
# 2018 2019 2020 2021 2022 2023 
# 32   32   32   32   32   32

#Use population to calculate GDP/capita 
total_pop <- mxpop %>%
  filter(ANIO >= 2018, ANIO <= 2023) %>%
  group_by(ENTIDAD_FEDERATIVA, ANIO) %>%
  summarise(
    population = sum(POBLACION),
    .groups = "drop"
  ) %>%
  rename(
    state = ENTIDAD_FEDERATIVA,
    year = ANIO
  )

#fix state names 
total_pop <- total_pop %>%
  mutate(
    state = recode(
      state,
      "Ciudad de México" = "Ciudad de Mexico",
      "Estado de México" = "Estado de Mexico",
      "Michoacán" = "Michoacan",
      "Nuevo León" = "Nuevo Leon",
      "Querétaro" = "Queretaro",
      "San Luis Potosí" = "San Luis Potosi",
      "Yucatán" = "Yucatan"
    )
  )

#GDP panel prep 
mx_gdp_long <- mx_gdp_long %>%
  filter(year >= 2018, year <= 2023)

#verify name match before merging 
setdiff(mx_gdp_long$state_name, total_pop$state)
#[1] "Mexico"                          "Veracruz de Ignacio de la Llave"

setdiff(total_pop$state, mx_gdp_long$state_name)
#[1] "Estado de Mexico" "Veracruz"        

#fix gdp names 
mx_gdp_long <- mx_gdp_long %>%
  mutate(state_name = recode(state_name,
                             "Estado de Mexico" = "Mexico",
                             "Veracruz de Ignacio de la Llave" = "Veracruz"
  ))
#fix pop names 
total_pop <- total_pop %>%
  mutate(state = recode(state,
                        "Estado de Mexico" = "Mexico",
                        "Veracruz de Ignacio de la Llave" = "Veracruz"
  ))
#verify again 
setdiff(mx_gdp_long$state_name, total_pop$state)
setdiff(total_pop$state, mx_gdp_long$state_name)
# character(0)

#Merge GDP & Population 
mxgdp_pc_panel <- mx_gdp_long %>%
  left_join(total_pop,
            by = c("state_name" = "state", "year" = "year")) %>%
  mutate(
    gdp_pc = (gdp * 1e6) / population,
    ln_gdp_pc = log(gdp_pc)
  )

#Checks 
summary(mxgdp_pc_panel$gdp_pc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#59371  129414  175813  193558  230837  606990 
sum(is.na(mxgdp_pc_panel$gdp_pc))
#[1] 0

mxgdp_pc_panel %>%
  filter(is.na(gdp_pc)) %>%
  select(state_name, year, gdp, population)

#save in control folder 
write.csv(
  mxgdp_pc_panel %>%
    select(state_name, year, ln_gdp_pc),
  file = file.path(path, "mxgdp_pc_panel_log_2018_2023.csv"),
  row.names = FALSE
)

# =====================================================
# BRAZIL GDP/CAPITA (CONTROL)
# =====================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

input_dir    <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/gdp/bragdp2010_23"
output_path  <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/gdp"
target_years <- 2018:2023

valid_states <- c(
  "Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins",
  "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba",
  "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais",
  "Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana",
  "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
  "Mato Grosso", "Goias", "Distrito Federal"
)

clean_str <- function(x) {
  stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
}

# -------------------------------------------------------
# Extractor: finds ALL "Valor Adicionado Bruto" blocks
# in sheet 2, matches against valid_states (case-insensitive),
# reads Col 1 = year, Col 6 = current-price GDP (millions BRL)
# -------------------------------------------------------

extract_file <- function(path) {
  raw      <- readxl::read_excel(path, sheet = 2,
                                 col_names = FALSE, .name_repair = "minimal")
  char_mat <- apply(raw, 2, as.character)
  n_rows   <- nrow(char_mat)
  
  section_rows <- which(apply(char_mat, 1, function(r) {
    any(grepl("valor adicionado bruto", tolower(r), fixed = TRUE))
  }))
  
  if (length(section_rows) == 0) return(NULL)
  
  results_list <- list()
  
  for (sr in section_rows) {
    unit_row <- sr + 1
    if (unit_row > n_rows) next
    
    unit_name  <- trimws(char_mat[unit_row, 1])
    if (is.na(unit_name) || nchar(unit_name) == 0) next
    
    unit_clean <- clean_str(unit_name)
    
    # Case-insensitive match against valid_states
    matched <- valid_states[tolower(valid_states) == tolower(unit_clean)]
    if (length(matched) == 0) next
    unit_standard <- matched[1]   # use the properly-cased name from valid_states
    
    data_start <- sr + 4
    data_end   <- min(sr + 18, n_rows)
    
    years_vec  <- suppressWarnings(as.integer(char_mat[data_start:data_end, 1]))
    values_vec <- suppressWarnings(as.numeric(char_mat[data_start:data_end, 6]))
    
    df <- data.frame(
      state            = unit_standard,
      year             = years_vec,
      gdp_millions_brl = values_vec,
      stringsAsFactors = FALSE
    )
    
    df <- df[!is.na(df$year) & df$year %in% target_years &
               !is.na(df$gdp_millions_brl), ]
    
    if (nrow(df) > 0) results_list[[unit_standard]] <- df
  }
  
  if (length(results_list) == 0) return(NULL)
  bind_rows(results_list)
}

# -------------------------------------------------------
# Run across all 33 files
# -------------------------------------------------------

files <- list.files(input_dir, pattern = "^Tabela[0-9].*\\.xls$",
                    full.names = TRUE, ignore.case = TRUE)

cat("Files found:", length(files), "\n")

all_results <- list()
errors      <- list()

for (f in files) {
  tryCatch({
    out <- extract_file(f)
    if (!is.null(out) && nrow(out) > 0) {
      all_results[[basename(f)]] <- out
      cat("OK:", basename(f), "->",
          paste(unique(out$state), collapse = ", "), "\n")
    } else {
      cat("SKIP:", basename(f), "\n")
    }
  }, error = function(e) {
    errors[[basename(f)]] <<- conditionMessage(e)
    cat("FAILED:", basename(f), "-", conditionMessage(e), "\n")
  })
}

# -------------------------------------------------------
# Combine, verify
# -------------------------------------------------------

brazil_gdp_raw <- bind_rows(all_results) %>%
  distinct(state, year, .keep_all = TRUE) %>%
  arrange(state, year)

cat("\n--- STRUCTURAL CHECKS ---\n")
cat("Rows:", nrow(brazil_gdp_raw), "\n")                    # 162
cat("States:", length(unique(brazil_gdp_raw$state)), "\n")  # 27
cat("Missing:", paste(setdiff(valid_states, unique(brazil_gdp_raw$state)),
                      collapse = ", "), "\n")               # empty
print(table(brazil_gdp_raw$year))                           # 27 per year

# YoY consistency check
suspicious <- brazil_gdp_raw %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(yoy = gdp_millions_brl / lag(gdp_millions_brl)) %>%
  filter(!is.na(yoy)) %>%
  summarise(max_yoy = max(yoy), min_yoy = min(yoy), .groups = "drop") %>%
  filter(max_yoy > 1.5 | min_yoy < 0.7)

cat("States with suspicious YoY jumps:", nrow(suspicious), "\n")  # 0

# -------------------------------------------------------
# Build total population panel from brapop
# -------------------------------------------------------

clean_brazil_states <- function(x) {
  stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
}

brazil_total_pop <- brapop %>%
  filter(SEXO == "Ambos") %>%
  filter(!SIGLA %in% c("BR","NO","ND","SD","SU","CO")) %>%
  group_by(LOCAL) %>%
  summarise(
    `2018` = sum(as.numeric(`2018`), na.rm = TRUE),
    `2019` = sum(as.numeric(`2019`), na.rm = TRUE),
    `2020` = sum(as.numeric(`2020`), na.rm = TRUE),
    `2021` = sum(as.numeric(`2021`), na.rm = TRUE),
    `2022` = sum(as.numeric(`2022`), na.rm = TRUE),
    `2023` = sum(as.numeric(`2023`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = `2018`:`2023`,
               names_to = "year", values_to = "total_population") %>%
  mutate(
    year  = as.integer(year),
    state = clean_brazil_states(LOCAL)
  ) %>%
  select(state, year, total_population)

cat("\nPopulation panel rows:", nrow(brazil_total_pop), "\n")
# 162
cat("Population panel states:", length(unique(brazil_total_pop$state)), "\n")
# 27

# Name alignment check
cat("GDP states not in pop:", 
    paste(setdiff(brazil_gdp_raw$state, brazil_total_pop$state), collapse = ", "), "\n")
cat("Pop states not in GDP:", 
    paste(setdiff(brazil_total_pop$state, brazil_gdp_raw$state), collapse = ", "), "\n")

# -------------------------------------------------------
# Compute GDP per capita
# GDP in millions BRL * 1e6 = BRL / population = BRL per person
# -------------------------------------------------------

brazil_gdp_pc_panel <- brazil_gdp_raw %>%
  left_join(brazil_total_pop, by = c("state", "year")) %>%
  mutate(
    gdp_pc    = (gdp_millions_brl * 1e6) / total_population,
    ln_gdp_pc = log(gdp_pc)
  ) %>%
  select(state, year, gdp_millions_brl, total_population, gdp_pc, ln_gdp_pc) %>%
  arrange(state, year)

cat("\n--- GDP PER CAPITA CHECKS ---\n")
cat("Rows:", nrow(brazil_gdp_pc_panel), "\n")                      
# 162
cat("States:", length(unique(brazil_gdp_pc_panel$state)), "\n")    
# 27
cat("Missing gdp_pc:", sum(is.na(brazil_gdp_pc_panel$gdp_pc)), "\n")  
# 0
summary(brazil_gdp_pc_panel$gdp_pc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#12134   19778   26292   31495   37942  109550 

# Spot check — DF and SP should be among highest, Maranhao among lowest
brazil_gdp_pc_panel %>%
  filter(state %in% c("Sao Paulo", "Maranhao", "Distrito Federal")) %>%
  select(state, year, gdp_pc, ln_gdp_pc) %>%
  print()

# -------------------------------------------------------
# Save
# -------------------------------------------------------

write.csv(
  brazil_gdp_pc_panel %>% select(state, year, ln_gdp_pc),
  file = file.path(output_path, "brazil_gdp_pc_panel_log_2018_2023.csv"),
  row.names = FALSE
)

cat("\nSaved to:", file.path(output_path, "brazil_gdp_pc_panel_log_2018_2023.csv"), "\n")

# # =============================================================================
# Build state-year poverty panel for Mexico, 2018-2023 
#
# Combine two CONEVAL sources into one balanced panel (32 states x 6 years):
#   MX_AE_2022.xlsx  : multidimensional poverty (ENIGH; 2018/2020/2022 only)
#   MX_ITLP_2024.xlsx: pobreza laboral, quarterly ENOE series (Cuadro 10),
#                      which average to annual to get all years 2018-2023
# Output: poverty_panel_mx_2018_2023.csv
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)

dir <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/poverty"

ae_file   <- file.path(dir, "MX_AE_2022.xlsx")
itlp_file <- file.path(dir, "MX_ITLP_2024.xlsx")

# ---- helpers ----------------------------------------------------------------

# normalize names before matching: lowercase, trim, strip Spanish accents
# with chartr (portable on Windows, unlike iconv //TRANSLIT), collapse spaces.
norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun", x)
  gsub("\\s+", " ", x)
}

# hardcode the 32 states in official INEGI order so position = entity code;
# this gives the standard "01"-"32" keys for merging with other datasets.
ent_names <- c("Aguascalientes","Baja California","Baja California Sur",
               "Campeche","Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
               "Ciudad de M\u00e9xico","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
               "M\u00e9xico","Michoac\u00e1n de Ocampo","Morelos","Nayarit","Nuevo Le\u00f3n",
               "Oaxaca","Puebla","Quer\u00e9taro","Quintana Roo","San Luis Potos\u00ed",
               "Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala",
               "Veracruz de Ignacio de la Llave","Yucat\u00e1n","Zacatecas")
code_lookup <- setNames(sprintf("%02d", seq_along(ent_names)), norm_name(ent_names))

# map the short name variants CONEVAL uses in some tables to official names.
alias <- c("coahuila"         = "coahuila de zaragoza",
           "michoacan"        = "michoacan de ocampo",
           "veracruz"         = "veracruz de ignacio de la llave",
           "estado de mexico" = "mexico",
           "distrito federal" = "ciudad de mexico")

# convert any state-name spelling to its 2-digit code; NA means "not a
# state", which I use to skip title rows, national rows, and footnotes.
to_code <- function(x) {
  k <- norm_name(x)
  k <- ifelse(k %in% names(alias), alias[k], k)
  unname(code_lookup[k])
}

# coerce text cells to numeric; CONEVAL's 'ND' (no data) becomes NA quietly.
num <- function(x) suppressWarnings(as.numeric(x))

# locate the first cell whose text starts with `pat` and return c(row, col).
# search the flattened matrix and map back with arrayInd because trimws()
# drops dim attributes, which would break which(..., arr.ind = TRUE).
# anchor by content because readxl trims empty leading rows/columns, so
# hardcoded positions are unreliable.
find_cell <- function(m, pat) {
  v <- trimws(as.vector(m))
  v[is.na(v)] <- ""
  idx <- which(startsWith(v, pat))
  if (length(idx) == 0) NULL else arrayInd(idx[1], dim(m))[1, ]
}

# ---- 1. Multidimensional poverty (AE workbook, one sheet per state) ---------

# pull three indicators from each state sheet; the keys are the normalized
# row labels as they appear in the workbook.
labels_map <- c("poblacion en situacion de pobreza"          = "pobreza",
                "poblacion en situacion de pobreza moderada" = "pobreza_m",
                "poblacion en situacion de pobreza extrema"  = "pobreza_e")

# For every sheet: skip non-state sheets, find the "2016" header cell to
# anchor the percentage block (4 columns: 2016/2018/2020/2022; labels sit one
# column to the left), then take the three indicator rows.
ae <- lapply(excel_sheets(ae_file), function(sh) {
  code <- to_code(sh)
  if (is.na(code)) return(NULL)
  raw <- as.matrix(suppressMessages(
    read_excel(ae_file, sheet = sh, col_names = FALSE, col_types = "text")))
  yc <- find_cell(raw, "2016")
  stopifnot(!is.null(yc))
  years    <- as.integer(substr(trimws(raw[yc[1], yc[2]:(yc[2] + 3)]), 1, 4))
  pct_cols <- yc[2]:(yc[2] + 3)
  lab_col  <- yc[2] - 1
  lab      <- norm_name(raw[, lab_col])
  hit      <- which(lab %in% names(labels_map))
  bind_rows(lapply(hit, function(i)
    tibble(ent = code, year = years,
           var = labels_map[[lab[i]]],
           val = num(raw[i, pct_cols]))))
}) |>
  bind_rows() |>
  filter(year >= 2018, !is.na(val)) |>          # drop 2016: outside window
  pivot_wider(names_from = var, values_from = val)

# expect exactly 32 states x 3 measurement years; anything else means a
# sheet parsed wrong, and stop rather than merge bad data.
stopifnot(nrow(ae) == 32 * 3)

# ---- 2. Pobreza laboral (ITLP Cuadro 10, quarterly) --------------------------

raw <- as.matrix(suppressMessages(
  read_excel(itlp_file, sheet = "Cuadro 10", col_names = FALSE, col_types = "text")))

# anchor table by content: the "Nacional" cell tell which
# column holds state names; the "2005" cell tell the year header row
# (quarter labels are always the row directly below it).
nac <- find_cell(raw, "Nacional")
stopifnot(!is.null(nac))
name_col <- nac[2]
y05 <- find_cell(raw, "2005")
stopifnot(!is.null(y05))
yr_row <- y05[1]; q_row <- yr_row + 1

# Year cells are merged across each year's 4 quarters, so only the first
# quarter's column carries the year; forward-fill it across the others.
# (The harmless 'NAs introduced by coercion' warning here is non-year header
# cells failing as.integer - that is expected.)
yr_fill <- as.integer(substr(raw[yr_row, ], 1, 4))
for (j in seq_along(yr_fill))
  if (j > 1 && is.na(yr_fill[j])) yr_fill[j] <- yr_fill[j - 1]

# clean the quarter labels: uppercase FIRST, then replace L with I (v3 fix),
# because CONEVAL labels 2020-Q2 with lowercase Ls as 'll*'; strip
# the footnote asterisks (e.g. 'IV**') by keeping only I/V characters.
q_clean <- gsub("L", "I", toupper(raw[q_row, ]))
q_clean <- gsub("[^IV]", "", q_clean)
q_num   <- c("I" = 1, "II" = 2, "III" = 3, "IV" = 4)[q_clean]

# data column must have a valid quarter, an inherited year, and sit to the
# right of the state-name column. Require >= 80 columns (2005-2024 = 80
# quarters) as a guard: if this trips, the header parse went wrong.
data_cols <- which(!is.na(q_num) & !is.na(yr_fill) & seq_along(q_num) > name_col)
stopifnot(length(data_cols) >= 80)

# Put every row below the quarter header; to_code() returning NA filters
# out the Nacional row, blank rows, and footnotes automatically. 'ND' cells
# (2020-Q2 everywhere; Guerrero 2023-Q4 after Hurricane Otis) become NA.
lab <- bind_rows(lapply((q_row + 1):nrow(raw), function(i) {
  code <- to_code(raw[i, name_col])
  if (is.na(code)) return(NULL)
  tibble(ent     = code,
         year    = yr_fill[data_cols],
         quarter = q_num[data_cols],
         pl      = num(raw[i, data_cols]))
})) |>
  filter(year >= 2018, year <= 2023)

stopifnot(n_distinct(lab$ent) == 32)

# Collapse quarters to an annual mean and record how many quarters fed each
# average, so partial years (2020; Guerrero 2023) stay visible downstream.
ann <- lab |>
  group_by(ent, year) |>
  summarise(pobreza_laboral = mean(pl, na.rm = TRUE),
            n_quarters      = sum(!is.na(pl)), .groups = "drop")

# ---- 3. Merge into balanced panel -------------------------------------------

# Build the full 32x6 grid first and left-join both sources onto it, so the
# panel stays balanced and the biennial gaps (2019/2021/2023) show up as NA
# rather than missing rows. I deliberately do NOT interpolate them.
panel <- expand_grid(ent = sprintf("%02d", 1:32), year = 2018:2023) |>
  left_join(ann, by = c("ent", "year")) |>
  left_join(ae,  by = c("ent", "year")) |>
  mutate(state             = ent_names[as.integer(ent)],
         flag_partial_year = as.integer(n_quarters < 4),
         across(c(pobreza_laboral, pobreza, pobreza_m, pobreza_e),
                ~ round(.x, 4))) |>
  select(ent, state, year, pobreza_laboral, n_quarters,
         flag_partial_year, pobreza, pobreza_m, pobreza_e) |>
  arrange(ent, year)

write.csv(panel, file.path(dir, "poverty_panel_mx_2018_2023.csv"),
          row.names = FALSE, na = "")

# ---- 4. Validation (must match the reference CSV exactly) -------------------

# Assert the panel shape and the exact count of partial-year flags
# (32 states missing 2020-Q2, plus Guerrero missing 2023-Q4 = 33).
stopifnot(nrow(panel) == 192,
          sum(is.na(panel$pobreza_laboral)) == 0,
          sum(panel$flag_partial_year) == 33)

#Check against CONEVAL's published figures; every line must say OK.
check <- function(st, yr, colname, expect) {
  v <- round(panel[[colname]][panel$state == st & panel$year == yr], 1)
  cat(sprintf("%-15s %d %-15s %.1f (expect %.1f) %s\n",
              st, yr, colname, v, expect,
              ifelse(v == expect, "OK", "** MISMATCH **")))
}
check("Chiapas",        2022, "pobreza",   67.4)
check("Guerrero",       2022, "pobreza",   60.4)
check("Aguascalientes", 2018, "pobreza",   26.3)
check("Chiapas",        2022, "pobreza_e", 28.2)
check("Aguascalientes", 2018, "pobreza_laboral", 35.7)
check("Guerrero",       2023, "pobreza_laboral", 59.8)
cat("\nPanel written to:", file.path(dir, "poverty_panel_mx_2018_2023.csv"), "\n")
# =============================================================================
# BUILD BRAZIL STATE-YEAR POVERTY PANEL, 2018-2023
#
# Source: IBGE, Sintese de Indicadores Sociais 2025 (plano tabular),
#   Tabela 2.18.
#
# Output: poverty/brazil_poverty_panel_2018_2023.csv
# =============================================================================

library(readxl)
library(dplyr)

dir_pov <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final/poverty"
sis_file <- file.path(dir_pov, "bra_poverty_sis2025.xls")

# The same accent-free state names used across the Brazil pipeline; matching
# against this list keeps UF rows and drops Brasil, the five regions, and
# the metropolitan-region rows automatically.
uf_names <- c("Rondonia","Acre","Amazonas","Roraima","Para","Amapa",
              "Tocantins","Maranhao","Piaui","Ceara","Rio Grande do Norte","Paraiba",
              "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais","Espirito Santo",
              "Rio de Janeiro","Sao Paulo","Parana","Santa Catarina","Rio Grande do Sul",
              "Mato Grosso do Sul","Mato Grosso","Goias","Distrito Federal")

strip_acc <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE))
    stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
  else chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4\u00e3\u00f5\u00e7",
              "aeiouaeoaoc", trimws(as.character(x)))
}

# Reads one year sheet. Column layout (verified against the 2025 workbook):
# col 1 = geography, col 3 = % below US$2.15, col 5 = % below US$6.85.
# A header guard confirms the layout before any values are harvested, so a
# future re-ordering of columns by IBGE fails loudly instead of silently.
read_sis_year <- function(yr) {
  raw <- suppressMessages(read_excel(sis_file, sheet = as.character(yr),
                                     col_names = FALSE, col_types = "text"))
  hdr <- apply(raw[1:8, ], 2, function(col) paste(na.omit(col), collapse = " "))
  stopifnot(grepl("2,15", hdr[3]), grepl("6,85", hdr[5]))
  raw %>%
    transmute(state       = strip_acc(...1),
              extreme_215 = suppressWarnings(as.numeric(...3)),
              poverty_685 = suppressWarnings(as.numeric(...5))) %>%
    filter(state %in% uf_names) %>%
    distinct(state, .keep_all = TRUE) %>%   # UF row precedes any metro row
    mutate(year = yr)
}

br_poverty <- bind_rows(lapply(2018:2023, read_sis_year)) %>%
  select(state, year, poverty_685, extreme_215) %>%
  arrange(state, year)

# Validation: balanced panel, no NAs, and spot checks against the values
# extracted independently from this workbook (plus the known geography:
# Santa Catarina lowest, Maranhao/Acre highest).
stopifnot(nrow(br_poverty) == 27 * 6,
          n_distinct(br_poverty$state) == 27,
          sum(is.na(br_poverty)) == 0)

chk <- function(st, yr, col, expect) {
  v <- round(br_poverty[[col]][br_poverty$state == st &
                                 br_poverty$year == yr], 1)
  cat(sprintf("%-15s %d %-12s %.1f (expect %.1f) %s\n", st, yr, col, v,
              expect, ifelse(v == expect, "OK", "** MISMATCH **")))
}
chk("Maranhao",       2018, "poverty_685", 63.2)
chk("Maranhao",       2023, "poverty_685", 51.2)
chk("Santa Catarina", 2023, "poverty_685", 11.5)
chk("Acre",           2023, "extreme_215", 13.4)

write.csv(br_poverty,
          file.path(dir_pov, "brazil_poverty_panel_2018_2023.csv"),
          row.names = FALSE)
cat("\nBrazil poverty panel written:",
    file.path(dir_pov, "brazil_poverty_panel_2018_2023.csv"), "\n")
# =============================================================================
# MASTER PANEL MERGE + REGRESSION ANALYSIS
# =============================================================================
#
# It merges panels, estimates the models, and writes every output file. 
#
# PIPELINE ORDER (each script leaves its objects in memory for the next):
#   01_build_panels.R          homicides, population, legislatures, capacity,
#                              GDP, Mexican poverty
#   02_legislature_fixes.R     Brazil 2014 step function; CDMX patches;
#                              Mexico 2021 election-calendar fill
#   03_build_brazil_poverty.R  IBGE/SIS Tabela 2.18 -> br_poverty
#   04_master.R                THIS FILE
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(fixest)

base_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final"
reg_path  <- file.path(base_path, "regression")
dir.create(reg_path, recursive = TRUE, showWarnings = FALSE)

# Guards: upstream objects must be complete before anything is merged.
stopifnot(exists("br_leg"),     nrow(br_leg) == 162,
          exists("br_poverty"), nrow(br_poverty) == 162,
          sum(is.na(br_leg$pct_women_legislature)) == 0)

# -----------------------------------------------------------------------------
# 1. CANONICAL MEXICAN STATE NAMES
# Every source spells states differently; all keys are mapped to one ASCII
# canonical name so joins cannot silently drop rows.
# -----------------------------------------------------------------------------
canon_mx <- function(x) {
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun",
              tolower(trimws(as.character(x))))
  x <- gsub("\\s+", " ", x)
  recode(x,
         "coahuila de zaragoza"            = "coahuila",
         "michoacan de ocampo"             = "michoacan",
         "veracruz de ignacio de la llave" = "veracruz",
         "mexico"                          = "estado de mexico",
         "distrito federal"                = "ciudad de mexico",
         .default = x)
}

# -----------------------------------------------------------------------------
# 2. MEXICO: MERGE  (DV + IV + moderator + controls)
# -----------------------------------------------------------------------------
mx_poverty <- read_csv(
  file.path(base_path, "poverty/poverty_panel_mx_2018_2023.csv"),
  show_col_types = FALSE) %>%
  mutate(key = canon_mx(state)) %>%
  select(key, year, pobreza_laboral, flag_partial_year,
         pobreza, pobreza_m, pobreza_e)

mx <- mexico_panel %>%
  mutate(key = canon_mx(state)) %>%
  select(key, state, year, female_homicides, female_population,
         female_homicide_rate) %>%
  left_join(mxleg_panel           %>% mutate(key = canon_mx(state)) %>%
              select(key, year, pct_women_legislature), by = c("key","year")) %>%
  left_join(mxstate_capacity_panel %>% mutate(key = canon_mx(state)) %>%
              select(key, year, state_capacity, log_state_capacity),
            by = c("key","year")) %>%
  left_join(mxgdp_pc_panel        %>% mutate(key = canon_mx(state_name)) %>%
              select(key, year, gdp_pc, ln_gdp_pc), by = c("key","year")) %>%
  left_join(mx_poverty, by = c("key","year")) %>%
  mutate(country = "Mexico", poverty = pobreza_laboral)

stopifnot(nrow(mx) == 192,
          !anyNA(mx[c("pct_women_legislature","log_state_capacity",
                      "ln_gdp_pc","poverty")]))
# RESULT (final run): 192 rows; 0 missing in all four key variables.

# -----------------------------------------------------------------------------
# 3. BRAZIL: CAPACITY + MERGE
# SICONFI identifies states by two-letter sigla, so the sigla is converted to
# the full state name via uf_lookup before joining the population panel.
# -----------------------------------------------------------------------------
br_capacity <- br_total %>%
  left_join(uf_lookup, by = c("UF" = "SG_UF")) %>%
  left_join(brazil_total_pop, by = c("state","year")) %>%
  mutate(state_capacity     = total_expenditure / total_population,
         log_state_capacity = log(state_capacity)) %>%
  select(state, year, state_capacity, log_state_capacity)

br <- brazil_panel %>%
  select(state, year, female_homicides, female_population,
         female_homicide_rate) %>%
  left_join(br_leg,      by = c("state","year")) %>%
  left_join(br_capacity, by = c("state","year")) %>%
  left_join(brazil_gdp_pc_panel %>% select(state, year, gdp_pc, ln_gdp_pc),
            by = c("state","year")) %>%
  left_join(br_poverty %>% select(state, year, poverty_685, extreme_215),
            by = c("state","year")) %>%
  mutate(country = "Brazil", poverty = poverty_685)

stopifnot(nrow(br) == 162,
          !anyNA(br[c("pct_women_legislature","log_state_capacity",
                      "ln_gdp_pc","poverty")]))
# RESULT (final run): 162 rows; 0 missing in all four key variables.

# -----------------------------------------------------------------------------
# 4. LAGS AND POOLED FILE
# Representation in t-1 predicts homicide in t; the first year (2018) drops
# from estimation in each country.
# -----------------------------------------------------------------------------
add_lag <- function(d) d %>%
  arrange(state, year) %>% group_by(state) %>%
  mutate(l_femrep = lag(pct_women_legislature, order_by = year)) %>%
  ungroup()

mx <- add_lag(mx)
br <- add_lag(br)

panel_all <- bind_rows(
  mx %>% select(country, state, year, female_homicide_rate, l_femrep,
                log_state_capacity, ln_gdp_pc, poverty, flag_partial_year),
  br %>% select(country, state, year, female_homicide_rate, l_femrep,
                log_state_capacity, ln_gdp_pc, poverty) %>%
    mutate(flag_partial_year = 0L))

# Regressors are standardized WITHIN country for the pooled model so that
# pesos/reais and different poverty definitions never share a coefficient.
panel_z <- panel_all %>%
  group_by(country) %>%
  mutate(across(c(l_femrep, log_state_capacity, ln_gdp_pc, poverty),
                ~ as.numeric(scale(.x)), .names = "z_{.col}")) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 5. MAIN MODELS  (two-way FE, SEs clustered by state)
# -----------------------------------------------------------------------------
m_mx_base <- feols(female_homicide_rate ~ l_femrep + log_state_capacity +
                     ln_gdp_pc + poverty | state + year,
                   data = mx, cluster = ~state)
m_mx_int  <- feols(female_homicide_rate ~ l_femrep * log_state_capacity +
                     ln_gdp_pc + poverty | state + year,
                   data = mx, cluster = ~state)
m_br_base <- feols(female_homicide_rate ~ l_femrep + log_state_capacity +
                     ln_gdp_pc + poverty | state + year,
                   data = br, cluster = ~state)
m_br_int  <- feols(female_homicide_rate ~ l_femrep * log_state_capacity +
                     ln_gdp_pc + poverty | state + year,
                   data = br, cluster = ~state)
m_pool    <- feols(female_homicide_rate ~ z_l_femrep * z_log_state_capacity +
                     z_ln_gdp_pc + z_poverty | state + year,
                   data = panel_z, cluster = ~state)

# RESULTS (final run; full tables in regression/results_main.txt):
#   l_femrep,  MX base : -0.051 (0.032)   n = 160   not significant
#   l_femrep,  BR base : -0.030 (0.027)   n = 135   not significant
#   z_l_femrep, pooled : -0.307 (0.149)*  n = 295   p < .05
#   interaction (femrep x capacity): positive, small, never significant
#     MX: 0.049 (0.104)   BR: 0.007 (0.007)   pooled z: 0.091 (0.070)
#   poverty controls: ~0 and insignificant everywhere ->
#     representation estimates are not absorbing poverty dynamics.

# -----------------------------------------------------------------------------
# 6. MARGINAL EFFECTS OF l_femrep AT CAPACITY QUANTILES
# The raw coefficients of an uncentered interaction are not interpretable;
# these marginal effects are the quantity reported in the paper (Table 2).
# -----------------------------------------------------------------------------
me_lines <- function(model, d, label) {
  qs <- quantile(d$log_state_capacity, c(.25,.5,.75), na.rm = TRUE)
  co <- coef(model); vc <- vcov(model)
  c(sprintf("Marginal effect of lagged female representation (%s):", label),
    vapply(qs, function(q) {
      me <- co["l_femrep"] + co["l_femrep:log_state_capacity"]*q
      se <- sqrt(vc["l_femrep","l_femrep"] +
                   q^2*vc["l_femrep:log_state_capacity",
                          "l_femrep:log_state_capacity"] +
                   2*q*vc["l_femrep","l_femrep:log_state_capacity"])
      sprintf("  log capacity = %.2f : ME = %+.4f (SE %.4f, t = %.2f)",
              q, me, se, me/se)
    }, character(1)), "")
}
writeLines(c(me_lines(m_mx_int, mx, "Mexico"),
             me_lines(m_br_int, br, "Brazil")),
           file.path(reg_path, "table2_marginal_effects.txt"))

# RESULTS (final run):
#   Mexico: -0.0555 (t=-1.74) at p25  ->  -0.0424 (t=-1.12) at p75
#   Brazil: -0.0434 (t=-1.50) at p25  ->  -0.0382 (t=-1.39) at p75
#   Effects attenuate slightly as capacity rises (opposite of H2) but are
#   never significant -> no evidence of capacity moderation.

# -----------------------------------------------------------------------------
# 7. ROBUSTNESS  (same specification, alternative forms and samples)
# -----------------------------------------------------------------------------
m_mx_asinh  <- feols(asinh(female_homicide_rate) ~ l_femrep *
                       log_state_capacity + ln_gdp_pc + poverty |
                       state + year, data = mx, cluster = ~state)
m_mx_pois   <- fepois(female_homicides ~ l_femrep * log_state_capacity +
                        ln_gdp_pc + poverty +
                        offset(log(female_population)) | state + year,
                      data = mx, cluster = ~state)
m_mx_no20   <- feols(female_homicide_rate ~ l_femrep * log_state_capacity +
                       ln_gdp_pc + poverty | state + year,
                     data = filter(mx, year != 2020), cluster = ~state)
m_br_asinh  <- feols(asinh(female_homicide_rate) ~ l_femrep *
                       log_state_capacity + ln_gdp_pc + poverty |
                       state + year, data = br, cluster = ~state)
m_br_pois   <- fepois(female_homicides ~ l_femrep * log_state_capacity +
                        ln_gdp_pc + poverty +
                        offset(log(female_population)) | state + year,
                      data = br, cluster = ~state)

# RESULTS (final run; full tables in regression/results_robustness_*.txt):
#   asinh + Poisson reproduce the negative sign in both countries;
#   MX Poisson: l_femrep -0.299 and interaction 0.030, each p < .10 only.
#   Dropping 2020 flips the (insignificant) MX point estimates ->
#   2020 is an influential year; reported transparently in the paper.
#   (The earlier no-flags check was dropped: the partial-quarter flag covers
#   all of 2020 plus Guerrero 2023, making it redundant with no-2020.)

# -----------------------------------------------------------------------------
# 8. EXPORT: TABLES AND FINAL PANEL
# -----------------------------------------------------------------------------


# =============================================================================
# FIGURES
# Two figures for the Results section, built from the fitted models and
# panels. 
# =============================================================================

base_path <- "C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/final"
reg_path  <- file.path(base_path, "regression")
fig_path  <- file.path(base_path, "figures")
dir.create(reg_path, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

# Helper: write etable output to a plain-text file regardless of fixest
# version, by capturing the printed table rather than relying on etable's
# own file= argument (which is silently ignored on older fixest).
write_table <- function(tbl_call, path) {
  out <- capture.output(print(tbl_call))
  writeLines(out, path)
  cat(sprintf("  wrote %-32s (%d lines, exists=%s)\n",
              basename(path), length(out), file.exists(path)))
}

cat("Writing regression tables to:", reg_path, "\n")

# Table 1: main results (5-model table)
write_table(
  etable(m_mx_base, m_mx_int, m_br_base, m_br_int, m_pool,
         headers = c("MX base","MX interact","BR base","BR interact","Pooled (z)"),
         digits = 3, fitstat = ~ n + r2 + war2),
  file.path(reg_path, "table1_main_results.txt"))

tex_main <- etable(m_mx_base, m_mx_int, m_br_base, m_br_int, m_pool,
                   headers = c("MX base","MX interact","BR base","BR interact","Pooled (z)"),
                   digits = 3, fitstat = ~ n + r2 + war2, tex = TRUE)
writeLines(tex_main, file.path(reg_path, "table1_main_results.tex"))
cat(sprintf("  wrote %-32s (exists=%s)\n", "table1_main_results.tex",
            file.exists(file.path(reg_path, "table1_main_results.tex"))))

# Table 3a/3b: robustness (Table 2 = marginal effects, written in section 6)
write_table(
  etable(m_mx_int, m_mx_asinh, m_mx_pois, m_mx_no20,
         headers = c("OLS rate","asinh","Poisson","no 2020"),
         digits = 3, fitstat = ~ n + r2),
  file.path(reg_path, "table3a_robustness_mx.txt"))

write_table(
  etable(m_br_int, m_br_asinh, m_br_pois,
         headers = c("OLS rate","asinh","Poisson"),
         digits = 3, fitstat = ~ n + r2),
  file.path(reg_path, "table3b_robustness_br.txt"))

# -----------------------------------------------------------------------------
# Final panel
# -----------------------------------------------------------------------------
final_panel <- bind_rows(
  mx %>% transmute(country, state, year, female_homicides, female_population,
                   female_homicide_rate, pct_women_legislature, l_femrep,
                   state_capacity, log_state_capacity, gdp_pc, ln_gdp_pc,
                   poverty, poverty_flag_partial = flag_partial_year,
                   pobreza_mmd = pobreza, pobreza_e_mmd = pobreza_e,
                   poverty_extreme = NA_real_),
  br %>% transmute(country, state, year, female_homicides, female_population,
                   female_homicide_rate, pct_women_legislature, l_femrep,
                   state_capacity, log_state_capacity, gdp_pc, ln_gdp_pc,
                   poverty, poverty_flag_partial = 0L,
                   pobreza_mmd = NA_real_, pobreza_e_mmd = NA_real_,
                   poverty_extreme = extreme_215)) %>%
  arrange(country, state, year)

stopifnot(nrow(final_panel) == 354, n_distinct(final_panel$state) == 59)

write_csv(mx,          file.path(reg_path,  "mexico_master_panel.csv"))
write_csv(br,          file.path(reg_path,  "brazil_master_panel.csv"))
write_csv(final_panel, file.path(base_path, "final_panel_all_variables.csv"))

# -----------------------------------------------------------------------------
# VERIFICATION
# -----------------------------------------------------------------------------
expected <- c(
  file.path(reg_path, "table1_main_results.txt"),
  file.path(reg_path, "table1_main_results.tex"),
  file.path(reg_path, "table2_marginal_effects.txt"),
  file.path(reg_path, "table3a_robustness_mx.txt"),
  file.path(reg_path, "table3b_robustness_br.txt"),
  file.path(reg_path, "mexico_master_panel.csv"),
  file.path(reg_path, "brazil_master_panel.csv"),
  file.path(base_path, "final_panel_all_variables.csv"),
  file.path(fig_path, "figure1_marginal_effects_capacity.png"),
  file.path(fig_path, "figure2_descriptive_trends.png")
)
cat("\n--- FILE CHECK ---\n")
for (f in expected) {
  cat(sprintf("%-65s exists=%-5s %s\n", f, file.exists(f),
              if (file.exists(f)) paste(file.size(f), "bytes") else "MISSING"))
}

library(ggplot2)

fig_path <- file.path(base_path, "figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# FIGURE 1: Marginal effect of lagged female representation across the
# observed range of state capacity, with 95% CIs, both countries.
# This is the figure for the moderation hypothesis (H2): if capacity
# moderated the effect, the line would have a visible slope and the CI
# would exclude zero somewhere. Instead both lines are nearly flat and the
# band covers zero throughout -- a null result shown directly.
# -----------------------------------------------------------------------------

me_curve <- function(model, d, country, n = 50) {
  x  <- seq(min(d$log_state_capacity), max(d$log_state_capacity), length.out = n)
  co <- coef(model); vc <- vcov(model)
  me <- co["l_femrep"] + co["l_femrep:log_state_capacity"] * x
  se <- sqrt(vc["l_femrep","l_femrep"] +
               x^2 * vc["l_femrep:log_state_capacity","l_femrep:log_state_capacity"] +
               2*x * vc["l_femrep","l_femrep:log_state_capacity"])
  tibble(country = country, log_state_capacity = x, me = me,
         lo = me - 1.96*se, hi = me + 1.96*se)
}

me_df <- bind_rows(
  me_curve(m_mx_int, mx, "Mexico"),
  me_curve(m_br_int, br, "Brazil"))

p1 <- ggplot(me_df, aes(log_state_capacity, me, color = country, fill = country)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(Mexico = "#2c7fb8", Brazil = "#41ab5d")) +
  scale_fill_manual(values = c(Mexico = "#2c7fb8", Brazil = "#41ab5d")) +
  labs(
    title = "Effect of lagged female representation on female homicide rates,\nby state capacity",
    subtitle = "Marginal effects with 95% confidence intervals (two-way FE models)",
    x = "Log state government expenditure per capita",
    y = "Marginal effect on female homicide rate\n(per 100,000 women, per 1 pp representation)",
    color = "Country", fill = "Country") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(fig_path, "figure1_marginal_effects_capacity.png"), p1,
       width = 7, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# FIGURE 2: Descriptive trends -- mean female homicide rate and mean female
# representation over time, by country (two panels). Establishes the raw
# patterns the models above are summarizing, and visualizes the compressed
# within-country variation in representation (flagged in Limitations).
# -----------------------------------------------------------------------------

trend_df <- bind_rows(
  mx %>% transmute(country="Mexico", year, female_homicide_rate, pct_women_legislature),
  br %>% transmute(country="Brazil", year, female_homicide_rate, pct_women_legislature)
) %>%
  group_by(country, year) %>%
  summarise(homicide_rate = mean(female_homicide_rate),
            pct_women     = mean(pct_women_legislature), .groups = "drop")

p2a <- ggplot(trend_df, aes(year, homicide_rate, color = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c(Mexico = "#2c7fb8", Brazil = "#41ab5d")) +
  labs(title = "Mean female homicide rate", x = NULL,
       y = "Per 100,000 women", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "none")

p2b <- ggplot(trend_df, aes(year, pct_women, color = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c(Mexico = "#2c7fb8", Brazil = "#41ab5d")) +
  labs(title = "Mean % women in state legislature", x = NULL,
       y = "Percent", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)
p2 <- (p2a / p2b) +
  plot_annotation(title = "State-level trends, 2018-2023",
                  theme = theme(plot.title = element_text(face = "bold")))

ggsave(file.path(fig_path, "figure2_descriptive_trends.png"), p2,
       width = 6, height = 7, dpi = 300)

cat("Figures written to:", fig_path, "\n")

# =============================================================================
# FORMATTED TABLES: main results 
# =============================================================================

if (!requireNamespace("modelsummary", quietly = TRUE)) install.packages("modelsummary")
if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
library(modelsummary)
library(gt)
library(webshot2)

reg_path <- file.path(base_path, "regression")

models <- list(
  "MX base"     = m_mx_base,
  "MX interact" = m_mx_int,
  "BR base"     = m_br_base,
  "BR interact" = m_br_int,
  "Pooled (z)"  = m_pool
)

# Clean, readable coefficient labels (raw + standardized versions share a row)
coef_map <- c(
  "l_femrep"                          = "Lagged % women legislature",
  "z_l_femrep"                        = "Lagged % women legislature (z)",
  "log_state_capacity"                = "Log state capacity",
  "z_log_state_capacity"              = "Log state capacity (z)",
  "ln_gdp_pc"                         = "Log GDP per capita",
  "z_ln_gdp_pc"                       = "Log GDP per capita (z)",
  "poverty"                           = "Poverty",
  "z_poverty"                         = "Poverty (z)",
  "l_femrep:log_state_capacity"       = "Representation x Capacity",
  "z_l_femrep:z_log_state_capacity"   = "Representation x Capacity (z)"
)

# modelsummary writes directly to file when `output` is a filename;
# the format is inferred from the extension. This avoids gtsave() entirely.
modelsummary(
  models, coef_map = coef_map,
  stars   = c('*' = .05, '**' = .01, '***' = .001),
  gof_map = c("nobs", "r.squared"),
  title   = "Table 1. Female representation and female homicide rates (two-way FE, SEs clustered by state)",
  notes   = "Standard errors in parentheses, clustered by state. Dependent variable: female homicide rate per 100,000 women. Female representation is lagged one year.",
  output  = file.path(reg_path, "table1_main_results.docx"))

modelsummary(
  models, coef_map = coef_map,
  stars   = c('*' = .05, '**' = .01, '***' = .001),
  gof_map = c("nobs", "r.squared"),
  title   = "Table 1. Female representation and female homicide rates (two-way FE, SEs clustered by state)",
  notes   = "Standard errors in parentheses, clustered by state. Dependent variable: female homicide rate per 100,000 women. Female representation is lagged one year.",
  output  = file.path(reg_path, "table1_main_results.png"))

cat("Formatted table written to:\n  ",
    file.path(reg_path, "table1_main_results.png"), "\n  ",
    file.path(reg_path, "table1_main_results.docx"), "\n")
