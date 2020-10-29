library(tidyverse)
library(haven)

# read the data and rename the datasets
personaldetails <- read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\plist.dta")
education <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_c.dta")
income <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_e.dta")
distancetobhu <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_g.dta")
immunisation <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_h.dta")
# columns that are needed, name (column name):
# personaldetails: gender (sbq04), age (age)
# income: screw income, I have no idea how to clean this shit
# immunisation: bcg (shq6b), penta3 (shq6d), polio3 (shq6g), measles2 (shq6l), (1,2,4)=yes(1), 3=no(0) recode