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
# distancetobhu: distance to nearest basic health unit (sgq10_81)
# education: highest education received (scq04) Remember to only regress for female ppl. At least primary education=1, else 0
# we are going to regress with regions as the individual unit
# merge dataframes by hhcode (household code) 
# create new column with district code (first 4 digits in hhcode)
# group_by district code
# remove non-needed columns, remove NA values
# count number of people per region, number of females per region
# immunisation: number of people fully immunised (1)/total number of people (1+0)
# age: mean age per region
# distance to basic health unit (mean distance to nearest basic health unit per region)
# education: number of females with at least primary education/total number of females in a region
# regress immunisation on age, distance, education