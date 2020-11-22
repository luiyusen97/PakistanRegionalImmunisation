library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
library(broom)

# read the data and rename the datasets
personaldetails <- read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\plist.dta")
education <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_c.dta")
distancetobhu <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_g.dta")
immunisation <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_h.dta")

income <-read_dta(file = "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_e.dta")
full_filepaths <- c("C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_a.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_b.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_d.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_f1.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_f2.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_i.dta",
                    "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\PakistanRegionalImmunisation\\Datasets\\Data in stata\\sec_j.dta")
listdata <- lapply(full_filepaths, read_dta)
bhu_usedata <- listdata[[7]][, seq(1, 10)]