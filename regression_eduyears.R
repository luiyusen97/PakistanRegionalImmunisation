library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)

# I'm going to try to do this with years of education instead of categories of education

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
# distancetobhu: distance to nearest basic health unit (sgq10_71)
# education: highest education received (scq04) Remember to only regress for female ppl. At least primary education=1, else 0
# primary education is at least Class VIII, which is factored as 8
# we are going to regress with regions as the individual unit
# for every hhcode, add the idc value to the end, call it hhcode_idc
# merge dataframes by new edited hhcode_idc (household code with idc)
# create new column with district code (first 4 digits in hhcode)
# group_by district code
# remove non-needed columns, remove NA values
# count number of people per region, number of females per region
# immunisation: number of people fully immunised (1)/total number of people (1+0)
# age: mean age per region
# distance to basic health unit (mean distance to nearest basic health unit per region)
# education: number of females with at least primary education/total number of females in a region
# regress immunisation on age, distance, education
# example merge code: merge(distancetobhu, immunisation, by.x = "hhcode", by.y = "hhcode", all = F)

education <- mutate(education, hhcodeidc = (hhcode*10)+idc)                                        # combine household code and identification code
education <- education[!duplicated(education[,"hhcodeidc", drop = F]),]                            # remove duplicate combined identification codes
personaldetails <- mutate(personaldetails, hhcodeidc = (hhcode*10)+idc)
personaldetails <- personaldetails[!duplicated(personaldetails[,"hhcodeidc",
                                                               drop = F]),]

education <- education[, c("hhcode", "hhcodeidc", "scq04", "psu", "province",
                           "region", "district", "hh", "sec", "idc"), drop = F]                    # take only needed columns
personaldetails <- personaldetails[, c("hhcode", "hhcodeidc", "psu", "province",
                                       "region", "district", "sec", "idc",
                                       "sbq04", "age"), drop = F]                                  # take only needed columns
individual_adults <- merge(education, personaldetails, by.x = "hhcodeidc",
                           by.y = "hhcodeidc", all = F)                                            # merge the dataframes based on combined ident codes
individual_adults <- individual_adults[!is.na(individual_adults$scq04),]                           # remove NA values, other columns had full data
individual_adults <- individual_adults[
    individual_adults$hhcode.x==individual_adults$hhcode.y, ]                                      # remove duplicate ident codes
individual_adults <- individual_adults[
    individual_adults$idc.x==individual_adults$idc.y, ]
individual_adults <- subset(individual_adults, select = -seq(11,17))                               # remove duplicate columns
colnames(individual_adults) <- c(
    "complete_identcode", "householdcode", "highest_edu", "psu", "province",
    "region", "district", "household", "sec", "ident_code", "gender", "age"    
)                                                                                                  # more readable variables
individual_adults <- mutate(individual_adults,
                            districtcode = as.numeric(str_sub(as.character(complete_identcode),
                                                              1, 4)
                            )
)                                                                      # extract district code from complete ident code
districtcode_vector <- unique(individual_adults$districtcode)
individual_adults$districtcode <- as.factor(individual_adults$districtcode)
individual_adults$highest_edu <- as.numeric(individual_adults$highest_edu)
individual_adults <- split(individual_adults, individual_adults$districtcode)
median_education <- c()
mean_age <- c()
female_edu <- c()
for (frame in individual_adults){
    median_education_category <- median(frame$highest_edu)                                         # use of median value because it is independent of units used
    median_education <- c(median_education, median_education_category)
    mean_age_datum <- mean(frame$age)
    mean_age <- c(mean_age, mean_age_datum)
    female_no <- nrow(frame[frame$gender == 2,])
    female_no_primaryedu <- nrow(frame[(frame$gender==2)&(frame$highest_edu>=9),])
    female_edu_region <- female_no_primaryedu/female_no
    female_edu <- c(female_edu, female_edu_region)
}
rm(frame)
rm(median_education_category)
rm(mean_age_datum)
rm(female_no)
rm(female_no_primaryedu)
rm(female_edu_region)
# individual_adults <- group_by(individual_adults, districtcode)
education_levels <- c("Below Class-I", "class I","Class II", "Class III",
                      "Class IV",
                      "Class V", "Class VI", "Class VII", "Class VIII",
                      "Class IX", "Class X", "Polytechnic Diploma",
                      "F.A/F.Sci/I.com", "Bachelors", "Masters",
                      "Degree in Engineering", "Degree in Medicine",
                      "Degree in Agriculture", "Degree in Law",
                      "PhD", "Others")
# individual_adults$highest_edu <- as.factor(individual_adults$highest_edu)
# levels(individual_adults$highest_edu) <- education_levels
# gender_levels <- c("male", "female")
# individual_adults$gender <- as.factor(individual_adults$gender)
# levels(individual_adults$gender) <- gender_levels

distance <- distancetobhu[, c(seq(1,5), 28)]                                                       # select distance to health facility column                    # convert distance to factor variable
# distance$sgq10_71 <- as.factor(distance$sgq10_71)
# levels(distance$sgq10_71) <- distance_factors
colnames(distance)[6] <- "distance_to_healthfacility"
distance <- mutate(distance, districtcode = as.numeric(str_sub(as.character(hhcode),
                                                               1, 4)
))
distance$distance_to_healthfacility <- as.numeric(distance$distance_to_healthfacility)
# distance <- group_by(distance, districtcode)
distance$districtcode <- as.factor(distance$districtcode)
distance <- split(distance, distance$districtcode, drop = T)
# all the district codes line up between the dataframes, so all we need to do is create vectors of the mean values and attach them side-by-side
median_distance <- c()                                                                             # use of median value because it is independent of units used
for (frame in distance){
    median_distance_category <- median(frame$distance_to_healthfacility)
    median_distance <- c(median_distance, median_distance_category)
}
rm(frame)
rm(median_distance_category)

immunisation <- immunisation[, c(1, 15, 18, 21, 26)]
immunisation <- na.omit(immunisation)
immunisation <- mutate(immunisation, full_immunisation = (((shq6a==3)|(shq6a==2))&((shq6d==3)|(shq6d==2))&((shq6g==3)|(shq6g==2))&((shq6l==3)|(shq6l==2))))
immunisation <- mutate(immunisation, districtcode = as.numeric(str_sub(as.character(hhcode), 1, 4)))
immunisation$districtcode <- as.factor(immunisation$districtcode)
immunisation <- split(immunisation, immunisation$districtcode)
immunised_proportion <- c()
for (frame in immunisation){
    proportion_of_children_immunised <- (nrow(frame)-sum(frame$full_immunisation))/nrow(frame)
    immunised_proportion <- c(immunised_proportion, proportion_of_children_immunised)
}
rm(frame)
rm(proportion_of_children_immunised)

regionaldata <- data.frame(districtcode_vector, median_distance, median_education, mean_age, female_edu, immunised_proportion)
regionaldata <- mutate(regionaldata, age_squared = (mean_age)**2)

education_levels <- c("Below Class-I", "class I","Class II", "Class III",
                      "Class IV",
                      "Class V", "Class VI", "Class VII", "Class VIII",
                      "Class IX", "Class X", "Polytechnic Diploma",
                      "F.A/F.Sci/I.com", "Bachelors", "Masters",
                      "Degree in Engineering", "Degree in Medicine",
                      "Degree in Agriculture", "Degree in Law",
                      "PhD", "Others")
regionaldata$median_education <- as.factor(as.integer(regionaldata$median_education))
levels(regionaldata$median_education) <- education_levels

distance_factors <- c("0-14min", "15-29min", "30-44min", "45-59min", "60+min") 
regionaldata$median_distance <- as.factor(as.integer(regionaldata$median_distance))
levels(regionaldata$median_distance) <- distance_factors
regression_report <- summary(lm(immunised_proportion ~ female_edu + mean_age + age_squared + median_education + median_distance, regionaldata))
print(regression_report)

# if using robust se
# robust_t <- coeftest(regression_report, vcov = vcovHC(regression_report, type = "HC0"))
# print(robust_t)

# need to run the heteroscedascity tests
regionaldata <- mutate(regionaldata, residuals_squared = (regression_report$residuals)**2)
ramsey_test <- summary(lm(residuals_squared~female_edu + mean_age + age_squared + median_education + median_distance, regionaldata))
print(ramsey_test)
# Reject for 0.01 significance level. Accept for 0.05 significance level
# set to 0.01, accept H_0: MLR5 holds

# shapiro-wilks test for normality of residuals
print(shapiro.test(regression_report$residuals))
# it's not normal shit