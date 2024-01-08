
## MenuEnergyLabelling model: A comparative assessment model to evaluate health impact of the menu energy labelling policy Copyright (C)
## 2023 Zoe Colombet

## MenuEnergyLabelling is free code; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.

## This program is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
## PARTICULAR PURPOSE.  See the GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/> or write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.


################################################################################
#                                Note for users                                #
################################################################################
# For using this code, you have to import and prepare the database 
# - Mortality
# - Population composition
# - Population projection
# you need to change / complete the info needed where the code specify "## TO COMPLETE"



################################################################################
#                   Summary of this MenuEnergyLabelling code                   #
################################################################################
# Import all database necessary
#      - 1. IMPORT THE MORTALITY DATABASE  
#      - 2. IMPORT THE POPULATION COMPOSITION DATABASE 
#      - 3. IMPORT THE POPULATION PROJECTION DATABASE 
#      - 4. MERGE  THE MORTALITY & POPULATION DATABASE 
#      - 5. IMPORT THE DIETARY INTAKE DATABASE 
#      - 6. IMPORT THE POLICY EFFECT 
#      - 7. IMPORT THE RR NEEDED FOR THE MODEL 

# 8. Run the Montecarlo containing the model 
#       Step A: Change in diet																				
#       Step B: Change in health:																		

# 9. Results
#       Step C:Outputs (e.g., DPPs):	absolute difference between baseline and scenarios						

# 10. Sensi without under-reporters



################################################################################
##                              PACKAGE NEEDED                                ##
################################################################################
#Run before go any further
#define your setwd

#load packages
library(haven)
library(ggplot2)
library(tidyverse) 
library(readr)
library(demography)
library(data.table)


# Functions ###
guardar.archivo <- function(x) {
  saveRDS(x, file=paste0(substitute(x),".rds"))
}
cargar.archivo <- function(x) {
  readRDS(paste0(substitute(x),".rds"))
}

`%nin%` = Negate(`%in%`)

################################################################################
##                        PREPARATION FOR THE MODEL                           ##
################################################################################

## Putting the years needed for your model
#Years studies: years from the baseline of the studies to the end of the projection
yearsstudies <- c(2022:2042) 
yearsstudiesplus1 <- c(2023:2042) #begin 1 year after the baseline



################################################################################
####                   1. IMPORT THE MORTALITY DATABASE                     ####
################################################################################

## TO COMPLETE : import the mortality data you need. 

# Here, I import CVD mortality from England by IMD
#Source
#Office for National Statistics (ONS). 2018 (accessed 8 Nov 2022)
#Deaths by selected causes and populations, both by deprivation decile areas, 
#5 year age groups and sex, England and Wales, registered years 1981 to 2016. 
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/008024deathsbyselectedcausesandpopulationsbothbydeprivationdecileareas5yearagegroupsandsexenglandandwalesregisteredyears1981to2016 


## CVD mortality dataframe ###
#CHD and stroke mortality data from the UK Office for National Statistics

# Keep CVD mortality for 25+ years old  
# Coronary heart disease 
#    ICD-9 codes (1981-2000) : 410-414 
#    ICD-10 codes (2001-2016) : I20-I25
# Overall stroke 
#    ICD-9 codes (1981-2000) : 430-438
#    ICD-10 codes (2001-2016) : I60-I69
# Ischaemic stroke 
#    ICD-10 codes (2001-2016) : I63, I65-I67 (except I67.4)
# Haemorrhagic stroke 
#    ICD-10 codes (2001-2016) : I60-I62, I69.0-I69.2, I67.4
# Other (not specified) stroke (class as Ischaemic stroke)
#    ICD-10 codes (2001-2016) : I64, I69.4, I69.8


#name of the database created
#cvd.longnew


################################################################################
####          2. IMPORT THE POPULATION COMPOSITION DATABASE                 ####
################################################################################
##### Population by Country, age, sex ###

##import the data containing the info on your population composition 
#As we didnt have the projected population by IMD, we used the population 
#composition by nations (England & Wales), Sex, Age and IMD
#Office for National Statistics (ONS) 2021 (accessed 15 Jun 2022).
#Populations by Index of Multiple Deprivation (IMD) decile, England and Wales, 2020
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020 

#name of the database created
#pobtotal_SES

#based on pobtotal, calculating the 2020 population composition (will be used later to standardize mortality rates)
#pob2020_red

################################################################################
####           3. IMPORT THE POPULATION PROJECTION DATABASE                 ####
################################################################################
#Here I have  population projection in England, by Sex & Age
# We also wanted the info by socioeconomic status (IMD), but info not available
#Office for National Statistics (ONS)
#Principal projection - England summary. 2022 (accessed 15 Jun 2022).
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea14principalprojectionenglandsummary 

#name of the database created
#pob.projection

#As the ONS (Office for National Statistics) does not provide population estimates and projections by IMD 
#we assumed that the relative differences in population estimates across IMD quintiles by age and sex group between 2022 and 2042
#were equal to the relative differences in 2020

############## Load database census tracts quintiles & populations ###
##creating a population projection dabase using the population composition database
#pob.projection_SES

#creating a file containing the % for each groups inside the all population (named porc.pop and porc.pop.withoutcountry for me)
#fififi 


################################################################################
####            4. MERGE MORTALITY AND POPULATION DATABASE                  ####
################################################################################
#Importing previous created databases
pobtotal_SES <- cargar.archivo(pobtotal_SES)       #Population data
cvd.longnew <- cargar.archivo(cvd.longnew)         #Mortality data

# Merge deaths and population
#saving
guardar.archivo(total)


################################################################################
####              4bis. MORTALITY PROJECTION DATABASE                       ####
################################################################################
total <- data.table(cargar.archivo(total)) 
strata <- c("Year", "Sexnum", "AgeGroups2", "IMD_quintile")

hor <- 40L # maximum simulation horizon
rate <- vector("list", 0)
pop <- vector("list", 0)

for (k in unique(total$Sexnum)) {
  for (l in unique(total$IMD_quintile)) {
    # Decompose mortality
    x1 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "Mx_all")
    x1[, AgeGroups2 := NULL]
    
    x2 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "pop")
    x2[, AgeGroups2 := NULL]
    
    nam <- paste0("E_", k, "_", l, "_allcause")
    rate[[nam]] <- as.matrix(x1)
    pop[[nam]] <- as.matrix(x2)
  }
}

# demog data doesn't work on lists of matrices
xx <- demogdata(
  rate[[1]],
  pop[[1]],
  c(seq(22,82, 15)), #list of the mean of our age groups, (15+29)/2, (30+44)/2 etc. 
  sort(unique(total$Year)),
  "mortality",
  paste0("E"),
  names(rate[1]),
  lambda = 0
)

# work around of above limitation
xx$rate <- rate
xx$pop  <- pop
xx$name <- names(rate)

xx <-
  smooth.demogdata(xx, age.grid = 15:99, obs.var = "theoretical") #15: lowest age/79: approx the highest

mort.fit <-
  coherentfdm(xx,
              10,
              10,
              method = "rapca",
              weight = T,
              # beta = 0.2,
              max.age = 99) # weight is the most important argument

mortf   <- forecast(mort.fit, h = hor, max.d = 1, level = 99)
mortf60 <- forecast(mort.fit, h = hor, max.d = 1, level = 60)
mortf70 <- forecast(mort.fit, h = hor, max.d = 1, level = 70)
mortf80 <- forecast(mort.fit, h = hor, max.d = 1, level = 80)
mortf90 <- forecast(mort.fit, h = hor, max.d = 1, level = 90)

# produce lui & uui
mortf.1 <- mortf.99 <- mortf
mortf.40 <- mortf.60 <- mortf60
mortf.30 <- mortf.70 <- mortf70
mortf.20 <- mortf.80 <- mortf80
mortf.10 <- mortf.90 <- mortf90

output    <- vector("list", length(mortf) - 2)
output.1  <- vector("list", length(mortf) - 2)
output.99 <- vector("list", length(mortf) - 2)
output.40 <- vector("list", length(mortf) - 2)
output.60 <- vector("list", length(mortf) - 2)
output.30 <- vector("list", length(mortf) - 2)
output.70 <- vector("list", length(mortf) - 2)
output.20 <- vector("list", length(mortf) - 2)
output.80 <- vector("list", length(mortf) - 2)
output.10 <- vector("list", length(mortf) - 2)
output.90 <- vector("list", length(mortf) - 2)

strata <- c("Sexnum", "IMD_quintile", "type")
for (ii in 1:(length(mortf) - 2)) {
  mortf.1[[ii]]$rate[[1]]  <- mortf[[ii]]$rate$lower
  mortf.99[[ii]]$rate[[1]] <- mortf[[ii]]$rate$upper
  mortf.40[[ii]]$rate[[1]] <- mortf60[[ii]]$rate$lower
  mortf.60[[ii]]$rate[[1]] <- mortf60[[ii]]$rate$upper
  mortf.30[[ii]]$rate[[1]] <- mortf70[[ii]]$rate$lower
  mortf.70[[ii]]$rate[[1]] <- mortf70[[ii]]$rate$upper
  mortf.20[[ii]]$rate[[1]] <- mortf80[[ii]]$rate$lower
  mortf.80[[ii]]$rate[[1]] <- mortf80[[ii]]$rate$upper
  mortf.10[[ii]]$rate[[1]] <- mortf90[[ii]]$rate$lower
  mortf.90[[ii]]$rate[[1]] <- mortf90[[ii]]$rate$upper
  
  output[[ii]] <-
    as.data.table(mortf[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf[ii]))]
  output[[ii]][, (strata) :=
                 tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.1[[ii]] <-
    as.data.table(mortf.1[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.1[ii]))]
  output.1[[ii]][, (strata) :=
                   tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.99[[ii]] <-
    as.data.table(mortf.99[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.99[ii]))]
  output.99[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.40[[ii]] <-
    as.data.table(mortf.40[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.40[ii]))]
  output.40[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.60[[ii]] <-
    as.data.table(mortf.60[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.60[ii]))]
  output.60[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.30[[ii]] <-
    as.data.table(mortf.30[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.30[ii]))]
  output.30[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.70[[ii]] <-
    as.data.table(mortf.70[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.70[ii]))]
  output.70[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.20[[ii]] <-
    as.data.table(mortf.20[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.20[ii]))]
  output.20[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.80[[ii]] <-
    as.data.table(mortf.80[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.80[ii]))]
  output.80[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.10[[ii]] <-
    as.data.table(mortf.10[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.10[ii]))]
  output.10[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.90[[ii]] <-
    as.data.table(mortf.90[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.90[ii]))]
  output.90[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
}

output    <- rbindlist(output)
output.1  <- rbindlist(output.1)
output.99 <- rbindlist(output.99)
output.40 <- rbindlist(output.40)
output.60 <- rbindlist(output.60)
output.30 <- rbindlist(output.30)
output.70 <- rbindlist(output.70)
output.20 <- rbindlist(output.20)
output.80 <- rbindlist(output.80)
output.10 <- rbindlist(output.10)
output.90 <- rbindlist(output.90)

strata <- c("rn", "Sexnum", "IMD_quintile", "type")
output <- melt(output,
               id.vars = strata,
               value.name = "mx_total")
output.1 <-
  melt(output.1,
       id.vars = strata,
       value.name = "mx_total_1")
output.99 <-
  melt(output.99,
       id.vars = strata,
       value.name = "mx_total_99")
output.10 <-
  melt(output.10,
       id.vars = strata,
       value.name = "mx_total_10")
output.20 <-
  melt(output.20,
       id.vars = strata,
       value.name = "mx_total_20")
output.30 <-
  melt(output.30,
       id.vars = strata,
       value.name = "mx_total_30")
output.40 <-
  melt(output.40,
       id.vars = strata,
       value.name = "mx_total_40")
output.60 <-
  melt(output.60,
       id.vars = strata,
       value.name = "mx_total_60")
output.70 <-
  melt(output.70,
       id.vars = strata,
       value.name = "mx_total_70")
output.80 <-
  melt(output.80,
       id.vars = strata,
       value.name = "mx_total_80")
output.90 <-
  melt(output.90,
       id.vars = strata,
       value.name = "mx_total_90")

strata <- c("rn", "Sexnum", "IMD_quintile", "type", "variable")
output[output.1, mx_total_1 := i.mx_total_1, on = strata]
output[output.99, mx_total_99 := i.mx_total_99, on = strata]
output[output.10, mx_total_10 := i.mx_total_10, on = strata]
output[output.20, mx_total_20 := i.mx_total_20, on = strata]
output[output.30, mx_total_30 := i.mx_total_30, on = strata]
output[output.40, mx_total_40 := i.mx_total_40, on = strata]
output[output.60, mx_total_60 := i.mx_total_60, on = strata]
output[output.70, mx_total_70 := i.mx_total_70, on = strata]
output[output.80, mx_total_80 := i.mx_total_80, on = strata]
output[output.90, mx_total_90 := i.mx_total_90, on = strata]

test <- copy(xx$rate)
original <- vector("list", length(test))

for (ii in 1:(length(test))) {
  original[[ii]] <-
    as.data.table(test[[ii]], keep.rownames = T)[, `:=`(type = names(test[ii]))]
  original[[ii]][, c("Sexnum", "IMD_quintile", "type") := tstrsplit(type, "_", fixed = TRUE, keep = 2:4)]
}
original <- rbindlist(original)
original <-
  melt(original,
       id.vars = c("rn", "Sexnum", "IMD_quintile", "type"),
       value.name = "mx_total")
original[, paste0("mx_total_",
                  c(1, 99, 10, 20, 30, 40, 60, 70, 80, 90)) :=
           mx_total]

lifetable_all <-
  rbind(original, output, use.names = TRUE, fill = TRUE)
lifetable_all[, age := as.integer(rn)]
lifetable_all[, year := as.integer(as.character(variable))]


#test coherence graph
# lifetable_all[, quan := runif(1)]
# lifetable_all[year >= 2030, quan := runif(1)]
#  ggplot(lifetable_all[
#    (age %% 10) == 5 & between(age, 20, 79) & year > 1000, ],
#    aes(
#      y = mx_total,
#      ymin = mx_total_1,
#      ymax = mx_total_99,
#      x = year
#    )) +
#    geom_pointrange() +
#    facet_grid(age~Sexnum + IMD_quintile, scales = "free")
# 



# cause specific mortality: CHD and stroke
rate <- vector("list", 0)
pop <- vector("list", 0)

for (k in unique(total$Sexnum)) {
  for (l in unique(total$IMD_quintile)) {
    # Decompose mortality
    x1 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "Mx_nonmodelled")
    x1[, AgeGroups2 := NULL]
    for (j in 1:ncol(x1)) set(x1, which(x1[[j]] == 0), j, 1e-8)
    
    x2 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "pop")
    x2[, AgeGroups2 := NULL]
    
    nam <- paste0("E_", k, "_", l, "_nonmodelled")
    rate[[nam]] <- as.matrix(x1)
    pop[[nam]] <- as.matrix(x2)
    
    x1 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "Mx_chd")
    x1[, AgeGroups2 := NULL]
    for (j in 1:ncol(x1)) set(x1, which(x1[[j]] == 0), j, 1e-8)
    
    nam <- paste0("E_", k, "_", l, "_chd")
    rate[[nam]] <- as.matrix(x1)
    pop[[nam]] <- as.matrix(x2)
    
    x1 <- dcast(total[Sexnum == k & IMD_quintile == l, ],
                AgeGroups2 ~ Year, value.var = "Mx_stroke")
    x1[, AgeGroups2 := NULL]
    for (j in 1:ncol(x1)) set(x1, which(x1[[j]] == 0), j, 1e-8)
    
    nam <- paste0("E_", k, "_", l, "_stroke")
    rate[[nam]] <- as.matrix(x1)
    pop[[nam]] <- as.matrix(x2)
  }
}

# demog data doesn't work on lists of matrices
xx <- demogdata(
  rate[[1]],
  pop[[1]],
  c(seq(22,82, 15)), #list of the mean of our age groups
  sort(unique(total$Year)),
  "mortality",
  paste0("E"),
  names(rate[1]),
  lambda = 0
)

# work around of above limitation
xx$rate <- rate
xx$pop  <- pop
xx$name <- names(rate)

xx <-
  smooth.demogdata(xx, age.grid = 15:99, obs.var = "theoretical")

mort.fit <-
  coherentfdm(
    xx,
    10,
    10,
    method = "rapca",
    weight = T,
    beta = 0.2,
    max.age = 99
  ) # weight is the most important arguement
mortf   <- forecast(mort.fit, h = hor, max.d = 1, level = 99)
mortf60 <- forecast(mort.fit, h = hor, max.d = 1, level = 60)
mortf70 <- forecast(mort.fit, h = hor, max.d = 1, level = 70)
mortf80 <- forecast(mort.fit, h = hor, max.d = 1, level = 80)
mortf90 <- forecast(mort.fit, h = hor, max.d = 1, level = 90)

# produce lui & uui
mortf.1  <- mortf.99 <- mortf
mortf.40 <- mortf.60 <- mortf60
mortf.30 <- mortf.70 <- mortf70
mortf.20 <- mortf.80 <- mortf80
mortf.10 <- mortf.90 <- mortf90

output    <- vector("list", length(mortf) - 2)
output.1  <- vector("list", length(mortf) - 2)
output.99 <- vector("list", length(mortf) - 2)
output.40 <- vector("list", length(mortf) - 2)
output.60 <- vector("list", length(mortf) - 2)
output.30 <- vector("list", length(mortf) - 2)
output.70 <- vector("list", length(mortf) - 2)
output.20 <- vector("list", length(mortf) - 2)
output.80 <- vector("list", length(mortf) - 2)
output.10 <- vector("list", length(mortf) - 2)
output.90 <- vector("list", length(mortf) - 2)

for (ii in 1:(length(mortf) - 2)) {
  mortf.1[[ii]]$rate[[1]]  <- mortf[[ii]]$rate$lower
  mortf.99[[ii]]$rate[[1]] <- mortf[[ii]]$rate$upper
  mortf.40[[ii]]$rate[[1]] <- mortf60[[ii]]$rate$lower
  mortf.60[[ii]]$rate[[1]] <- mortf60[[ii]]$rate$upper
  mortf.30[[ii]]$rate[[1]] <- mortf70[[ii]]$rate$lower
  mortf.70[[ii]]$rate[[1]] <- mortf70[[ii]]$rate$upper
  mortf.20[[ii]]$rate[[1]] <- mortf80[[ii]]$rate$lower
  mortf.80[[ii]]$rate[[1]] <- mortf80[[ii]]$rate$upper
  mortf.10[[ii]]$rate[[1]] <- mortf90[[ii]]$rate$lower
  mortf.90[[ii]]$rate[[1]] <- mortf90[[ii]]$rate$upper
  
  strata <- c("Sexnum", "IMD_quintile", "type")
  output[[ii]] <-
    as.data.table(mortf[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf[ii]))]
  output[[ii]][, (strata) :=
                 tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.1[[ii]] <-
    as.data.table(mortf.1[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.1[ii]))]
  output.1[[ii]][, (strata) :=
                   tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.99[[ii]] <-
    as.data.table(mortf.99[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.99[ii]))]
  output.99[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.40[[ii]] <-
    as.data.table(mortf.40[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.40[ii]))]
  output.40[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.60[[ii]] <-
    as.data.table(mortf.60[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.60[ii]))]
  output.60[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.30[[ii]] <-
    as.data.table(mortf.30[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.30[ii]))]
  output.30[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.70[[ii]] <-
    as.data.table(mortf.70[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.70[ii]))]
  output.70[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.20[[ii]] <-
    as.data.table(mortf.20[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.20[ii]))]
  output.20[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.80[[ii]] <-
    as.data.table(mortf.80[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.80[ii]))]
  output.80[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.10[[ii]] <-
    as.data.table(mortf.10[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.10[ii]))]
  output.10[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
  output.90[[ii]] <-
    as.data.table(mortf.90[[ii]]$rate[[1]],
                  keep.rownames = T)[, `:=`(type = names(mortf.90[ii]))]
  output.90[[ii]][, (strata) :=
                    tstrsplit(type, "_", fixed = TRUE, keep = 2:(length(strata) +1L))]
}

output    <- rbindlist(output)
output.1  <- rbindlist(output.1)
output.99 <- rbindlist(output.99)
output.40 <- rbindlist(output.40)
output.60 <- rbindlist(output.60)
output.30 <- rbindlist(output.30)
output.70 <- rbindlist(output.70)
output.20 <- rbindlist(output.20)
output.80 <- rbindlist(output.80)
output.10 <- rbindlist(output.10)
output.90 <- rbindlist(output.90)

strata <- c("rn", "Sexnum", "IMD_quintile", "type")
output <- melt(output,
               id.vars = strata,
               value.name = "mx")
output.1 <-
  melt(output.1,
       id.vars = strata,
       value.name = "mx_1")
output.99 <-
  melt(output.99,
       id.vars = strata,
       value.name = "mx_99")
output.10 <-
  melt(output.10,
       id.vars = strata,
       value.name = "mx_10")
output.20 <-
  melt(output.20,
       id.vars = strata,
       value.name = "mx_20")
output.30 <-
  melt(output.30,
       id.vars = strata,
       value.name = "mx_30")
output.40 <-
  melt(output.40,
       id.vars = strata,
       value.name = "mx_40")
output.60 <-
  melt(output.60,
       id.vars = strata,
       value.name = "mx_60")
output.70 <-
  melt(output.70,
       id.vars = strata,
       value.name = "mx_70")
output.80 <-
  melt(output.80,
       id.vars = strata,
       value.name = "mx_80")
output.90 <-
  melt(output.90,
       id.vars = strata,
       value.name = "mx_90")

strata <- c("rn", "Sexnum", "IMD_quintile", "type", "variable")
output[output.1, mx_1 := i.mx_1, on = strata]
output[output.99, mx_99 := i.mx_99, on = strata]
output[output.10, mx_10 := i.mx_10, on = strata]
output[output.20, mx_20 := i.mx_20, on = strata]
output[output.30, mx_30 := i.mx_30, on = strata]
output[output.40, mx_40 := i.mx_40, on = strata]
output[output.60, mx_60 := i.mx_60, on = strata]
output[output.70, mx_70 := i.mx_70, on = strata]
output[output.80, mx_80 := i.mx_80, on = strata]
output[output.90, mx_90 := i.mx_90, on = strata]

test <- copy(xx$rate)
original <- vector("list", length(test))

for (ii in 1:(length(test))) {
  original[[ii]] <-
    as.data.table(test[[ii]], keep.rownames = T)[, `:=`(type = names(test[ii]))]
  original[[ii]][, c("Sexnum", "IMD_quintile", "type") := tstrsplit(type, "_", fixed = TRUE, keep = 2:4)]
}
original <- rbindlist(original)
original <-
  melt(
    original,
    id.vars = c("rn", "Sexnum", "IMD_quintile", "type"),
    value.name = "mx"
  )
original[, paste0("mx_", c(1, 99, 10, 20, 30, 40, 60, 70, 80, 90)) := mx]

tt <- rbind(original, output, use.names = TRUE, fill = TRUE)
tt[, age := as.integer(rn)]
tt[, year := as.integer(as.character(variable))]

lifetable_all[tt[type == "chd", ], `:=` (
  mx_chd    = mx,
  mx_chd_1  = mx_1,
  mx_chd_99 = mx_99,
  mx_chd_10 = mx_10,
  mx_chd_20 = mx_20,
  mx_chd_30 = mx_30,
  mx_chd_40 = mx_40,
  mx_chd_60 = mx_60,
  mx_chd_70 = mx_70,
  mx_chd_80 = mx_80,
  mx_chd_90 = mx_90
),
on = c("age", "Sexnum", "IMD_quintile", "year")]

lifetable_all[tt[type == "stroke", ], `:=` (
  mx_stroke    = mx,
  mx_stroke_1  = mx_1,
  mx_stroke_99 = mx_99,
  mx_stroke_10 = mx_10,
  mx_stroke_20 = mx_20,
  mx_stroke_30 = mx_30,
  mx_stroke_40 = mx_40,
  mx_stroke_60 = mx_60,
  mx_stroke_70 = mx_70,
  mx_stroke_80 = mx_80,
  mx_stroke_90 = mx_90
),
on = c("age", "Sexnum", "IMD_quintile", "year")]

lifetable_all[, `:=` (
  Age=as.integer(rn),
  Year=as.integer(as.character(variable)),
  rn = NULL,
  type = NULL,
  variable = NULL
)]


setcolorder(lifetable_all,
            c("Year", "Age", "Sexnum", "IMD_quintile",
              "mx_total",
              paste0("mx_total_", c(1, 99, 10, 90, 20, 80, 30, 70, 40, 60)),
              "mx_chd",
              paste0("mx_chd_", c(1, 99, 10, 90, 20, 80, 30, 70, 40, 60)),
              "mx_stroke",
              paste0("mx_stroke_", c(1, 99, 10, 90, 20, 80, 30, 70, 40, 60))
            ))


setkey(lifetable_all, Year, Age, Sexnum, IMD_quintile)

lifetable_all <- lifetable_all %>% mutate(Country="England")

#mortality_projections_CVD_15_95_2001_2056
guardar.archivo(lifetable_all)

rm(rate, pop, hor,xx,x1,x2, output,output.1,output.10,output.20,output.30,output.40,output.60,output.70,output.80,output.90,output.99,
   ii,mortf, mortf.1,mortf.10,mortf.20,mortf.30,mortf.40,mortf.60,mortf.70,mortf.80,mortf.90,mortf.99,mortf60,mortf70,mortf80,mortf90,
   mort.fit, k, l, tt)


#I need to have the mx by agegroup and not by age, so I used the mean by agegroup
lifetable_all <- cargar.archivo(lifetable_all)
lifetable_all2 <- lifetable_all %>% filter(age<88) %>% #88 is coming from different test to find the threshold to be consistent with observed death in 75+ group
  mutate(Sex=ifelse(Sexnum=="2","Women","Men"),
         AgeGroups2= ifelse(age<30, "15-29",
                            ifelse(age<45, "30-44",
                                   ifelse(age<60, "45-59",
                                          ifelse(age<75, "60-74", "75 & over")))),
         fusion=paste(Country, Sex, AgeGroups2, IMD_quintile,Year,sep=" "))
lifetable_all3 <- lifetable_all2 %>% group_by(fusion) %>% summarize(across(mx_total:mx_stroke_60, mean, na.rm=TRUE)) 
lifetable_allred <- lifetable_all2 %>% filter(!duplicated(fusion)) %>% dplyr::select(c(Sex, AgeGroups2, IMD_quintile,Year, fusion))
lifetable_all4 <- lifetable_all3 %>% merge(lifetable_allred, by="fusion")

# Assign % of strokes that are ischemic or haemorrhagic 
porc.isch <- cargar.archivo(porc.isch) %>% ungroup() %>% dplyr::select(-c(Country))
lifetable_all_meangroup1 <- lifetable_all4 %>% left_join(porc.isch, by=c("Sex","AgeGroups2")) %>%
  mutate(mx_isch=mx_stroke*porc.isch,
         mx_isch_1=mx_stroke_1*porc.isch,
         mx_isch_99=mx_stroke_99*porc.isch,
         
         mx_haem=mx_stroke-mx_isch,
         mx_haem_1=mx_stroke_1-mx_isch_1,
         mx_haem_99=mx_stroke_99-mx_isch_99) %>% dplyr::select(-porc.isch)

#we add the pop info
#from 2001 to 2020
pobtotal_SES2 <- cargar.archivo(pobtotal_SES) %>% mutate(Coutry=ifelse(str_detect(Groups,"England"),"England","NA"),
                                                         Sex=ifelse(str_detect(Groups,"Women"),"Women","Men"),
                                                         Groupall=paste(Groups,IMD_quintile, sep=" ")) %>%
  filter(!str_detect(Groups,"15-29")) %>%
  dplyr::select(Year,Groupall,pop)
#from 2021 to 2042
pob.projection_SES2 <- cargar.archivo(pob.projection_SES)  %>% filter(Country=="England" & AgeGroups!="15-29") %>%
  mutate(Groupall=paste(Country, Sex, AgeGroups, IMD_quintile, sep=" ")) %>% 
  dplyr::select(Groupall,"2021":"2042") %>%
  gather(key=Year, value=pop, "2021":"2042") %>% filter(!(str_detect(Groupall,"15-29"))) 
totpop <- rbind(pobtotal_SES2,pob.projection_SES2) %>% mutate(fusion=paste(Groupall,Year,sep=" ")) %>%
  dplyr::select(fusion,pop)

lifetable_all_meangroup <- lifetable_all_meangroup1 %>% merge(totpop, by="fusion") 
#sum(is.na(lifetable_all_meangroup$pop))

guardar.archivo(lifetable_all_meangroup)


########################################################################################
#Checking our results coherence 
########################################################################################


########################################################################################
#test to see if quartiles death trend ok
#Q1 have higher mortality than Q5
#ok
########################################################################################



################################################################################
####                 5. IMPORT THE DIETARY INTAKE DATABASE                  ####
################################################################################

## Import the data containing the dietary intake info you need for your model
#we are using the NDNS data 
#University of Cambridge, NatCen Social Research. National Diet and Nutrition Survey. UK Data Service 2019. 


### Here, create a dataset with the baseline out-of-home consumption, weighted, according to categories (Country, Age, Sex & IMD)

#Check for aberrant intake data 

#create a database named
#dietary_intakes


#calculating the means for each interested intake data (e.g., energy) for each group (Country, Age, Sex & IMD)
#creating a database named
#Var_Mean_red


#Trend in intakes across NDNS, from 2008-2009 to 2018-2019
#Based on the trend, estimation of the 2022 Baseline intakes
Var_Mean_red <- cargar.archivo(Var_Mean_red)
dietary_intakes <- cargar.archivo(dietary_intakes)
#declaration of surveydesign
dw <- svydesign(ids = ~ids, weights = ~ weight, strata=~strata, data = dietary_intakes)


#List of food you want to study
listfood <- c(paste(intakevarout, "_Mean", sep=""))
#Groups needed
z <- subset(Var_Mean_red, !duplicated(Groups))
liste <- c(z$Groups)
rm(z)

#Estimation of the future intakes based on the previous trend
gr <- expand.grid(Country=c("England"), 
                  Sex=c("Men", "Women"), 
                  AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"), 
                  SES=1:5, 
                  Year2=2009:2042)
gr$Groups2 <- paste(gr$Country, gr$Sex, gr$AgeGroups2, gr$SES, gr$Year2) 
gr$Groups <- paste(gr$Country, gr$Sex, gr$AgeGroups2, gr$SES) 
trend_all <- subset(gr, select=c(Year2, Groups, Groups2))
all.trend <- subset(gr, select=c(Groups), !duplicated(Groups))
apc.all <- subset(gr, select=c(Groups), !duplicated(Groups))
byYear <- data.frame()
for (f in listfood) {
  trend_j <- data.frame()
  j.trend <- data.frame()
  apc.j <- data.frame()
  for (i in liste) {
    a <- Var_Mean_red %>% filter(Groups==i) %>%  ungroup() %>% dplyr::select(Year, Groups, Mean=paste(f))  
    # dwa <- svydesign(ids = ~ids, weights = ~ weight, strata=~strata, data = a)
    
    #If we have only one point, we attribute this only point to all estimations
    #before but not working no more: if (sum(!is.na(a$Mean))<=2 | summary(lm.a)$coef[2,4]>0.05) {
    if (sum(!is.na(a$Mean))<=2) {
      a <- rbind(a, data.frame(Year=2020:2042, Groups=i, Mean=NA)) 
      s <- a %>% mutate(aa=ifelse(is.na(Mean)==TRUE,1,0)) %>% filter(aa==0) %>% ungroup()  
      maxy <- s %>% summarise_if(is.numeric, max) %>% dplyr::select(Year)
      c <- a %>% filter(Year==maxy[1,1]) #Mean of this variable in 2019 or more recent year
      
      # a$pred <- sum(a$Mean,na.rm=T)
      a$pred <- as.numeric(sum(c[3]))
      a.trend <- list(mean = 0, se = 0, Groups=i) 
      j.trend <- rbind(j.trend, a.trend) 
      
      apc.a <- c((exp(a.trend[[1]])-1)*100,(exp(a.trend$mean-1.96*a.trend$se)-1)*100,(exp(a.trend$mean+1.96*a.trend$se)-1)*100, i)
      apc.j <- rbind(apc.j, apc.a)  
      rm(c,a.trend, apc.a)
    } else { 
      lm.a <- lm(log(Mean) ~ Year, data=a, na.action=na.omit)
      #WARNING CARLOS : svyglm not working ("Error in svyglm(formula=log(Mean) ~ Year, data=a, design=dw, na.action=na.omit) : all variables must be in design= argument)
      #I tried a lot of thing, but none worked... do you have an idea?
      #lm.svy.a <- svyglm(formula=log(Mean) ~ Year, data=a, design=dwa, na.action=na.omit) 
      
      #Or if the trend is not significant
      if (summary(lm.a)$coef[2,4]>0.05) {
        a <- rbind(a, data.frame(Year=2020:2042, Groups=i, Mean=NA)) 
        s <- a %>% mutate(aa=ifelse(is.na(Mean)==TRUE,1,0)) %>% filter(aa==0) %>% ungroup()  
        maxy <- s %>% summarise_if(is.numeric, max) %>% dplyr::select(Year)
        c <- a %>% filter(Year==maxy[1,1]) #Mean of this variable in 2019 or more recent year
        
        # a$pred <- sum(a$Mean,na.rm=T)
        a$pred <- as.numeric(sum(c[3]))
        a.trend <- list(mean = 0, se = 0, Groups=i) 
        j.trend <- rbind(j.trend, a.trend) 
        
        apc.a <- c((exp(a.trend[[1]])-1)*100,(exp(a.trend$mean-1.96*a.trend$se)-1)*100,(exp(a.trend$mean+1.96*a.trend$se)-1)*100, i)
        apc.j <- rbind(apc.j, apc.a)  
        rm(c,a.trend, apc.a)
      } else { 
        #we have some 0 and lm does not work with 0: sugar, fruits and vegetables
        #For now, I have a constant (+1) to avoid 0 
        #  For more info read : Bellégo, Benatia & Pape 2021 - Dealing with Logs and Zeros in Regression Models 
        
        if (min(a$Mean,na.rm=T)==0) {
          a$Mean <- ifelse(is.na(a$Mean), NA, a$Mean+1)
        } else {
          a$Mean <- as.numeric(a$Mean)
        }
        
        a <- rbind(a, data.frame(Year=2020:2042, Groups=i, Mean=NA)) 
        xx1 <- as.data.frame(exp(predict(lm.a,a, interval = "confidence", level=0.95)))
        a <- cbind(a, data.frame(xx1)) %>% mutate(pred=fit) %>% dplyr::select(-c(fit,lwr,upr))
        rm(xx1)
        
        #a$pred.svy <- exp(predict(lm.svy.a,a, interval = "confidence", level=0.95))
        #do if necessary
        #assign(paste("trend.", i, sep=""), a)
        
        #I have done some test, and adding a constant just change the a$pred (and only very slightly the a.trend and the apc.a, as expected)
        if (min(a$Mean,na.rm=T)==0) {
          a$pred <- a$pred-1
          a$Mean <- ifelse(is.na(a$Mean), NA, a$Mean-1)
        } else {
          a$pred <- a$pred
        }
        a.trend <- list(mean = lm.a[[c(1,2)]], se = summary(lm.a)$coef[2,2], Groups=i) 
        j.trend <- rbind(j.trend, a.trend) 
        
        apc.a <- c((exp(a.trend[[1]])-1)*100,(exp(a.trend$mean-1.96*a.trend$se)-1)*100,(exp(a.trend$mean+1.96*a.trend$se)-1)*100, i)
        apc.j <- rbind(apc.j, apc.a)  
        # print(paste0(i," /  ", f, " /1"))
        rm(lm.a, a.trend, apc.a)
      }
    }
    trend_j <- rbind(trend_j,a)
    rm(a)
  }
  
  xx <- subset(trend_j, select=c(Groups, Year, pred)) 
  xx <- dcast(xx, Groups ~ Year, value.var = "pred")
  xx$name <- f 
  byYear <- rbind(byYear, xx)
  #assign(paste("byYear_", f, sep=""), xx)
  
  setnames(trend_j, old = "Mean" , new = f)
  setnames(trend_j, old = "pred" , new = paste(f, "_pred", sep=""))
  trend_j$Groups2 <- paste(trend_j$Groups, trend_j$Year)
  trend_j <- subset(trend_j, select=-c(Year, Groups))
  
  setnames(j.trend, old = "mean" , new = paste("mean_", f, sep=""))
  setnames(j.trend, old = "se" , new = paste("se_", f, sep=""))
  
  setnames(apc.j, old = , new = c("Col1", "Col2", "Col3", "Groups"))
  setnames(apc.j, old = "Col1", new = paste("Col1_", f, sep=""))
  setnames(apc.j, old = "Col2", new = paste("Col2_", f, sep=""))
  setnames(apc.j, old = "Col3", new = paste("Col3_", f, sep=""))
  
  trend_all <- merge(trend_all,trend_j, by="Groups2")
  all.trend <- merge(all.trend,j.trend, by="Groups")
  apc.all <- merge(apc.all,apc.j, by="Groups")
  # print(paste0(f))
  rm(trend_j, j.trend, apc.j, xx)
}
rm(f,i)
#Maybe you have this warning message : In qt((1 - level)/2, df) : Production de NaN
#The problem comes from the content of the data : the software's can't naturally handle imaginary numbers.


#Check no NA

# saving database
guardar.archivo(all.trendok)




################################################################################
####                    6. IMPORT THE POLICY EFFECT                         ####
################################################################################
#No difference find according to the Country, so same betas across all Country
#No difference find according to the Sex, so same betas across all Sex
#No difference find according to the Age, so same betas across all Age
#No difference find according to the SES, so same betas across all Age 

#Based on the Crockett 2018 - Nutritional labelling for healthier food or nonalcoholic drink purchasing and consumption
#Be aware that here, it's not a beta for now : it's -47 kcal purchased
rr.policy <- list(estimate=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                      menulabelling_kcal=c(-47,-47,-47,-47,-47)), 
                  lower=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                   menulabelling_kcal=c(-15,-15,-15,-15,-15)),
                  upper=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                   menulabelling_kcal=c(-79,-79,-79,-79,-79)))


################################################################################
####                 7. IMPORT THE RR NEEDED FOR THE MODEL                  ####
################################################################################

###################################################################################
#    Import the RR for the model
###################################################################################
# Mortality RR for fruits and vegetables (Micha et al., JAMA 2017, supplement, eTable 5, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5852674/) ###
# With AgeGroup different from Micha, we make mean of the RR for each group
rr.fruit <- list(estimate=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                     chd=c(0.92,0.92,0.93,0.95,0.97),
                                     isch=c(0.83,0.83,0.87,0.89,0.94),
                                     haem=c(0.63,0.64,0.69,0.75,0.86)),
                 lower=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                  chd=c(0.87,0.87,0.90,0.92,0.96),
                                  isch=c(0.76,0.77,0.81,0.85,0.92),
                                  haem=c(0.49,0.50,0.58,0.65,0.80)),
                 upper=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                  chd=c(0.97,0.97,0.97,0.98,0.99),
                                  isch=c(0.90,0.90,0.92,0.94,0.96),
                                  haem=c(0.81,0.82,0.85,0.88,0.92)))

#Artilce ERFC 2011 - Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease: collaborative analysis of 58 prospective studies
# https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(11)60105-0/fulltext#secd19223949e1020
# Dans le doc https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/6d6bb978-0d17-4017-9845-2903d3b68778/gr2.gif
# In people with BMI of 20 kg/m² or higher: HRs per 1 SD higher baseline values : 4·56 kg/m² higher BMI
# CHD
# 40-59 years : 1.41 (1.30-1.53)
# 60-69 years : 1.23 (1.15-1.31)
# 70+ years   : 1.12 (1.05-1.19)
# 
# Ischaemic stroke
# 40-59 years : 1.34 (1.21-1.48)
# 60-69 years : 1.22 (1.13-1.31)
# 70+ years   : 1.08 (0.99-1.18)

rr.obesity <- list(estimate=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                       chd=c(1,1.41,1.41,1.23,1.12),
                                       isch=c(1,1.34,1.34,1.22,1.08),
                                       haem=c(1,1,1,1,1)),
                   lower=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                    chd=c(1,1.30,1.30,1.15,1.05),
                                    isch=c(1,1.21,1.21,1.13,0.99),
                                    haem=c(1,1,1,1,1)),
                   upper=data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                    chd=c(1,1.53,1.53,1.31,1.19), 
                                    isch=c(1,1.48,1.48,1.31,1.18),
                                    haem=c(1,1,1,1,1)))


# # Extract SE from IC
rr.fruit[["se"]]<- data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                              chd= ((log(rr.fruit[["upper"]]$chd) - log(rr.fruit[["estimate"]]$chd))/1.96 +
                                      (log(rr.fruit[["lower"]]$chd) - log(rr.fruit[["estimate"]]$chd))/-1.96)/2,
                              isch= ((log(rr.fruit[["upper"]]$isch) - log(rr.fruit[["estimate"]]$isch))/1.96 +
                                       (log(rr.fruit[["lower"]]$isch) - log(rr.fruit[["estimate"]]$isch))/-1.96)/2,
                              haem= ((log(rr.fruit[["upper"]]$haem) - log(rr.fruit[["estimate"]]$haem))/1.96 +
                                       (log(rr.fruit[["lower"]]$haem) - log(rr.fruit[["estimate"]]$haem))/-1.96)/2
)   
rr.obesity[["se"]]<- data.frame(AgeGroups2=c("15-29","30-44","45-59","60-74","75 & over"),
                                chd= ((log(rr.obesity[["upper"]]$chd) - log(rr.obesity[["estimate"]]$chd))/1.96 +
                                        (log(rr.obesity[["lower"]]$chd) - log(rr.obesity[["estimate"]]$chd))/-1.96)/2,
                                isch= ((log(rr.obesity[["upper"]]$isch) - log(rr.obesity[["estimate"]]$isch))/1.96 +
                                         (log(rr.obesity[["lower"]]$isch) - log(rr.obesity[["estimate"]]$isch))/-1.96)/2,
                                haem= ((log(rr.obesity[["upper"]]$haem) - log(rr.obesity[["estimate"]]$haem))/1.96 +
                                         (log(rr.obesity[["lower"]]$haem) - log(rr.obesity[["estimate"]]$haem))/-1.96)/2
)   



rr.food_morta <- bind_rows(rr.fruit, .id="stat") %>% mutate(food = "Fruits") %>%
  bind_rows(bind_rows(rr.obesity, .id="stat") %>% mutate(food = "Obesity")) %>%
  filter(stat %in% c("estimate","se")) %>%
  gather(key=disease, value=value, chd:haem) %>%
  spread(key=stat, value=value)

guardar.archivo(rr.food_morta)

rm(rr.fruit, rr.obesity) 


################################################################################
####                           8. RUN THE MODEL                             ####
################################################################################

#Years studies: years from the baseline of the studies to the end of the projection
yearsstudies <- c(2022:2042) 
yearsstudiesplus1 <- c(2023:2042) #begin 1 year after the baseline

#Food variable we want to keep
intakevarout <- c("Energy_out_kcal", "Energy_kcal_meal", "Mealoutofhome", "Mealoutofhome_year", "totalveg_out")


## Relative Risks of mortality by SES ###
rr.ses <- data.frame(Sex=rep(c("Men","Women"),each=10),
                     IMD_quintile = factor(rep(1:5)),
                     disease = rep(c("chd","stroke")),
                     estimate = c(1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1),
                        lower = c(1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1),
                        upper = c(1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1,
                                  1, 1, 1, 1, 1)) %>%
  arrange(disease,Sex,IMD_quintile)


###Calling all the register database needed
pobtotal_SES <- cargar.archivo(pobtotal_SES) %>% filter(!str_detect(Groups,"15-29"))
pobtotal <- cargar.archivo(pobtotal) %>% filter(Country=="England" & AgeGroups!="15-19" & AgeGroups!="20-24" & AgeGroups!="25-29")
total <- cargar.archivo(total) %>% filter(AgeGroups2!="15-29")
pob.projection_SES <- cargar.archivo(pob.projection_SES) %>% filter(Country=="England" & AgeGroups!="0-14" & AgeGroups!="15-29") 
pob2020_red <- cargar.archivo(pob2020_red) %>% filter(Country=="England" & AgeGroups2!="15-29") 
Var_Mean_red <- cargar.archivo(Var_Mean_red) %>% filter(AgeGroups!="15-29")
all.trendok <- cargar.archivo(all.trendok) %>% filter(!str_detect(Groups, "15-29"))
priv.census <- cargar.archivo(priv.census) %>% filter(Country=="England" & AgeGroups2!="0-14" & AgeGroups2!="15-19" & AgeGroups2!="20-24" & AgeGroups2!="25-29") 
fififi <- cargar.archivo(fififi) %>% filter(str_detect(Groups, "England") &
  !str_detect(GroupswithoutCountry, "0-14") & !str_detect(GroupswithoutCountry, "15-29"))
porc.isch <- cargar.archivo(porc.isch) %>% filter(AgeGroups2!="15-29") %>% mutate(Sexnumber=ifelse(Sex=="Men", 1, 2))
rr.food_morta <- cargar.archivo(rr.food_morta) %>% filter(AgeGroups2!="15-29")
lifetable_all_meangroup <- cargar.archivo(lifetable_all_meangroup) %>%
        mutate(Country="England",
               mx_total_se = ((log(mx_total_99) - log(mx_total))/qnorm(0.995) + (log(mx_total_1) - log(mx_total))/qnorm(0.005))/2,
               mx_chd_se   = ((log(mx_chd_99) - log(mx_chd))/qnorm(0.995) + (log(mx_chd_1) - log(mx_chd))/qnorm(0.005))/2,
               mx_stroke_se= ((log(mx_stroke_99) - log(mx_stroke))/qnorm(0.995) + (log(mx_stroke_1) - log(mx_stroke))/qnorm(0.005))/2,
               mx_isch_se= ((log(mx_isch_99) - log(mx_isch))/qnorm(0.995) + (log(mx_isch_1) - log(mx_isch))/qnorm(0.005))/2,
               mx_haem_se= ((log(mx_haem_99) - log(mx_haem))/qnorm(0.995) + (log(mx_haem_1) - log(mx_haem))/qnorm(0.005))/2) 
#check in normal distribution: not normal, so log
# z <- lifetable_all_meangroup %>%
#          mutate(Country="EW",
#          cvd1 = log(mx_total) - log(mx_total_99),
#          cvd2 = log(mx_total) - log(mx_total_1),
#          cvd3 = cvd1 - cvd2,
#          
#          chd1 = log(mx_chd) - log(mx_chd_99),
#          chd2 = log(mx_chd) - log(mx_chd_1),
#          chd3=chd1-chd2,
#          
#          st1 = log(mx_stroke) - log(mx_stroke_99),
#          st2 = log(mx_stroke) - log(mx_stroke_1),
#          st3=st1-st2,
#          
#          isch1 = log(mx_isch) - log(mx_isch_99),
#          isch2 = log(mx_isch) - log(mx_isch_1),
#          isch3=isch1-isch2,
#          
#          haem1 = log(mx_haem) - log(mx_haem_99),
#          haem2 = log(mx_haem) - log(mx_haem_1), haem3=haem1-haem2
#          ) %>% dplyr::select(c(fusion,cvd1:isch2))

listfood <- c(paste(intakevarout, "_Mean", sep=""))
dietary_intakes <- cargar.archivo(dietary_intakes) %>% filter(AgeGroups!="15-29", AgeGroups!="80 & over", is.na(Energy_out_kcal)==F) %>% 
                   mutate(AgeGroups=ifelse(AgeGroups=="75-79","75 & over",as.character(AgeGroups)), Groups=paste(Country,Sex,AgeGroups,SES)) %>%
                   dplyr::select(-c(Countryreal,Sexnumber,EnergyFood_out_kcal:totalfruit_out,SurveyYear))



################################################################################
##                        MONTE CARLO SIMULATION                              ## 
################################################################################
#Number of iterations (for testing our model, put 1:2)
montecarlo <- 1:2
numsim <- max(montecarlo)
time.begin <- Sys.time()
ggt <- data.frame(policy.energy.effect=NA, policy.energy.effect_sensi=NA,policy.vegetable.effect=NA, 
                  policy.energy.refeffect=NA, policy.energy.refeffect_sensi=NA, policy.energy.refeffect_bis=NA, 
                  policy.energy.compeffect = NA,policy.energy.compeffect_min = NA,
                  policy.energy.compeffect_max = NA,sim=NA)
for (sim in montecarlo) {
  gc()
  time.old <- Sys.time()
  
  #Preparation before steps																					
  ## Simulation: Relative Risks of mortality by SES ###
  maaax <- nrow(rr.ses)
  rr.ses.sim <- data.frame()
  for (i in 1:maaax) {
    r1 <- rr.ses[i,]
    r <- r1 %>% transmute(Sex, IMD_quintile, disease, rr = rpert(1, x.min=rr.ses[[i,5]], x.max=rr.ses[[i,6]], x.mode=rr.ses[[i,4]]))
    rr.ses.sim <- rbind(rr.ses.sim, r)
    rm(r, r1)
  }
  rm(maaax)
  
  # Simulation: Consumption trends
  z <- subset(all.trendok, !duplicated(Groups))
  liste <- c(z$Groups)
  rm(z)
  trend.sim <- subset(all.trendok, !duplicated(Groups), select=c(Groups))
  for (f in listfood) {
    tes <- data.frame() 
    for (i in liste) {
      l <- subset(all.trendok, Groups==i, select=c(paste("mean_", f, sep=""), paste("se_", f, sep="")))
      a <- rnorm(1, mean = l[[1]], sd = l[[2]]) 
      tes <- rbind(tes,data.frame(var=a, Groups=i))
      rm(l,a)
    }
    tes$var <- as.numeric(ifelse(tes$var=="NaN", 1, tes$var))
    setnames(tes, old = "var" , new = paste(f,".sim",sep=""))
    assign(paste("trend", f, "sim", sep="."), tes)
    trend.sim <- merge(trend.sim, tes, by="Groups")
    rm(tes)
  }  
  
  # Simulation: RR for fruits, veg and obesity ###
  # log normal distribution for RR
  #Here, we generate RR coherent according to age groups
  rr.food_morta.sim2 <- setDT(rr.food_morta)[, k:=runif(1), by=c("food","disease")] 
  rr.food_morta.sim <- rr.food_morta.sim2[,.(AgeGroups2,food,disease,sim=qlnorm(k,meanlog = log(estimate), sdlog = se))]
  rr.food_morta.sim <- dcast(rr.food_morta.sim, AgeGroups2 ~ disease + food, value.var = "sim")
  rr.food_morta.sim <- rr.food_morta.sim %>% rename_with(.cols= chd_Fruits:isch_Vegetables,~ paste0("rr.",.))
  
  rm(rr.food_morta.sim2)
  
  #Importing estimation of the future health outcomes based on the previous trend
  # mx = central rate of mortality
  # we decide to use the rate of mortality for 100000
  k=runif(1)
  health.pred <- setDT(lifetable_all_meangroup)[, `:=` (cvd.rate=qlnorm(k,meanlog = log(mx_total), sdlog = mx_total_se)* 100000,
                                               chd.rate=qlnorm(k,meanlog = log(mx_chd), sdlog = mx_chd_se)* 100000,
                                               stroke.rate=qlnorm(k,meanlog = log(mx_stroke), sdlog = mx_stroke_se)* 100000,
                                               isch.rate=qlnorm(k,meanlog = log(mx_isch), sdlog = mx_isch_se)* 100000,
                                               haem.rate=qlnorm(k,meanlog = log(mx_haem), sdlog = mx_haem_se)* 100000,
                                               SES = as.integer(IMD_quintile))][,c("Sex","AgeGroups2","IMD_quintile","SES",
                                                                                   "Year","pop","Country","cvd.rate","chd.rate",
                                                                                   "stroke.rate","isch.rate","haem.rate")]
  
  # fortable <- health.pred %>% mutate(Groups=paste(Country, Sex, AgeGroups2, IMD_quintile)) %>% ungroup() %>% dplyr::select(c(Groups,Year, n.chd ,n.stroke,n.isch ,n.haem,n.cvd))
  # write.xlsx(fortable, file="C:\\Users\\zoe_c\\OneDrive - The University of Liverpool\\ZOE PROJECT 2\\Datas\\Results\\Death prediction trends.xlsx", sheetName = paste(sim), col.names = TRUE, row.names = TRUE, append = TRUE)
  # rm(fortable)
  rm(k)
  
  #estimations of the future dietary consumption based on the previous trend
  gc()
  z <- subset(Var_Mean_red, !duplicated(Groups) & Groups!="NA")
  liste <- c(z$Groups)
  rm(z)
  dataaa <- setDT(Var_Mean_red)[, .(Year,Energy_out_kcal_Mean,Energy_kcal_meal_Mean,#Energykcalwhere_Mean,
                                    Mealoutofhome_Mean,Mealoutofhome_year_Mean,totalveg_out_Mean, Country, Sex, AgeGroups, SES,
                                    Groups=paste(Country, Sex, AgeGroups, SES))]
  bc <- expand.grid(Groups=liste, Year=2009:2042)
  dataaa <- bc %>% left_join(dataaa, by=c("Groups", "Year"))
  
  infoind <- setDT(dietary_intakes)[, .(seriali,Country, Sex, AgeGroups, SES)]
  dataaaind <- setDT(dietary_intakes)[, .(seriali,Year,Energy_out_kcal,Energy_kcal_meal,Mealoutofhome,Mealoutofhome_year,totalveg_out)]
  bcind <- expand.grid(seriali=dataaaind$seriali, Year=2009:2042)
  dataaaind <- bcind %>% left_join(infoind, by=c("seriali")) %>% left_join(dataaaind, by=c("seriali","Year"))
  dataaaind$Groups <- paste(dataaaind$Country, dataaaind$Sex, dataaaind$AgeGroups, dataaaind$SES)
  bcind <- setDT(dataaaind)[,.(seriali,Year, Groups)]
  
  setDT(Var_Mean_red)
  setDT(dataaa)
  setDT(dietary_intakes)
  setDT(dataaaind)
  
  for (f in listfood) {
    subii <- data.frame()
    subiind <- data.frame()
    for (i in liste) {
      b <- subset(trend.sim, Groups==i, select=c(paste(f, "sim", sep=".")))     #Trend for this variable
      ss <- Var_Mean_red[Groups==i, .SD, .SDcols = c(f, "Year", "Groups")]
      s <- ss[!is.na(rowSums(ss[,1])),]
      maxt <- s[,max(Year)] 
      cc <- ss[Year==maxt,] #Mean of this variable in 2019 or more recent year
      yy <- as.numeric(cc[1,2])
      sub <- dataaa[Groups==i, .SD, .SDcols = c("Year", "Groups", f)]
      sub <- sub[, paste("quant", f, "pred", sep=".") := exp(log(as.numeric(cc[1,1])) + b[1,1]*(sub$Year-yy))]
      subii <- rbind(subii, sub)
      
      #for doing the change by individual and not by group
      ff <- sub("_Mean", "", f)
      ssind <- dietary_intakes[Groups==i, .SD, .SDcols = c(ff, "Year", "Groups","seriali")]
      sind <- ssind[!is.na(rowSums(ssind[,1])),]
      maxtind <- sind[, max(Year), by=seriali] 
      maxtind$Year <- maxtind$V1
      
      subind <- dataaaind[Groups==i, .SD, .SDcols = c("seriali","Year", "Groups", ff)]
      subiindxx <- data.frame()
      z <- subset(subind, !duplicated(seriali)) 
      seriali <- c(z$seriali)
      rm(z)
      
      for (e in seriali) {
        cind <- ssind[seriali==e, -c("Groups")]
        yye <- as.numeric(cind[1,2])
        subind11 <- subind[seriali==e, -c("Groups")]
        subind11 <- subind11[, paste("quant", ff, "pred", sep=".") := exp(log(as.numeric(cind[1,1])) + b[1,1]*(subind11$Year-yye))]
        subiindxx <- rbind(subiindxx, subind11)
        rm(yye,subind11,cind)
      }
      subiind <- rbind(subiind, subiindxx)
      rm(sub,subind,b,cc,seriali,subiindxx)
    }
    bc <- merge(bc, subii, by=c("Groups", "Year"))
    bcind <- merge(bcind, subiind, by=c("seriali", "Year"))
    rm(subii,yy,subiind,maxt,maxtind,sind,ssind,ss)
  }
  rm(i,f)
  
  bc2 <- bc %>% dplyr::select(Groups, Year, ends_with('Mean.pred'))
  # write.xlsx(bc2, file="C:\\Users\\zoe_c\\OneDrive - The University of Liverpool\\ZOE PROJECT 2\\Datas\\Results\\Food prediction trends.xlsx", sheetName = paste(sim), col.names = TRUE, row.names = TRUE, append = TRUE)
  bcind2 <- bcind %>% dplyr::select(seriali,Groups, Year, ends_with('.pred'))
  
  # Merge with consumption database
  health.pred$Groups <- paste(health.pred$Country, health.pred$Sex, health.pred$AgeGroups2, health.pred$SES)
  databb1 <- health.pred %>% left_join(bc2, by=c("Groups", "Year")) %>% filter(Year>2008) 
  
  # Merge with anthropo info
  anthroinfo <- Var_Mean_red[, .(htval_Mean, wtval_Mean, bmival_Mean,
                                 htvalunder20_Mean,htval_20_25_Mean,htval_25_30_Mean,htvalabove30_Mean,htval_NA_Mean,
                                 wtvalunder20_Mean,wtval_20_25_Mean,wtval_25_30_Mean,wtvalabove30_Mean,wtval_NA_Mean,
                                 bmivalunder20_Mean,bmival_20_25_Mean,bmival_25_30_Mean,bmivalabove30_Mean,bmival_NA_Mean,
                                 pct_bmivalunder20_Mean,pct_bmival_20_25_Mean,pct_bmival_25_30_Mean,
                                 pct_bmivalabove30_Mean,pct_bmival_NA_Mean,Groups, Year)]
  databb2 <- databb1 %>% left_join(anthroinfo, by=c("Groups", "Year")) %>% filter(Year>2008) 
  #Here I don't find exactly the same BMI (because of the mean) SO we decided to recalculate htval_Mean
  databb2$htval_MeanNEW_m=(sqrt(databb2$wtval_Mean/databb2$bmival_Mean))
  databb2$htvalunder20_MeanNEW_m=ifelse(databb2$pct_bmivalunder20_Mean==0, NA,sqrt(databb2$wtvalunder20_Mean/databb2$bmivalunder20_Mean))
  databb2$htval_20_25_MeanNEW_m=ifelse(databb2$pct_bmival_20_25_Mean==0, NA,sqrt(databb2$wtval_20_25_Mean/databb2$bmival_20_25_Mean))
  databb2$htval_25_30_MeanNEW_m=ifelse(databb2$pct_bmival_25_30_Mean==0, NA,sqrt(databb2$wtval_25_30_Mean/databb2$bmival_25_30_Mean))
  databb2$htvalabove30_MeanNEW_m=ifelse(databb2$pct_bmivalabove30_Mean==0, NA,sqrt(databb2$wtvalabove30_Mean/databb2$bmivalabove30_Mean))
  databb2$htval_NA_MeanNEW_m=ifelse(databb2$pct_bmival_NA_Mean==0, NA,sqrt(databb2$wtval_NA_Mean/databb2$bmival_NA_Mean))
  
  databb3 <- setDT(databb2)[, -c("htval_Mean")][, Sexnumber:=ifelse(Sex=="Men",1,2)]
  
  rm(dataaaind,databb2,databb1,bc2, dataaa, trend.sim,infoind,bc,anthroinfo,health.pred)
  
  #anthropo info by individual
  anthroinfoind11 <- setDT(dietary_intakes)[, .(seriali, Groups, Sex, AgeGroups, Country, SES,
                                   Sexnumber=ifelse(Sex=="Men",1,2), AgeGroups2=AgeGroups, IMD_quintile=SES)]
  anthroinfoind22 <- setDT(dietary_intakes)[,.(seriali, Year, htval, wtval, bmival)]
  tete <- expand.grid(seriali=anthroinfoind22$seriali, Year=2009:2042)
  anthroinfoind <- tete %>% left_join(anthroinfoind11, by=c("seriali")) %>% left_join(anthroinfoind22, by=c("seriali","Year"))
  rm(tete,anthroinfoind11,anthroinfoind22)
  
  #Estimation of the future health outcomes based on the previous trend
  #We started to look at change in BMI, and none where significant (except for group "EW Women 60-74 1" but the predition where odd
  #So we decided to just put the more recent as an estimation
  gc()
  ageg <- c("30-44","45-59","60-74","75 & over")
  coun <- c(dietary_intakes$Country[1])
  baselineanthro <- data.frame()
  # Negative binomial regression (BMI) ###
  subdata <- list()
  subdata22 <- list()
  i <- 1
  for (x in 1:2){
    if (x==1) {
      s <- 1
    }
    if (x==2) {
      s <- 2
    }
    for (a in ageg) {
      for (c in coun) {
        for (q in 1:5) {
          # Sub-dataframe of BMI data by Sex, age and quintile 
          temp <- databb3  %>% 
            filter(Sexnumber==s, AgeGroups2==a, Country==c, IMD_quintile==q)
          
          #more recent data
          setDT(temp) 
          recent1 <- temp[!is.na(rowSums(temp[,"bmival_Mean"])),] #keeping when BMI not NA
          maxr <- recent1[, max(Year)] 
          recent <- recent1[Year==maxr,.SD,.SDcols = c("htval_MeanNEW_m","wtval_Mean","bmival_Mean",#bmival_Mean of the most recent year
                                                       "htvalunder20_MeanNEW_m","htval_20_25_MeanNEW_m","htval_25_30_MeanNEW_m","htvalabove30_MeanNEW_m","htval_NA_MeanNEW_m",
                                                       "wtvalunder20_Mean","wtval_20_25_Mean","wtval_25_30_Mean","wtvalabove30_Mean","wtval_NA_Mean",
                                                       "bmivalunder20_Mean","bmival_20_25_Mean","bmival_25_30_Mean","bmivalabove30_Mean","bmival_NA_Mean",
                                                       "pct_bmivalunder20_Mean","pct_bmival_20_25_Mean","pct_bmival_25_30_Mean",
                                                       "pct_bmivalabove30_Mean","pct_bmival_NA_Mean")] 

          # Negative binomial model of BMI 
          #model.bmi <- glm.nb(bmival_Mean ~ Year + offset(log(pop)), data=temp)
          
          #if (sum(temp$bmival_Mean,na.rm=T)>0 & summary(model.bmi)$coeff[[2,4]]<0.05) {
          #   temp <- temp %>% mutate(bmi.pred = exp(predict(model.bmi, temp, type = "link", se.fit = T)[[1]] +
          #                                          predict(model.bmi, temp, type = "link", se.fit = T)[[2]]))
          # # } else {
          #   temp <- temp %>% mutate(bmi.pred=as.numeric(recent[1,3]))
          # }
          temp <- temp[,`:=` (ht.pred = ifelse(Year<2022 & !is.na(htval_MeanNEW_m),htval_MeanNEW_m,as.numeric(recent[1,1])),
                               wt.pred = ifelse(Year<2022 & !is.na(wtval_Mean),wtval_Mean,as.numeric(recent[1,2])),
                                  bmi.pred = ifelse(Year<2022 & !is.na(bmival_Mean),bmival_Mean,as.numeric(recent[1,3])),
                                  
                                  htunder20.pred = ifelse(Year<2022 & !is.na(htvalunder20_MeanNEW_m),htvalunder20_MeanNEW_m,as.numeric(recent[1,4])),
                                  wtunder20.pred = ifelse(Year<2022 & !is.na(wtvalunder20_Mean),wtvalunder20_Mean,as.numeric(recent[1,9])),
                                  bmiunder20.pred = ifelse(Year<2022 & !is.na(bmivalunder20_Mean),bmivalunder20_Mean,as.numeric(recent[1,14])),
                                  
                                  ht2025.pred = ifelse(Year<2022 & !is.na(htval_20_25_MeanNEW_m),htval_20_25_MeanNEW_m,as.numeric(recent[1,5])),
                                  wt2025.pred = ifelse(Year<2022 & !is.na(wtval_20_25_Mean),wtval_20_25_Mean,as.numeric(recent[1,10])),
                                  bmi2025.pred = ifelse(Year<2022 & !is.na(bmival_20_25_Mean),bmival_20_25_Mean,as.numeric(recent[1,15])),
                                  
                                  ht2530.pred = ifelse(Year<2022 & !is.na(htval_25_30_MeanNEW_m),htval_25_30_MeanNEW_m,as.numeric(recent[1,6])),
                                  wt2530.pred = ifelse(Year<2022 & !is.na(wtval_25_30_Mean),wtval_25_30_Mean,as.numeric(recent[1,11])),
                                  bmi2530.pred = ifelse(Year<2022 & !is.na(bmival_25_30_Mean),bmival_25_30_Mean,as.numeric(recent[1,16])),
                                  
                                  htabove30.pred = ifelse(Year<2022 & !is.na(htvalabove30_MeanNEW_m),htvalabove30_MeanNEW_m,as.numeric(recent[1,7])),
                                  wtabove30.pred = ifelse(Year<2022 & !is.na(wtvalabove30_Mean),wtvalabove30_Mean,as.numeric(recent[1,12])),
                                  bmiabove30.pred = ifelse(Year<2022 & !is.na(bmivalabove30_Mean),bmivalabove30_Mean,as.numeric(recent[1,17])),
                                  
                                  htNA.pred = ifelse(Year<2022 & !is.na(htval_NA_MeanNEW_m),htval_NA_MeanNEW_m,as.numeric(recent[1,8])),
                                  wtNA.pred = ifelse(Year<2022 & !is.na(wtval_NA_Mean),wtval_NA_Mean,as.numeric(recent[1,13])),
                                  bmiNA.pred = ifelse(Year<2022 & !is.na(bmival_NA_Mean),bmival_NA_Mean,as.numeric(recent[1,18])),
                                  
                                  pct_bmiunder20.pred = ifelse(Year<2022 & !is.na(pct_bmivalunder20_Mean),pct_bmivalunder20_Mean,as.numeric(recent[1,19])),
                                  pct_bmi2025.pred = ifelse(Year<2022 & !is.na(pct_bmival_20_25_Mean),pct_bmival_20_25_Mean,as.numeric(recent[1,20])),
                                  pct_bmi2530.pred = ifelse(Year<2022 & !is.na(pct_bmival_25_30_Mean),pct_bmival_25_30_Mean,as.numeric(recent[1,21])),
                                  pct_bmiabove30.pred = ifelse(Year<2022 & !is.na(pct_bmivalabove30_Mean),pct_bmivalabove30_Mean,as.numeric(recent[1,22])),
                                  pct_bmiNA.pred = ifelse(Year<2022 & !is.na(pct_bmival_NA_Mean),pct_bmival_NA_Mean,as.numeric(recent[1,23])))]
          rm(recent,maxr,recent1)
          
          # Save projection
          subdata[[i]] <- temp
          # print(paste0(s," ", a, " ",c, " ",q))
          
          #Baseline BMI, Weight and Height by groups
          anthro <- temp %>% filter(Year==2021) %>% dplyr::select(ht.pred:pct_bmiNA.pred,Groups,Year) %>%
            dplyr::rename(Groups_red=Groups,
                          htval_Mean=ht.pred, wtval_Mean=wt.pred, bmival_Mean=bmi.pred, 
                          htvalunder20_Mean=htunder20.pred,htval_20_25_Mean=ht2025.pred,htval_25_30_Mean=ht2530.pred,htvalabove30_Mean=htabove30.pred,htval_NA_Mean=htNA.pred,
                          wtvalunder20_Mean=wtunder20.pred,wtval_20_25_Mean=wt2025.pred,wtval_25_30_Mean=wt2530.pred,wtvalabove30_Mean=wtabove30.pred,wtval_NA_Mean=wtNA.pred,
                          bmivalunder20_Mean=bmiunder20.pred,bmival_20_25_Mean=bmi2025.pred,bmival_25_30_Mean=bmi2530.pred,bmivalabove30_Mean=bmiabove30.pred,bmival_NA_Mean=bmiNA.pred,
                          pct_bmivalunder20_Mean=pct_bmiunder20.pred,pct_bmival_20_25_Mean=pct_bmi2025.pred,pct_bmival_25_30_Mean=pct_bmi2530.pred,
                          pct_bmivalabove30_Mean=pct_bmiabove30.pred,pct_bmival_NA_Mean=pct_bmiNA.pred) 
          baselineanthro <- rbind(baselineanthro, anthro)
          rm(anthro)
          
          #for doing the change by individual and not by group
          tempind <- anthroinfoind  %>% filter(Sexnumber==s, AgeGroups2==a, Country==c, IMD_quintile==q)
          recent1ind <- tempind %>% filter(is.na(tempind[,13])==FALSE) %>% ungroup()             #keeping when BMI not NA
          maxrind <- recent1ind %>% group_by(seriali) %>% summarise_if(is.numeric, max) %>% dplyr::select(Year, seriali)
          
          tempindind <- data.frame()
          z <- subset(tempind, !duplicated(seriali)) 
          seriali <- c(z$seriali)
          rm(z)
          for (e in seriali) {
            cind22 <- maxrind %>% filter(seriali==e) 
            yye <- as.numeric(cind22[1,1])
            cind <- tempind %>% filter(seriali==e, Year==yye) 
            tempind11 <- tempind %>% filter(seriali==e) %>% dplyr::select(-Groups) %>% 
              mutate(ht.pred = ifelse(Year<2020 & !is.na(htval),htval,as.numeric(cind[1,11])),
                     wt.pred = ifelse(Year<2020 & !is.na(wtval),wtval,as.numeric(cind[1,12])),
                     bmi.pred = ifelse(Year<2020 & !is.na(bmival),bmival,as.numeric(cind[1,13])),
                     
                     bmiunder20=ifelse(is.na(bmi.pred),NA,
                                       ifelse(bmi.pred<20,1,0)),
                     bmi2025=ifelse(is.na(bmi.pred),NA,
                                    ifelse(bmi.pred>=20 & bmi.pred<25,1,0)),
                     bmi2530=ifelse(is.na(bmi.pred),NA,
                                    ifelse(bmi.pred>=25 & bmi.pred<30,1,0)),
                     bmiover30=ifelse(is.na(bmi.pred),NA,
                                      ifelse(bmi.pred>=30,1,0)),
                     bmiNA=ifelse(is.na(bmi.pred),1,0))
            tempindind <- rbind(tempindind, tempind11)
            rm(yye,tempind11)
          }
          # Save projection
          subdata22[[i]] <- tempindind
          rm(temp, tempind,cind22,seriali,maxrind,recent1ind,cind)
          i <- i+1
        }
      }
    }
  }
  databb4 <- bind_rows(subdata)
  gc()
  
  ww <- bind_rows(subdata22)
  #merging conso and anthropo by indiv
  dd <- dietary_intakes %>% dplyr::select(c(seriali,ids,strata,weight,astrata1:astrata5, Age))
  #check that only one weight by seriali
  # z <- dd %>% filter(!duplicated(seriali))
  byindiv <- merge(bcind2,ww, by=c("seriali","Year")) %>% merge(dd, by=c("seriali")) %>% dplyr::select(-c(htval,wtval,bmival))
  
  rm(bcind,bcind2,ww,dd,anthroinfoind,tempindind)
  # #prevalence of BMI>20kg/m2 by AgeGroups
  # nb <- databb4 %>% mutate(c=1) %>% group_by(AgeGroups2,Year) %>% summarise(nbtot=count(c),nb=count(c[bmi.pred>20])) %>%
  #                   mutate(p_pred=nb/nbtot) %>% dplyr::select(AgeGroups2,Year,p_pred)
  # 
  # databb <- databb4 %>% left_join(nb, by=c("AgeGroups2","Year"))
  #rm(nb, databb1,databb2,databb3,databb4)
  
  databb <- databb4
  
  rm(databb3,databb4,subdata,subdata22)
  
  
  ##############################################################################
  #                      DEFINING ALL THE MODEL SCENARIOS                      #																				
  ##############################################################################
  # S0. no policy 
  
  # S1. Policy full coverage: people affected by the MenuLabelling: 
  #100% of micro & small businees + 41% of the large business + 71% of the medium business impacted by the new policy 
  #representing respectively  80%, 17% and 3% of the market. Then the policy will impact 1*0.80 + 0.71*0.03 + 0.41*0.17 = 89% of the business
  # + Compensation: 26.5 % compensation
  # People might eat fewer kcals in a restaurant by 
  # -	selecting smaller meals = (1) portion size
  # -	selecting meals that are less energy dense = (2) energy density. 
  # 
  # Those two ways appear to produce different levels of compensation:
  # (1)	= 42% compensation later in the day (https://www.cambridge.org/core/journals/british-journal-of-nutrition/article/downsizing-food-a-systematic-review-and-metaanalysis-examining-the-effect-of-reducing-served-food-portion-sizes-on-daily-energy-intake-and-body-weight/C6E701A6B87BC0AFBD3EF65AC5AE4FDA)
  #                       "changes to energy intake at meals caused by serving smaller portion sizes are in part later compensated for; approximately 42% of the reduction in energy
  #                         intake observed at manipulated portion size meals was 'compensated for' through additional energy intake at other meals"
  # (2)	= 11% compensation (https://ijbnpa.biomedcentral.com/articles/10.1186/s12966-022-01287-z)
  # 
  # As we don't really know how people will reduce their energy intake in response to kcal labelling (i.e. (1), (2), or both)  we will take an average (26.5%)
  
  # S1compmax: 42% compensation 
  # S1compmin: 11% compensation
  
  # S2. Energy current coverage: we reduced the amount of people affected by the MenuLabelling : 59% of the large business already have implemented the policy, 
  #     thus it's will be new only in 41%. But they represent only 17% of the market. Then the policy will impact 0.41*0.17 7% of the business
  
  # S1bis. Energy full coverage / different estimation for energy
  # S2bis. Energy current coverage  / different estimation for energy
  # S1qua. Energy full coverage / different estimation for energy: where
  # S2qua. Energy current coverage  / different estimation for energy: where
  # S1sensi. sensitivity analysis, reduction of 7.3% in energy intake full coverage : everyone.   
  # S2sensi. sensitivity analysis, reduction of 7.3% in energy intake current coverage
  # S1vgt. Vegetables full coverage : everyone. 
  # S2vgt. Vegetables full current coverage 
  # S1cum. Nrj + vgt full coverage : everyone. 
  # S2cum. Nrj + vgt full current coverage 
  
  #S1rev: sensi for review1; change by year, full coverage : everyone. 
  #S2rev: sensi for review1; change by year, current coverage
  
  #sensi for review1; use of turnover instead of the number of outlets
  #1turnover33: if 33% already have info; full coverage : everyone. 
  #2turnover33: if 33% already have info;current coverage
  #1turnover59: if 59% already have info; full coverage : everyone. 
  #2turnover59: if 59% already have info;current coverage
  
  # S3. Reformulation full: the retailers will decrease 15kcal each meal
  # S3comp. Reformulation full with 26.5% compensation
  # S3compmin. Reformulation full with 11% compensation
  # S3compmax. Reformulation full with 42% compensation
  # S3sensicomp. Reformulation: the retailers will decrease 15kcal each meal +/- 20% with 26.5% compensation
  
  # S4. Reformulation current: the retailers will decrease 15kcal each meal
  # S4comp. Reformulation current with 26.5% compensation
  # S4compmin. Reformulation current with 11% compensation
  # S4compmax. Reformulation current with 42% compensation
  # S4sensicomp. Reformulation: the retailers will decrease 15kcal each meal +/- 20% with 26.5% compensation
  
  
  # S5. full Conso  - Compensation + Reformulation
  # S5sensi. sensitivity analysis, reduction of 7.3% in energy intake full coverage : everyone (Conso  - Compensation + Reformulation)
  
  # S6. current Conso - Compensation + Reformulation
  # S6sensi. sensitivity analysis, reduction of 7.3% in energy intake full coverage : current (Conso  - Compensation + Reformulation)
  

  ### Create a dataframe of differential effects by age, Sex, and SES
  fififi2 <- fififi %>% mutate(XX=paste(Groups, IMD_quintile),IMD_quintile=as.numeric(IMD_quintile)) %>%
    filter(!duplicated(XX)) %>% dplyr::select(-c(XX, GroupswithoutCountry,porc.pop))
  #fififi2 <- fififi2 %>% mutate(Groups=GroupswithoutCountry) %>% dplyr::select(-GroupswithoutCountry)
  effect <- expand.grid(Country=c(dietary_intakes$Country[1]), Sex=factor(c("Men","Women")), AgeGroups2 = factor(1:4, labels=c("30-44","45-59","60-74","75 & over")),
                        IMD_quintile = factor(1:5),scenario=factor(c("0",
                                                                     "1","2","3","4","5","6",
                                                                     "1.sensi","2.sensi","5.sensi","6.sensi",#"3.sensi","4.sensi",
                                                                     "3bis","4bis",
                                                                     "3.sensi.comp","4.sensi.comp",
                                                                     "1rev", "2rev",
                                                                     "1turnover33", "2turnover33","3turnover33","4turnover33","5turnover33","6turnover33",
                                                                     "3turnover33.comp","4turnover33.comp",
                                                                     "1turnover59","2turnover59","3turnover59","4turnover59","5turnover59","6turnover59",
                                                                     "3turnover59.comp","4turnover59.comp",
                                                                     "1.compmin","2.compmin","3.compmin","4.compmin","5.compmin","6.compmin",
                                                                     "1.compmax","2.compmax","3.compmax","4.compmax","5.compmax","6.compmax",
                                                                     "1turnover33.compmin", "2turnover33.compmin","3turnover33.compmin","4turnover33.compmin","5turnover33.compmin","6turnover33.compmin",
                                                                     "1turnover33.compmax", "2turnover33.compmax","3turnover33.compmax","4turnover33.compmax","5turnover33.compmax","6turnover33.compmax",
                                                                     "1.vgt","2.vgt")))  %>%
    mutate(IMD_quintile=as.numeric(IMD_quintile),
           Groups=paste(Country, Sex, AgeGroups2),
           Groups_red=paste(Country, Sex, AgeGroups2,IMD_quintile)) %>% 
    left_join(fififi2, by=c("Groups","IMD_quintile"))  %>%
    #Impact of the policy
    mutate(#Energy kcal labelling decrease Energy (Crockett 2018): reduction of 47 kcal in energy purchased (MD:-46.72 kcal, 95% CI: -78.35, -15.10, N = 1877).
      policy.energy.effect = rnorm(1,mean = -47, sd = (-15-(-78))/3.92),
      #Food labeling decreased consumer intakes of energy by 7.3% (95% CI:-10.1%, -4.4%) ; Shangguan 2019
      policy.energy.effect_sensi = rnorm(1,mean = -0.073, sd = (-0.044-(-0.101))/3.92),
      #Food labeling Increasing vegetable consumption by 13.5% (95% CI=2.4%, 24.6%, n=5) ; Shangguan 2019
      policy.vegetable.effect = rnorm(1,mean = 0.135, sd = (0.246-0.024)/3.92),
      
      #Reformulation: Zlatevskaa, but we don't have info on SD
      #policy.energy.refeffect_bis = rnorm(1,mean = -15, sd = 0),
      #sensitivity analysis 20%
      #policy.energy.refeffect = rnorm(1,mean = -15, sd =(((15*0.2)/1.96) + (-(15*0.2)/-1.96))/2),
      #Email from Zlatevska saying that Estimate-15.34 [-23.1780;-7.5150]
      policy.energy.refeffect = rnorm(1,mean = -15, sd = (-7.5150-(-23.1780))/3.92),
  
      #review old - dont know
      #estimation of the CI for the review to be −15.34 [-18.68; -12.00], 
      policy.energy.refeffect_bis = rnorm(1,mean = -15.34, sd=10.922), #sqrt(119.29)
      
      #not done
      policy.energy.refeffect_sensi = rnorm(1,mean = -15, sd =(((15*0.2)/1.96) + (-(15*0.2)/-1.96))/2),
      
      #Compensation: we don't have info on SD
      policy.energy.compeffect = rnorm(1,mean = 0.265, sd = 0),
      policy.energy.compeffect_min = rnorm(1,mean = 0.11, sd = 0),
      policy.energy.compeffect_max = rnorm(1,mean = 0.42, sd = 0),
      
      #the current policy will impact 41% of 17% of the food sector = 7%
      #the full policy will impact 88% of 100% of the food sector = 88%
      policy.coverage = ifelse(scenario %in% c("2","2.sensi","2rev","2.sensi","2.compmin","2.compmax","2.vgt",
                                               "4","4bis","4.comp","4.sensi.comp","4.compmin","4.compmax",#"4.sensi",
                                               "6","6.sensi","6.compmin","6.compmax"), 0.07,
                        ifelse(scenario %in% c("1turnover33","3turnover33","5turnover33",
                                               "3turnover33.comp",
                                               "1turnover33.compmin","3turnover33.compmin","5turnover33.compmin",
                                               "1turnover33.compmax","3turnover33.compmax","5turnover33.compmax"), 0.67, #33% already have info
                        ifelse(scenario %in% c("1turnover59","3turnover59","5turnover59",
                                               "3turnover59.comp",
                                               "1turnover59.compmin","3turnover59.compmin","5turnover59.compmin",
                                               "1turnover59.compmax","3turnover59.compmax","5turnover59.compmax"), 0.69, 
                        ifelse(scenario %in% c("2turnover33","4turnover33","6turnover33",
                                               "4turnover33.comp",
                                               "2turnover33.compmin","4turnover33.compmin","6turnover33.compmin",
                                               "2turnover33.compmax","4turnover33.compmax","6turnover33.compmax"), 0.31, 
                        ifelse(scenario %in% c("2turnover59","4turnover59","6turnover59",
                                               "4turnover59.comp",
                                               "2turnover59.compmin","4turnover59.compmin","6turnover59.compmin",
                                               "2turnover59.compmax","4turnover59.compmax","6turnover59.compmax"), 0.19, 0.88)))))) %>% #here, we only consider restaurant coverage 
    left_join(rr.food_morta.sim, by="AgeGroups2") 

  setDT(pob2020_red)
  pob2020_red_ok <- pob2020_red[, Groups:=paste(Country,Sex,AgeGroups2)][, .(Groups,weight2,weight_sex2)]
  
  
  t1 <- data.frame(policy.energy.effect=effect$policy.energy.effect,
                   policy.energy.effect_sensi=effect$policy.energy.effect_sensi,
                   policy.vegetable.effect=effect$policy.vegetable.effect,
                   policy.energy.refeffect=effect$policy.energy.refeffect, 
                   policy.energy.refeffect_sensi=effect$policy.energy.refeffect_sensi, 
                   policy.energy.refeffect_bis=effect$policy.energy.refeffect_bis,
                   policy.energy.compeffect=effect$policy.energy.compeffect, 
                   policy.energy.compeffect_min=effect$policy.energy.compeffect_min,
                   policy.energy.compeffect_max=effect$policy.energy.compeffect_max,
                   sim=sim)
  ggt <- rbind(ggt, t1)
  rm(t1,fififi2,rr.food_morta.sim)
  
  memean <- databb[, mean(quant.Energy_out_kcal_Mean.pred,na.rm=T), by=c("IMD_quintile", "Year")][, .(IMD_quintile=as.numeric(IMD_quintile),Year,mean_energy_byIMD=V1)]
  
  setDT(byindiv)
  nrjpred2021 <- byindiv[Year==2021,.(seriali,quant.Energy_out_kcal.pred_2021=quant.Energy_out_kcal.pred)]
  
  ##############################################################################
  #Step A : Change in dietary outcome attributable to the policy, in different scenarios																					
  ##############################################################################
  #NEW BY INDIV
  setDT(effect)
  setDT(memean)
  datacc2 <- byindiv[nrjpred2021, on = 'seriali'][, `:=` (Groups=paste(Country, Sex, AgeGroups2),
                                                          Groups_red=paste(Country, Sex, AgeGroups2,IMD_quintile),
                                                          IMD_quintile=as.numeric(IMD_quintile))][,
                     !c('Country', 'Sex','AgeGroups2','Groups', 'IMD_quintile')][effect,on ='Groups_red',allow.cartesian=TRUE][memean,on = c('Year','IMD_quintile')]
  
  datacc1nn <- datacc2[, `:=` ( 
      #making the names homogeneous
      quant.Energy_out_kcal_Mean.pred=quant.Energy_out_kcal.pred,
      quant.Energy_kcal_meal_Mean.pred=quant.Energy_kcal_meal.pred,
      quant.Mealoutofhome_Mean.pred=quant.Mealoutofhome.pred,
      quant.Mealoutofhome_year_Mean.pred=quant.Mealoutofhome_year.pred,
      quant.totalveg_out_Mean.pred=quant.totalveg_out.pred,
      ht.pred=ht.pred/100)][, `:=` ( 
      #first model, reduction of 47kcal in energy
      #we only consider restaurant coverage as we already have take into account "eating outside" probability in the baseline intakes.
      #tt=((quant.Energy_out_kcal_Mean.pred * policy.coverage) + (policy.energy.effect * (1-policy.energy.compeffect))), #Here, it's an addition even if we can normally expect a multiplication
      tt=((quant.Energy_out_kcal_Mean.pred + (policy.energy.effect * (1-policy.energy.compeffect))) * policy.coverage), #Here, it's an addition even if we can normally expect a multiplication
          
      #for review
      ttrev=(((policy.energy.effect * (1-policy.energy.compeffect))) * policy.coverage * quant.Mealoutofhome_year_Mean.pred), #Here, it's an addition even if we can normally expect a multiplication
      
      ttcompmin=((quant.Energy_out_kcal_Mean.pred + (policy.energy.effect * (1-policy.energy.compeffect_min))) * policy.coverage), #Here, it's an addition even if we can normally expect a multiplication
      ttcompmax=((quant.Energy_out_kcal_Mean.pred + (policy.energy.effect * (1-policy.energy.compeffect_max))) * policy.coverage), #Here, it's an addition even if we can normally expect a multiplication

      #not out of home but per meal, so we need to take into account "eating outside" probability
      ttbis=((quant.Energy_kcal_meal_Mean.pred + policy.energy.effect) * (policy.coverage * quant.Mealoutofhome_Mean.pred)), #Here, it's an addition even if we can normally expect a multiplication

      #relative change
      ttter=((quant.Energy_out_kcal_Mean.pred + (quant.Energy_out_kcal_Mean.pred * (policy.energy.effect/mean_energy_byIMD))) * policy.coverage),

      #sensitivity analysis, reduction of 7.3% in energy intake
      tt_sensi=((quant.Energy_out_kcal_Mean.pred + ((quant.Energy_out_kcal_Mean.pred*policy.energy.effect_sensi) * (1-policy.energy.compeffect))) * policy.coverage),

      #reformulation
      ttref=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect * (1-0))) * policy.coverage) ,
      ttref_bis=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect_bis* (1-0))) * policy.coverage),
      #not done
      #ttref_sensi=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect_sensi* (1-0))) * policy.coverage),
      
      ttref_comp=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect * (1-policy.energy.compeffect))) * policy.coverage),
      ttref_compmin=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect * (1-policy.energy.compeffect_min))) * policy.coverage),
      ttref_compmax=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect * (1-policy.energy.compeffect_max))) * policy.coverage),
      
      ttref_sensi_comp=((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect_sensi * (1-policy.energy.compeffect))) * policy.coverage),
      ttref_bis_comp  =((quant.Energy_out_kcal_Mean.pred + (policy.energy.refeffect_bis * (1-policy.energy.compeffect))) * policy.coverage),
      
      #combined Conso - Compensation + Reformulation
      tt5.6=((quant.Energy_out_kcal_Mean.pred + (((policy.energy.effect) * (1-policy.energy.compeffect)) + policy.energy.refeffect)) * policy.coverage),
      tt5.6_sensi=((quant.Energy_out_kcal_Mean.pred + (((quant.Energy_out_kcal_Mean.pred*policy.energy.effect_sensi) * (1-policy.energy.compeffect)) + policy.energy.effect_sensi)) * policy.coverage),

      tt5.6compmin=((quant.Energy_out_kcal_Mean.pred + ((policy.energy.effect * (1-policy.energy.compeffect_min)) + policy.energy.refeffect)) * policy.coverage) , #Here, it's an addition even if we can normally expect a multiplication
      tt5.6compmax=((quant.Energy_out_kcal_Mean.pred + ((policy.energy.effect * (1-policy.energy.compeffect_max)) + policy.energy.refeffect)) * policy.coverage))][, `:=` ( 
      nrj=case_when(
        Year < 2022 ~ quant.Energy_out_kcal_Mean.pred, #2022: year of implementation of the policy in our model
        
        scenario == "0" ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("1","2","1turnover33","1turnover59","2turnover33","2turnover59") & (tt > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("1","2","1turnover33","1turnover59","2turnover33","2turnover59") & (tt <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("1.sensi","2.sensi") & (tt_sensi  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt_sensi, 
        scenario %in% c("1.sensi","2.sensi") & (tt_sensi <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("1rev","2rev") & (ttrev > 0) ~ ttrev,
        scenario %in% c("1rev","2rev") & (ttrev <= 0) ~ ttrev,
        
        scenario %in% c("1.compmin","2.compmin") & (ttcompmin  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttcompmin, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("1.compmin","2.compmin") & (ttcompmin  <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("1.compmax","2.compmax") & (ttcompmax  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttcompmax, 
        scenario %in% c("1.compmax","2.compmax") & (ttcompmax  <= 0) ~ quant.Energy_out_kcal_Mean.pred,

        scenario == "1.vgt" ~ quant.Energy_out_kcal_Mean.pred, #this scenario do not impact nrj
        scenario == "2.vgt" ~ quant.Energy_out_kcal_Mean.pred, #this scenario do not impact nrj
        
        scenario %in% c("3","4","3turnover33","3turnover59","4turnover33","4turnover59") & (ttref  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref,
        scenario %in% c("3","4","3turnover33","3turnover59","4turnover33","4turnover59") & (ttref <= 0) ~ quant.Energy_out_kcal_Mean.pred, 
        
        scenario  %in% c("3bis","4bis") & (ttref_bis  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_bis,
        scenario  %in% c("3bis","4bis") & (ttref_bis <= 0) ~ quant.Energy_out_kcal_Mean.pred, 
        
        #scenario  %in% c("3.sensi","4.sensi") & (ttref_sensi  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_sensi,
        #scenario  %in% c("3.sensi","4.sensi") & (ttref_sensi <= 0) ~ quant.Energy_out_kcal_Mean.pred, 
        
        scenario %in% c("3.comp","4.comp","3turnover33.comp","3turnover59.comp","4turnover33.comp","4turnover59.comp") & (ttref_comp  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_comp,
        scenario %in% c("3.comp","4.comp","3turnover33.comp","3turnover59.comp","4turnover33.comp","4turnover59.comp") & (ttref_comp <= 0) ~ quant.Energy_out_kcal_Mean.pred, 
        
        scenario %in% c("3.compmin","4.compmin") & (ttref_compmin  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_compmin, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("3.compmin","4.compmin") & (ttref_compmin  <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("3.compmax","4.compmax") & (ttref_compmax  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_compmax, 
        scenario %in% c("3.compmax","4.compmax") & (ttref_compmax  <= 0) ~ quant.Energy_out_kcal_Mean.pred,
      
        scenario  %in% c("3.sensi.comp","4.sensi.comp") & (ttref_sensi_comp  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + ttref_sensi_comp,
        scenario  %in% c("3.sensi.comp","4.sensi.comp") & (ttref_sensi_comp <= 0) ~ quant.Energy_out_kcal_Mean.pred, 
      
        #combined
        scenario %in% c("5","6","5turnover33","5turnover59","6turnover33","6turnover59") & (tt5.6  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt5.6, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("5","6","5turnover33","5turnover59","6turnover33","6turnover59") & (tt5.6  <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("5.sensi","6.sensi") & (tt5.6_sensi  > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt5.6_sensi, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("5.sensi","6.sensi") & (tt5.6_sensi  <= 0) ~ quant.Energy_out_kcal_Mean.pred,
      
        scenario %in% c("5.compmin","6.compmin") & (tt5.6compmin > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt5.6compmin, #Intakes non-impacted by policy + Intakes impacted 
        scenario %in% c("5.compmin","6.compmin") & (tt5.6compmin <= 0) ~ quant.Energy_out_kcal_Mean.pred,
        
        scenario %in% c("5.compmax","6.compmax") & (tt5.6compmax > 0) ~ (quant.Energy_out_kcal_Mean.pred*(1-policy.coverage)) + tt5.6compmax, 
        scenario %in% c("5.compmax","6.compmax") & (tt5.6compmax <= 0) ~ quant.Energy_out_kcal_Mean.pred),

      #vegetable consumption increase by 13.5%
      tt_veg=((quant.totalveg_out_Mean.pred * policy.coverage) + (quant.totalveg_out_Mean.pred * policy.vegetable.effect)))][, `:=` (  
      
      vgt=case_when(Year < 2022 ~ quant.totalveg_out_Mean.pred, #2022: year of implementation of the policy in our model
                    scenario == "0" ~ quant.totalveg_out_Mean.pred,
                    scenario %in% c("1.vgt","2.vgt") & (tt_veg > 0) ~ (quant.totalveg_out_Mean.pred*(1-policy.coverage)) + tt_veg, #Intakes non-impacted by policy + Intakes impacted 
                    scenario %in% c("1.vgt","2.vgt") & (tt_veg <= 0) ~ quant.totalveg_out_Mean.pred,
                    
                    #these scenarii do not impact vgt
                    scenario %nin% c("0","1.vgt","2.vgt") ~ quant.totalveg_out_Mean.pred))][, `:=` (  
      
      # change in nrj consumption
      dif.nrj = ifelse(nrj==0, 0,
                ifelse(nrj > quant.Energy_out_kcal_Mean.pred, 0,
                ifelse(scenario %in% c("1rev","2rev"), nrj,
                nrj - quant.Energy_out_kcal_Mean.pred))),
      
      #change in vegetable consumption
      dif.vgt = vgt - quant.totalveg_out_Mean.pred,
      
      #Physical activity level (PAL) : for now, we imput 1.5 to everyone
      PAL = 1.5)][, `:=` (  
      #for review
      dif.nrj_day=dif.nrj*3,
      #dif.nrj_year=dif.nrj*quant.Mealoutofhome_year_Mean.pred,
      dif.nrj_year=dif.nrj)][, `:=` (
      
      ##############################################################################
      #Step B: Change in health outcome (disease/mortality) 																			
      ##############################################################################
      # Energy changes lead to obesity changes
      # We estimated weight change using the Christiansen and Garby method: 2002 Prediction of body weight changes caused by changes in energy balance 
      # This method converts changes in the ratio of calories consumed to calories expended 
      # into changes in body weight, based on principles of energy conservation 
      
      # the change in the distribution of BMI in the counterfactual scenario is calculates by using equations derived by Christiansen and
      # Garby to estimate the new steady state body weight that would be produced after a change in energy
      # balance (i.e., either changes to total energy intake or total energy output).
      #  BWss is steady state body weight measured in kg, 
      #  EI is energy intake measured in MJ per day, 
      #  PAL is physical activity level, 
      # a ratio of the total energy expenditure over resting energy
      # expenditure. 
      # In these equations k is a constant term that is based on both fundamental principles of energy conservation
      # and directly measured data and takes the value of 17.7 for men and 20.7 for women:
      #   ??BW = k * ??(EI/ PAL) 
      
      # With Energy intake in MJ
      # men:   ??BWss= 17.7 * ??(EI/PAL) 
      # women: ??BWss= 20.7 * ??(EI/PAL)
      BWss = case_when(Sex=="Men"   ~ 17.7 * ((dif.nrj*(4.2/1000))/PAL), #1 Kilocalorie = (4.2/1000) MJ
                       Sex=="Women" ~ 20.7 * ((dif.nrj*(4.2/1000))/PAL)),
      
      BWss_year = case_when(Sex=="Men"   ~ 17.7 * ((dif.nrj_year*(4.2/1000))/PAL), #1 Kilocalorie = (4.2/1000) MJ
                            Sex=="Women" ~ 20.7 * ((dif.nrj_year*(4.2/1000))/PAL)))][, `:=` (  
                              
      wt_byindiv_new=wt.pred+BWss,                    
      wt_byindiv_new_year=wt.pred+BWss_year)]
  #sensi for review
  # Based on Hall et al.
  #https://pdf.sciencedirectassets.com/272801/1-s2.0-S1570677X11X00047/1-s2.0-S1570677X11000906/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjENb%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIBB%2F4ZzafdMeYffIQj1aQdsowxPF%2F3EmKWbCXOA5vk64AiEAh5aisgEc4XWi5fqLOctxJgueHsFM0EKakQhHuZWfvy8qswUIXxAFGgwwNTkwMDM1NDY4NjUiDLBzCvEb6Gw%2BphnNVSqQBeO5Tk6bn7Ru%2BmInmXkF8orUfWITuK%2B0O%2BAQSatG1jFAXP4Aa0DsYfBPW6OS30yuiG%2BK3f9DjJHISGfEPHhB2rpxQ7b3YyfrkI5ix3Yh%2FD55hw%2BDvknm%2FctgnHz5g%2B9agF3E99xTmVNfw0%2BcAl9FqfIex1Xb109fCKPiQrSLWJ7Qkdc46VAXUeIXgoJAPDMdWTW8kOSqFhmFSCzxR94mUpXQn4L%2BOT07rdQ%2FoZyWcyZGewMtYM0yZM7lNTLio7XdQn7EbbmXfygq3WUueWp7Blqjnv49yD5u87%2FLHw1IYh70LMfLqJqtl4C0Xbh7Sl3hvhWjolerr3mmQyhNb5CS%2Fhi7LG%2BLiA%2FHP7IGWJI2DEAuazNSlUv9M%2BE1nqHndMqfr9S%2B%2B%2FruyF8LXhSeXlTWE5O9ghQ8NwHNO6Jpgigp5tM%2FiYO%2BGpth2G3wQR6vUX1QdL%2Fq%2B7LZKWfeD9Vb9PcgizV81zBUq8u1cpc%2FxklQ7uXOZisQFiB%2F0O61WI7XuInSfFdPUzU3ZycT5%2F27uaUKSgLsUWyyCjt2hQB91tRa9t1D2ikwv6dCE09Mc52x87vTjCItah%2FEaN%2BI%2FmOQJaEPv86PQbvye6Vept%2BuLyBdLL4VelqQhosxeEVv4L2CnrXk74K%2BlWbUh19Tbmls1Od18K%2FJ35cs5f8SQ9XaTLo9wdn%2Fa%2FQSvsoUk2W9KBbvOl9ycNZrwKycjo%2BDQ2RDWpkT%2F8KbTE18B%2B6kIBt3MSxSmVexFgaLcAZHGvvfvTXV8Nt3ar01gVZd8bWgTnKrEJ5t%2BebMcUN6ggzo5vu6j5Y40uzsjFJ6Le8qsnSS9czMqHEmktjCLKd2mGAlICVdQ35%2FuFHIGQ1H9jdRnLA9nSx31cLGMKqD1aUGOrEBh0lnu5OcoWzaPGdIfYx3h8h1btDulIWvQHFX%2B0yd%2BJdERA2TJu4hX0eA4e7RT%2BD6TcVWg%2B9zFts2uyHr3DKLwQwU3CxtQgYhpKcIeN3ng%2B02FLAUMZxHp8YV9eKIVQbAPR8zNWrQdzn9aXcRDM%2B4rd4m5sAnKSbbO56XZg5zgE%2BmSFbYsvOKh5ERdgY8thvfvLHPuCUTcj5hA9kpi39jWoDiBY7FDXbMIMbMXbdeBRzk&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20230717T140012Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYY6DWDSVD%2F20230717%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=b2bd7e14b7abaa498b5e5187f648be6c75804109f7d82d0a013c7b6ceaff68fa&hash=f15df44f867c15e720b2c87f3ff2b8f1671e0d736d9733b52a4f60d4c56719c1&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1570677X11000906&tid=spdf-f512e5bd-6a84-4c7b-bb9c-e2113a849090&sid=289dd9a34d220842962b50e3075deafa7696gxrqb&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=010158055c5d5e505a52&rr=7e8302864eb1360d&cc=gb
  
  # we are using bw package (based on Hall) to do the estimations
  #Dalia Camacho-Garcia-Formenti and Rodrigo Zepeda-Tello (2018). bw: Dynamic Body Weight Models for Children and Adults. R package version 1.0.0.
  #Days to model
  days <- 2500
  
  hall <- datacc1nn[scenario %in% c("1","2","3","4","5","6") & Year>2021 & !is.na(ht.pred) & !is.na(wt.pred),]
  datasvy <- setDF(hall[,.(seriali,Year,scenario,
                        sex=ifelse(Sex=="Men","male","female"),
                        ht=ht.pred, 
                        bw=wt.pred,
                        age=Age,
                        kcal=dif.nrj, 
                        PAL)])

  #Energy intake matrix
  EIchange <- matrix(rep(datasvy$kcal, days),ncol = days)
  gc()

  #Calculate weight change
  #we dont put EI=because create false results dont know why
  weight <- adult_weight(datasvy$bw, datasvy$ht, datasvy$age, datasvy$sex, PAL=datasvy$PAL, days = 2500, EIchange)$Body_Weight[,c(1,2500)] #we dont put EI=because create false results dont know why
  #weight$Body_Weight[,c(1,2500)]

  #Energy intake matrix
  #test to reduce time
  # test <- data.frame()
  # k <- 1
  # for(i in 1:nrow(datasvy[1:1e4,])){
  #     j <- k + 9
  #     i <- k:j
  #     EIchange <- matrix(rep(datasvy$kcal[i], days),ncol = days)
  #     tt <- adult_weight(datasvy$bw[i], datasvy$ht[i], datasvy$age[i], datasvy$sex[i], PAL=datasvy$PAL[i], days = 2500, EIchange)$Body_Weight[,c(1,2500)]
  #     test <- rbind(test,tt)
  #     rm(tt,EIchange)
  #     k <- k + 10
  # }
  # gc()
  # 
  # #Calculate weight change
  # #we dont put EI=because create false results dont know why
  # EIchange <- matrix(rep(datasvy$kcal[1:1e4], days),ncol = days)
  # test2 <- adult_weight(datasvy$bw[1:1e4], datasvy$ht[1:1e4], datasvy$age[1:1e4], datasvy$sex[1:1e4], PAL=datasvy$PAL[1:1e4], days = 2500, EIchange)
  # z <- cbind(test2,test)
  # 
  # 
  # EIchange <- matrix(rep(datasvy$kcal, days),ncol = days)
  # weight <- adult_weight(datasvy$bw, datasvy$ht, datasvy$age, datasvy$sex, PAL=datasvy$PAL, days = 2500, EIchange)$Body_Weight[,c(1,2500)] #we dont put EI=because create false results dont know why
  # #weight$Body_Weight[,c(1,2500)]
  
  
  rm(EIchange,datasvy)
  gc()
  
  #tomergehall <- cbind(hall,weight$Body_Weight[,c(1,2500)])
  tomergehall <- cbind(hall,weight)
  #tomergehall$BWssHALL <- tomergehall$V2-tomergehall$V1
  tomergehall$wt_byindiv_newHALL <- tomergehall$V2
  tomergehallok <- tomergehall[,.(seriali,Year,scenario,wt_byindiv_newHALL)]
  rm(weight,hall)
  
  #need to merge back to datacc1nn, change to a real merge by id, scenario and year
  datacc1nn2 <- merge(x=datacc1nn,y=tomergehallok, by=c("seriali","Year","scenario"), all.x=TRUE)
  datacc1nn2$wt_byindiv_newHALL <- ifelse(datacc1nn2$scenario==0,datacc1nn2$wt.pred,datacc1nn2$wt_byindiv_newHALL)
  ###END
  
  datacc1 <- datacc1nn2[, `:=` (                      
      #Calculate the new BMI / supposing that ht is constent in time
      bmi_byindiv_new=wt_byindiv_new/((ht.pred)^2),
      bmi_byindiv_new_year=wt_byindiv_new_year/((ht.pred)^2),
      
      bmi_byindiv_new_sensiHALL=wt_byindiv_newHALL/((ht.pred)^2),
        
      #old one
      bmi_byindiv_pred=wt.pred/((ht.pred)^2))][, `:=` (  
      
      bmiunder20_byindiv_new=ifelse(is.na(bmi_byindiv_new),NA,
                                    ifelse(bmi_byindiv_new<20,1,0)),
      bmi2025_byindiv_new=ifelse(is.na(bmi_byindiv_new),NA,
                                 ifelse(bmi_byindiv_new>=20 & bmi_byindiv_new<25,1,0)),
      bmi2530_byindiv_new=ifelse(is.na(bmi_byindiv_new),NA,
                                 ifelse(bmi_byindiv_new>=25 & bmi_byindiv_new<30,1,0)),
      bmiover30_byindiv_new=ifelse(is.na(bmi_byindiv_new),NA,
                                   ifelse(bmi_byindiv_new>=30,1,0)),
      bmiNA_byindiv_new=ifelse(is.na(bmi_byindiv_new),1,0),
      
      bmiunder20_byindiv_new_year=ifelse(is.na(bmi_byindiv_new_year),NA,
                                    ifelse(bmi_byindiv_new_year<20,1,0)),
      bmi2025_byindiv_new_year=ifelse(is.na(bmi_byindiv_new_year),NA,
                                 ifelse(bmi_byindiv_new_year>=20 & bmi_byindiv_new_year<25,1,0)),
      bmi2530_byindiv_new_year=ifelse(is.na(bmi_byindiv_new_year),NA,
                                 ifelse(bmi_byindiv_new_year>=25 & bmi_byindiv_new_year<30,1,0)),
      bmiover30_byindiv_new_year=ifelse(is.na(bmi_byindiv_new_year),NA,
                                   ifelse(bmi_byindiv_new_year>=30,1,0)),
      bmiNA_byindiv_new_year=ifelse(is.na(bmi_byindiv_new_year),1,0),
      
      ##FOR REVIEW HALL
      bmiunder20_byindiv_new_sensiHALL=ifelse(is.na(bmi_byindiv_new_sensiHALL),NA,
                                          ifelse(bmi_byindiv_new_sensiHALL<20,1,0)),
      bmi2025_byindiv_new_sensiHALL=ifelse(is.na(bmi_byindiv_new_sensiHALL),NA,
                                       ifelse(bmi_byindiv_new_sensiHALL>=20 & bmi_byindiv_new_sensiHALL<25,1,0)),
      bmi2530_byindiv_new_sensiHALL=ifelse(is.na(bmi_byindiv_new_sensiHALL),NA,
                                       ifelse(bmi_byindiv_new_sensiHALL>=25 & bmi_byindiv_new_sensiHALL<30,1,0)),
      bmiover30_byindiv_new_sensiHALL=ifelse(is.na(bmi_byindiv_new_sensiHALL),NA,
                                         ifelse(bmi_byindiv_new_sensiHALL>=30,1,0)),
      bmiNA_byindiv_new_sensiHALL=ifelse(is.na(bmi_byindiv_new_sensiHALL),1,0),
      
      
      #sensi for review
      # use of the approximate rule of thumb 
      #a 55 kcal per day reduction in calorie intake would lead to one pound weight loss within 1 year, with no further weight loss in the future.
      #1 lb = 0.45359237 kg
      BWss_sensi = (dif.nrj/55)*0.45359237,
      BWss_sensi_day = (dif.nrj_day/55)*0.45359237)][, `:=` (  
        
      wt_byindiv_new_sensi=wt.pred+BWss_sensi,
      wt_byindiv_new_sensi_day=wt.pred+BWss_sensi_day)][, `:=` (  
      #Calculate the new BMI / supposing that ht is constant in time
      bmi_byindiv_new_sensi=wt_byindiv_new_sensi/((ht.pred)^2),
      bmi_byindiv_new_sensi_day=wt_byindiv_new_sensi_day/((ht.pred)^2))][, `:=` (  
     
        bmiunder20_byindiv_new_sensi=ifelse(is.na(bmi_byindiv_new_sensi),NA,
                                            ifelse(bmi_byindiv_new_sensi<20,1,0)),
        bmi2025_byindiv_new_sensi=ifelse(is.na(bmi_byindiv_new_sensi),NA,
                                         ifelse(bmi_byindiv_new_sensi>=20 & bmi_byindiv_new_sensi<25,1,0)),
        bmi2530_byindiv_new_sensi=ifelse(is.na(bmi_byindiv_new_sensi),NA,
                                         ifelse(bmi_byindiv_new_sensi>=25 & bmi_byindiv_new_sensi<30,1,0)),
        bmiover30_byindiv_new_sensi=ifelse(is.na(bmi_byindiv_new_sensi),NA,
                                           ifelse(bmi_byindiv_new_sensi>=30,1,0)),
        bmiNA_byindiv_new_sensi=ifelse(is.na(bmi_byindiv_new_sensi),1,0),
        
      bmiunder20_byindiv_new_sensi_day=ifelse(is.na(bmi_byindiv_new_sensi_day),NA,
                                          ifelse(bmi_byindiv_new_sensi_day<20,1,0)),
      bmi2025_byindiv_new_sensi_day=ifelse(is.na(bmi_byindiv_new_sensi_day),NA,
                                       ifelse(bmi_byindiv_new_sensi_day>=20 & bmi_byindiv_new_sensi_day<25,1,0)),
      bmi2530_byindiv_new_sensi_day=ifelse(is.na(bmi_byindiv_new_sensi_day),NA,
                                       ifelse(bmi_byindiv_new_sensi_day>=25 & bmi_byindiv_new_sensi_day<30,1,0)),
      bmiover30_byindiv_new_sensi_day=ifelse(is.na(bmi_byindiv_new_sensi_day),NA,
                                         ifelse(bmi_byindiv_new_sensi_day>=30,1,0)),
      bmiNA_byindiv_new_sensi_day=ifelse(is.na(bmi_byindiv_new_sensi_day),1,0))]
      

  
  rm(byindiv,nrjpred2021)
  
  #calculating the % of BMI<20 by groups
  pctcalcul <- setDT(datacc1)[!is.na(bmi_byindiv_new),][, .(pct_byindiv_under20_new=weighted.mean(bmiunder20_byindiv_new,weight,na.rm = TRUE),
                                                    pct_byindiv_2025_new=weighted.mean(bmi2025_byindiv_new,weight,na.rm = TRUE),
                                                    pct_byindiv_2530_new=weighted.mean(bmi2530_byindiv_new,weight,na.rm = TRUE),
                                                    pct_byindiv_over30_new=weighted.mean(bmiover30_byindiv_new,weight,na.rm = TRUE),
                                                    pct_byindiv_NA_new=weighted.mean(bmiNA_byindiv_new,weight,na.rm = TRUE),
                                                    weight_grp=sum(weight)),by = c("Groups_red","Year","scenario")]
  #test <- pctcalcul %>% mutate(sum=pct_byindiv_under20_new+pct_byindiv_2025_new+pct_byindiv_2530_new+pct_byindiv_over30_new)
  
  pctcalcul_year <- setDT(datacc1)[!is.na(bmi_byindiv_new_year),][, .(pct_byindiv_under20_new_year=weighted.mean(bmiunder20_byindiv_new_year,weight,na.rm = TRUE),
                                                                      pct_byindiv_2025_new_year=weighted.mean(bmi2025_byindiv_new_year,weight,na.rm = TRUE),
                                                                      pct_byindiv_2530_new_year=weighted.mean(bmi2530_byindiv_new_year,weight,na.rm = TRUE),
                                                                      pct_byindiv_over30_new_year=weighted.mean(bmiover30_byindiv_new_year,weight,na.rm = TRUE),
                                                                      pct_byindiv_NA_new_year=weighted.mean(bmiNA_byindiv_new_year,weight,na.rm = TRUE)),by = c("Groups_red","Year","scenario")]
  
  pctcalcul_sensiHALL <- setDT(datacc1)[!is.na(bmi_byindiv_new_sensi),][, .(pct_byindiv_under20_new_sensiHALL=weighted.mean(bmiunder20_byindiv_new_sensiHALL,weight,na.rm = TRUE),
                                                                        pct_byindiv_2025_new_sensiHALL=weighted.mean(bmi2025_byindiv_new_sensiHALL,weight,na.rm = TRUE),
                                                                        pct_byindiv_2530_new_sensiHALL=weighted.mean(bmi2530_byindiv_new_sensiHALL,weight,na.rm = TRUE),
                                                                        pct_byindiv_over30_new_sensiHALL=weighted.mean(bmiover30_byindiv_new_sensiHALL,weight,na.rm = TRUE),
                                                                        pct_byindiv_NA_new_sensiHALL=weighted.mean(bmiNA_byindiv_new_sensiHALL,weight,na.rm = TRUE)),by = c("Groups_red","Year","scenario")]
  
  pctcalcul_sensi <- setDT(datacc1)[!is.na(bmi_byindiv_new_sensi),][, .(pct_byindiv_under20_new_sensi=weighted.mean(bmiunder20_byindiv_new_sensi,weight,na.rm = TRUE),
                                                                        pct_byindiv_2025_new_sensi=weighted.mean(bmi2025_byindiv_new_sensi,weight,na.rm = TRUE),
                                                                        pct_byindiv_2530_new_sensi=weighted.mean(bmi2530_byindiv_new_sensi,weight,na.rm = TRUE),
                                                                        pct_byindiv_over30_new_sensi=weighted.mean(bmiover30_byindiv_new_sensi,weight,na.rm = TRUE),
                                                                        pct_byindiv_NA_new_sensi=weighted.mean(bmiNA_byindiv_new_sensi,weight,na.rm = TRUE)),by = c("Groups_red","Year","scenario")]
  
  pctcalcul_sensi_day <- setDT(datacc1)[!is.na(bmi_byindiv_new_sensi_day),][, .(pct_byindiv_under20_new_sensi_day=weighted.mean(bmiunder20_byindiv_new_sensi_day,weight,na.rm = TRUE),
                                                                        pct_byindiv_2025_new_sensi_day=weighted.mean(bmi2025_byindiv_new_sensi_day,weight,na.rm = TRUE),
                                                                        pct_byindiv_2530_new_sensi_day=weighted.mean(bmi2530_byindiv_new_sensi_day,weight,na.rm = TRUE),
                                                                        pct_byindiv_over30_new_sensi_day=weighted.mean(bmiover30_byindiv_new_sensi_day,weight,na.rm = TRUE),
                                                                        pct_byindiv_NA_new_sensi_day=weighted.mean(bmiNA_byindiv_new_sensi_day,weight,na.rm = TRUE)),by = c("Groups_red","Year","scenario")]
  gc()
  lop <- c("bmi_byindiv_pred", "bmi_byindiv_new","bmi_byindiv_new_year","bmi_byindiv_new_sensiHALL","bmi_byindiv_new_sensi","bmi_byindiv_new_sensi_day","dif.nrj","dif.nrj_day","dif.nrj_year","dif.vgt")
  meanbmichange <- datacc1[, lapply(.SD,weighted.mean,w=weight,na.rm = TRUE), by=c("Groups_red", "Year","scenario"),.SDcols =lop]

  meanbmichangebyBMI1<- datacc1[, .(Groups_red,Year,scenario,bmi_byindiv_new,weight,
                                    bmiunder20_byindiv_new,bmi2025_byindiv_new,bmi2530_byindiv_new,bmiover30_byindiv_new,bmiNA_byindiv_new)][,
              bmicateg:=case_when(bmiunder20_byindiv_new=="1" ~ "bmi_byindiv_under20_new",
                                  bmi2025_byindiv_new=="1" ~ "bmi_byindiv_2025_new",
                                  bmi2530_byindiv_new=="1" ~ "bmi_byindiv_2530_new",
                                  bmiover30_byindiv_new=="1" ~ "bmi_byindiv_over30_new",
                                  bmiNA_byindiv_new=="1" ~ "bmi_byindiv_NA_new")][,
             lapply(.SD,weighted.mean,w=weight,na.rm = TRUE),by=c("Groups_red","Year","scenario","bmicateg"),.SDcols="bmi_byindiv_new"]
  meanbmichangebyBMI22 <- dcast(meanbmichangebyBMI1, Groups_red + Year + scenario ~ bmicateg, value.var = "bmi_byindiv_new")
  meanbmichangebyBMI <- subset(meanbmichangebyBMI22,select=c("Groups_red","Year","scenario","bmi_byindiv_2025_new", "bmi_byindiv_2530_new",
                                                             "bmi_byindiv_over30_new","bmi_byindiv_under20_new"))
  rm(meanbmichangebyBMI1,meanbmichangebyBMI22)
  
  meanbmichangebyBMI_year1 <- datacc1[, .(Groups_red,Year,scenario,bmi_byindiv_new_year,weight,
                                         bmiunder20_byindiv_new_year,bmi2025_byindiv_new_year,bmi2530_byindiv_new_year,bmiover30_byindiv_new_year,bmiNA_byindiv_new_year)][,
             bmicateg_year:=case_when(bmiunder20_byindiv_new_year=="1" ~ "bmi_byindiv_under20_new_year",
                                      bmi2025_byindiv_new_year=="1" ~ "bmi_byindiv_2025_new_year",
                                      bmi2530_byindiv_new_year=="1" ~ "bmi_byindiv_2530_new_year",
                                      bmiover30_byindiv_new_year=="1" ~ "bmi_byindiv_over30_new_year",
                                      bmiNA_byindiv_new_year=="1" ~ "bmi_byindiv_NA_new_year")][,
            lapply(.SD,weighted.mean,w=weight,na.rm = TRUE),by=c("Groups_red","Year","scenario","bmicateg_year"),.SDcols="bmi_byindiv_new_year"]
  meanbmichangebyBMI_year22 <- dcast(meanbmichangebyBMI_year1, Groups_red + Year + scenario ~ bmicateg_year, value.var = "bmi_byindiv_new_year")
  meanbmichangebyBMI_year <- subset(meanbmichangebyBMI_year22,select=c("Groups_red","Year","scenario","bmi_byindiv_2025_new_year", 
                                                                       "bmi_byindiv_2530_new_year","bmi_byindiv_over30_new_year",
                                                                       "bmi_byindiv_under20_new_year"))
  
  rm(meanbmichangebyBMI_year1,meanbmichangebyBMI_year22)
  
  meanbmichangebyBMI_sensiHALL1 <- datacc1[, .(Groups_red,Year,scenario,bmi_byindiv_new_sensiHALL,weight,
                                           bmiunder20_byindiv_new_sensiHALL,bmi2025_byindiv_new_sensiHALL,
                                           bmi2530_byindiv_new_sensiHALL,bmiover30_byindiv_new_sensiHALL,bmiNA_byindiv_new_sensiHALL)][,
                                           bmicateg_sensiHALL:=case_when(bmiunder20_byindiv_new_sensiHALL=="1" ~ "bmi_byindiv_under20_new_sensiHALL",
                                                                         bmi2025_byindiv_new_sensiHALL=="1" ~ "bmi_byindiv_2025_new_sensiHALL",
                                                                         bmi2530_byindiv_new_sensiHALL=="1" ~ "bmi_byindiv_2530_new_sensiHALL",
                                                                         bmiover30_byindiv_new_sensiHALL=="1" ~ "bmi_byindiv_over30_new_sensiHALL",
                                                                         bmiNA_byindiv_new_sensiHALL=="1" ~ "bmi_byindiv_NA_new_sensiHALL")][,
                                          lapply(.SD,weighted.mean,w=weight,na.rm = TRUE),by=c("Groups_red","Year","scenario","bmicateg_sensiHALL"),.SDcols="bmi_byindiv_new_sensiHALL"]
  meanbmichangebyBMI_sensiHALL22 <- dcast(meanbmichangebyBMI_sensiHALL1, Groups_red + Year + scenario ~ bmicateg_sensiHALL, value.var = "bmi_byindiv_new_sensiHALL")
  meanbmichangebyBMI_sensiHALL <- subset(meanbmichangebyBMI_sensiHALL22,select=c("Groups_red","Year","scenario","bmi_byindiv_2025_new_sensiHALL", 
                                                                         "bmi_byindiv_2530_new_sensiHALL","bmi_byindiv_over30_new_sensiHALL",
                                                                         "bmi_byindiv_under20_new_sensiHALL"))
  rm(meanbmichangebyBMI_sensiHALL1,meanbmichangebyBMI_sensiHALL22)
  
  meanbmichangebyBMI_sensi1 <- datacc1[, .(Groups_red,Year,scenario,bmi_byindiv_new_sensi,weight,
                                           bmiunder20_byindiv_new_sensi,bmi2025_byindiv_new_sensi,bmi2530_byindiv_new_sensi,bmiover30_byindiv_new_sensi,bmiNA_byindiv_new_sensi)][,
                                          bmicateg_sensi:=case_when(bmiunder20_byindiv_new_sensi=="1" ~ "bmi_byindiv_under20_new_sensi",
                                                                    bmi2025_byindiv_new_sensi=="1" ~ "bmi_byindiv_2025_new_sensi",
                                                                    bmi2530_byindiv_new_sensi=="1" ~ "bmi_byindiv_2530_new_sensi",
                                                                    bmiover30_byindiv_new_sensi=="1" ~ "bmi_byindiv_over30_new_sensi",
                                                                    bmiNA_byindiv_new_sensi=="1" ~ "bmi_byindiv_NA_new_sensi")][,
                                         lapply(.SD,weighted.mean,w=weight,na.rm = TRUE),by=c("Groups_red","Year","scenario","bmicateg_sensi"),.SDcols="bmi_byindiv_new_sensi"]
  meanbmichangebyBMI_sensi22 <- dcast(meanbmichangebyBMI_sensi1, Groups_red + Year + scenario ~ bmicateg_sensi, value.var = "bmi_byindiv_new_sensi")
  meanbmichangebyBMI_sensi <- subset(meanbmichangebyBMI_sensi22,select=c("Groups_red","Year","scenario","bmi_byindiv_2025_new_sensi", 
                                                                       "bmi_byindiv_2530_new_sensi","bmi_byindiv_over30_new_sensi",
                                                                       "bmi_byindiv_under20_new_sensi"))
  rm(meanbmichangebyBMI_sensi1,meanbmichangebyBMI_sensi22)
  
  meanbmichangebyBMI_sensi_day1 <- datacc1[, .(Groups_red,Year,scenario,bmi_byindiv_new_sensi_day,weight,
                                           bmiunder20_byindiv_new_sensi_day,bmi2025_byindiv_new_sensi_day,bmi2530_byindiv_new_sensi_day,bmiover30_byindiv_new_sensi_day,bmiNA_byindiv_new_sensi_day)][,
                                           bmicateg_sensi_day:=case_when(bmiunder20_byindiv_new_sensi_day=="1" ~ "bmi_byindiv_under20_new_sensi_day",
                                                                         bmi2025_byindiv_new_sensi_day=="1" ~ "bmi_byindiv_2025_new_sensi_day",
                                                                         bmi2530_byindiv_new_sensi_day=="1" ~ "bmi_byindiv_2530_new_sensi_day",
                                                                         bmiover30_byindiv_new_sensi_day=="1" ~ "bmi_byindiv_over30_new_sensi_day",
                                                                         bmiNA_byindiv_new_sensi_day=="1" ~ "bmi_byindiv_NA_new_sensi_day")][,
                                        lapply(.SD,weighted.mean,w=weight,na.rm = TRUE),by=c("Groups_red","Year","scenario","bmicateg_sensi_day"),.SDcols="bmi_byindiv_new_sensi_day"]
  meanbmichangebyBMI_sensi_day22 <- dcast(meanbmichangebyBMI_sensi_day1, Groups_red + Year + scenario ~ bmicateg_sensi_day, value.var = "bmi_byindiv_new_sensi_day")
  meanbmichangebyBMI_sensi_day <- subset(meanbmichangebyBMI_sensi_day22,select=c("Groups_red","Year","scenario","bmi_byindiv_2025_new_sensi_day", 
                                                                         "bmi_byindiv_2530_new_sensi_day","bmi_byindiv_over30_new_sensi_day",
                                                                         "bmi_byindiv_under20_new_sensi_day"))
  rm(meanbmichangebyBMI_sensi_day1,meanbmichangebyBMI_sensi_day22)
  
  
  gc()
  #link with mortality data
  #mortality data
  datacc011 <- setDT(databb)[, `:=` (Groups=paste(Country, Sex, AgeGroups2),
                              Groups_red=paste(Country, Sex, AgeGroups2,IMD_quintile),
                              IMD_quintile=as.numeric(IMD_quintile))][,!c("Country", "Sex", "AgeGroups2","Groups","IMD_quintile")]
  
  datacc0 <- datacc011 %>% left_join(effect, by=c("Groups_red"),multiple = "all") %>%
                             left_join(pob2020_red_ok, by=c("Groups")) %>%
                             left_join(memean, by=c("Year","IMD_quintile")) 
  rm(datacc011)
  
  #link
  datacc2 <- datacc0 %>% merge(pctcalcul, by=c("Groups_red","Year","scenario")) %>% 
                         merge(pctcalcul_year, by=c("Groups_red","Year","scenario"))  %>% 
                         merge(pctcalcul_sensiHALL, by=c("Groups_red","Year","scenario"))  %>% 
                         merge(pctcalcul_sensi, by=c("Groups_red","Year","scenario"))  %>% 
                         merge(pctcalcul_sensi_day, by=c("Groups_red","Year","scenario"))  %>% 
                         merge(meanbmichange, by=c("Groups_red","Year","scenario"))  %>% 
                         merge(meanbmichangebyBMI, by=c("Groups_red","Year","scenario")) %>%
                         merge(meanbmichangebyBMI_year, by=c("Groups_red","Year","scenario")) %>%
                         merge(meanbmichangebyBMI_sensiHALL, by=c("Groups_red","Year","scenario"))%>%
                         merge(meanbmichangebyBMI_sensi, by=c("Groups_red","Year","scenario"))%>%
                         merge(meanbmichangebyBMI_sensi_day, by=c("Groups_red","Year","scenario"))

  #here I put 2021 as the baseline as it's the final year without the intervention
  baseline <- datacc2[Year==2021,][, `:=` (bmi_byindiv_under20_2021=bmi_byindiv_under20_new,
                                                        bmi_byindiv_2025_2021=bmi_byindiv_2025_new,
                                                        bmi_byindiv_2530_2021=bmi_byindiv_2530_new,
                                                        bmi_byindiv_over30_2021=bmi_byindiv_over30_new,
                                                        
                                                        pct_byindiv_under20_2021=pct_byindiv_under20_new,
                                                        pct_byindiv_2025_2021=pct_byindiv_2025_new,
                                                        pct_byindiv_2530_2021=pct_byindiv_2530_new,
                                                        pct_byindiv_over30_2021=pct_byindiv_over30_new)]
  baseline <- subset(baseline, select=c(Groups_red,scenario,bmi_byindiv_under20_2021:pct_byindiv_over30_2021))
  
  datacc3 <- merge(datacc2,baseline, by=c("Groups_red","scenario"))[, `:=` (
      #Calculating the RR by group
      #for participant with BMI<20 kg/m2
      rr.chd_Obesity_under20=1,
      rr.isch_Obesity_under20=1,
      rr.haem_Obesity_under20=1,
      
      #for participant with BMI between 20-25 kg/m2 
      #baseline
      rr.chd_Obesity_2025_2021=rr.chd_Obesity^((bmi_byindiv_2025_2021-20)/4.56),  #BMIss bring back to 4.56 to be comparative with rr.Obesity
      rr.isch_Obesity_2025_2021=rr.isch_Obesity^((bmi_byindiv_2025_2021-20)/4.56),
      rr.haem_Obesity_2025_2021=rr.haem_Obesity^((bmi_byindiv_2025_2021-20)/4.56),
      #new
      rr.chd_Obesity_new2025=rr.chd_Obesity^((bmi_byindiv_2025_new-20)/4.56),
      rr.isch_Obesity_new2025=rr.isch_Obesity^((bmi_byindiv_2025_new-20)/4.56),
      rr.haem_Obesity_new2025=rr.haem_Obesity^((bmi_byindiv_2025_new-20)/4.56),
      #year
      rr.chd_Obesity_new2025_year=rr.chd_Obesity^((bmi_byindiv_2025_new_year-20)/4.56),
      rr.isch_Obesity_new2025_year=rr.isch_Obesity^((bmi_byindiv_2025_new_year-20)/4.56),
      rr.haem_Obesity_new2025_year=rr.haem_Obesity^((bmi_byindiv_2025_new_year-20)/4.56),
      #sensiHALL
      rr.chd_Obesity_new2025_sensiHALL=rr.chd_Obesity^((bmi_byindiv_2025_new_sensiHALL-20)/4.56),
      rr.isch_Obesity_new2025_sensiHALL=rr.isch_Obesity^((bmi_byindiv_2025_new_sensiHALL-20)/4.56),
      rr.haem_Obesity_new2025_sensiHALL=rr.haem_Obesity^((bmi_byindiv_2025_new_sensiHALL-20)/4.56),
      #sensi
      rr.chd_Obesity_new2025_sensi=rr.chd_Obesity^((bmi_byindiv_2025_new_sensi-20)/4.56),
      rr.isch_Obesity_new2025_sensi=rr.isch_Obesity^((bmi_byindiv_2025_new_sensi-20)/4.56),
      rr.haem_Obesity_new2025_sensi=rr.haem_Obesity^((bmi_byindiv_2025_new_sensi-20)/4.56),
      #sensi day
      rr.chd_Obesity_new2025_sensi_day=rr.chd_Obesity^((bmi_byindiv_2025_new_sensi_day-20)/4.56),
      rr.isch_Obesity_new2025_sensi_day=rr.isch_Obesity^((bmi_byindiv_2025_new_sensi_day-20)/4.56),
      rr.haem_Obesity_new2025_sensi_day=rr.haem_Obesity^((bmi_byindiv_2025_new_sensi_day-20)/4.56),
      
      #for participant with BMI between 25-30 kg/m2 
      #baseline
      rr.chd_Obesity_2530_2021=rr.chd_Obesity^((bmi_byindiv_2530_2021-20)/4.56),  #BMIss bring back to 4.56 to be comparative with rr.Obesity
      rr.isch_Obesity_2530_2021=rr.isch_Obesity^((bmi_byindiv_2530_2021-20)/4.56),
      rr.haem_Obesity_2530_2021=rr.haem_Obesity^((bmi_byindiv_2530_2021-20)/4.56),
      #new
      rr.chd_Obesity_new2530=rr.chd_Obesity^((bmi_byindiv_2530_new-20)/4.56),
      rr.isch_Obesity_new2530=rr.isch_Obesity^((bmi_byindiv_2530_new-20)/4.56),
      rr.haem_Obesity_new2530=rr.haem_Obesity^((bmi_byindiv_2530_new-20)/4.56),
      #year
      rr.chd_Obesity_new2530_year=rr.chd_Obesity^((bmi_byindiv_2530_new_year-20)/4.56),
      rr.isch_Obesity_new2530_year=rr.isch_Obesity^((bmi_byindiv_2530_new_year-20)/4.56),
      rr.haem_Obesity_new2530_year=rr.haem_Obesity^((bmi_byindiv_2530_new_year-20)/4.56),
      #sensiHALL
      rr.chd_Obesity_new2530_sensiHALL=rr.chd_Obesity^((bmi_byindiv_2530_new_sensiHALL-20)/4.56),
      rr.isch_Obesity_new2530_sensiHALL=rr.isch_Obesity^((bmi_byindiv_2530_new_sensiHALL-20)/4.56),
      rr.haem_Obesity_new2530_sensiHALL=rr.haem_Obesity^((bmi_byindiv_2530_new_sensiHALL-20)/4.56),
      #sensi
      rr.chd_Obesity_new2530_sensi=rr.chd_Obesity^((bmi_byindiv_2530_new_sensi-20)/4.56),
      rr.isch_Obesity_new2530_sensi=rr.isch_Obesity^((bmi_byindiv_2530_new_sensi-20)/4.56),
      rr.haem_Obesity_new2530_sensi=rr.haem_Obesity^((bmi_byindiv_2530_new_sensi-20)/4.56),
      
      rr.chd_Obesity_new2530_sensi_day=rr.chd_Obesity^((bmi_byindiv_2530_new_sensi_day-20)/4.56),
      rr.isch_Obesity_new2530_sensi_day=rr.isch_Obesity^((bmi_byindiv_2530_new_sensi_day-20)/4.56),
      rr.haem_Obesity_new2530_sensi_day=rr.haem_Obesity^((bmi_byindiv_2530_new_sensi_day-20)/4.56),
      
      #for participant with BMI >= 30 kg/m2 
      #baseline
      rr.chd_Obesity_over30_2021=rr.chd_Obesity^((bmi_byindiv_over30_2021-20)/4.56),  #BMIss bring back to 4.56 to be comparative with rr.Obesity
      rr.isch_Obesity_over30_2021=rr.isch_Obesity^((bmi_byindiv_over30_2021-20)/4.56),
      rr.haem_Obesity_over30_2021=rr.haem_Obesity^((bmi_byindiv_over30_2021-20)/4.56),
      #new
      rr.chd_Obesity_newover30=rr.chd_Obesity^((bmi_byindiv_over30_new-20)/4.56),
      rr.isch_Obesity_newover30=rr.isch_Obesity^((bmi_byindiv_over30_new-20)/4.56),
      rr.haem_Obesity_newover30=rr.haem_Obesity^((bmi_byindiv_over30_new-20)/4.56),
      
      #year
      rr.chd_Obesity_newover30_year=rr.chd_Obesity^((bmi_byindiv_over30_new_year-20)/4.56),
      rr.isch_Obesity_newover30_year=rr.isch_Obesity^((bmi_byindiv_over30_new_year-20)/4.56),
      rr.haem_Obesity_newover30_year=rr.haem_Obesity^((bmi_byindiv_over30_new_year-20)/4.56),
      
      #sensiHALL
      rr.chd_Obesity_newover30_sensiHALL=rr.chd_Obesity^((bmi_byindiv_over30_new_sensiHALL-20)/4.56),
      rr.isch_Obesity_newover30_sensiHALL=rr.isch_Obesity^((bmi_byindiv_over30_new_sensiHALL-20)/4.56),
      rr.haem_Obesity_newover30_sensiHALL=rr.haem_Obesity^((bmi_byindiv_over30_new_sensiHALL-20)/4.56),
      
      #sensi
      rr.chd_Obesity_newover30_sensi=rr.chd_Obesity^((bmi_byindiv_over30_new_sensi-20)/4.56),
      rr.isch_Obesity_newover30_sensi=rr.isch_Obesity^((bmi_byindiv_over30_new_sensi-20)/4.56),
      rr.haem_Obesity_newover30_sensi=rr.haem_Obesity^((bmi_byindiv_over30_new_sensi-20)/4.56),
  
      #sensi
      rr.chd_Obesity_newover30_sensi_day=rr.chd_Obesity^((bmi_byindiv_over30_new_sensi_day-20)/4.56),
      rr.isch_Obesity_newover30_sensi_day=rr.isch_Obesity^((bmi_byindiv_over30_new_sensi_day-20)/4.56),
      rr.haem_Obesity_newover30_sensi_day=rr.haem_Obesity^((bmi_byindiv_over30_new_sensi_day-20)/4.56))]
  
  rm(pob2020_red_ok,pctcalcul,baseline,baselineanthro,effect,meanbmichange,meanbmichangebyBMI,meanbmichangebyBMI_year,meanbmichangebyBMI_sensiHALL,
     meanbmichangebyBMI_sensi,meanbmichangebyBMI_sensi_day,memean,
     pctcalcul_year,pctcalcul_sensi,pctcalcul_sensi_day)
  
  #we assume a lag time of 6 years (ERFC 2011: 5.7 years [SD 3.0–9.0])) 
  #to be simpler, we will mismatch the year in purpose
  nb2 <- datacc3 %>% mutate(c=1) %>% group_by(Groups_red,Year,scenario) %>% 
    mutate(p_lag_under20=pct_byindiv_under20_new,
           p_lag_2025=pct_byindiv_2025_new,
           p_lag_2530=pct_byindiv_2530_new,
           p_lag_over30=pct_byindiv_over30_new,
           
           lag_rr.chd_Obesity_2025=rr.chd_Obesity_new2025,
           lag_rr.isch_Obesity_2025=rr.isch_Obesity_new2025,
           lag_rr.haem_Obesity_2025=rr.haem_Obesity_new2025,
           
           lag_rr.chd_Obesity_2530=rr.chd_Obesity_new2530,
           lag_rr.isch_Obesity_2530=rr.isch_Obesity_new2530,
           lag_rr.haem_Obesity_2530=rr.haem_Obesity_new2530,
           
           lag_rr.chd_Obesity_over30=rr.chd_Obesity_newover30,
           lag_rr.isch_Obesity_over30=rr.isch_Obesity_newover30,
           lag_rr.haem_Obesity_over30=rr.haem_Obesity_newover30,
           
           #year
           p_lag_under20_year=pct_byindiv_under20_new_year,
           p_lag_2025_year=pct_byindiv_2025_new_year,
           p_lag_2530_year=pct_byindiv_2530_new_year,
           p_lag_over30_year=pct_byindiv_over30_new_year,
           
           lag_rr.chd_Obesity_2025_year=rr.chd_Obesity_new2025_year,
           lag_rr.isch_Obesity_2025_year=rr.isch_Obesity_new2025_year,
           lag_rr.haem_Obesity_2025_year=rr.haem_Obesity_new2025_year,
           
           lag_rr.chd_Obesity_2530_year=rr.chd_Obesity_new2530_year,
           lag_rr.isch_Obesity_2530_year=rr.isch_Obesity_new2530_year,
           lag_rr.haem_Obesity_2530_year=rr.haem_Obesity_new2530_year,
           
           lag_rr.chd_Obesity_over30_year=rr.chd_Obesity_newover30_year,
           lag_rr.isch_Obesity_over30_year=rr.isch_Obesity_newover30_year,
           lag_rr.haem_Obesity_over30_year=rr.haem_Obesity_newover30_year,
           
           #sensi
           p_lag_under20_sensiHALL=pct_byindiv_under20_new_sensiHALL,
           p_lag_2025_sensiHALL=pct_byindiv_2025_new_sensiHALL,
           p_lag_2530_sensiHALL=pct_byindiv_2530_new_sensiHALL,
           p_lag_over30_sensiHALL=pct_byindiv_over30_new_sensiHALL,
           
           lag_rr.chd_Obesity_2025_sensiHALL=rr.chd_Obesity_new2025_sensiHALL,
           lag_rr.isch_Obesity_2025_sensiHALL=rr.isch_Obesity_new2025_sensiHALL,
           lag_rr.haem_Obesity_2025_sensiHALL=rr.haem_Obesity_new2025_sensiHALL,
           
           lag_rr.chd_Obesity_2530_sensiHALL=rr.chd_Obesity_new2530_sensiHALL,
           lag_rr.isch_Obesity_2530_sensiHALL=rr.isch_Obesity_new2530_sensiHALL,
           lag_rr.haem_Obesity_2530_sensiHALL=rr.haem_Obesity_new2530_sensiHALL,
           
           lag_rr.chd_Obesity_over30_sensiHALL=rr.chd_Obesity_newover30_sensiHALL,
           lag_rr.isch_Obesity_over30_sensiHALL=rr.isch_Obesity_newover30_sensiHALL,
           lag_rr.haem_Obesity_over30_sensiHALL=rr.haem_Obesity_newover30_sensiHALL,
           
           #sensi
           p_lag_under20_sensi=pct_byindiv_under20_new_sensi,
           p_lag_2025_sensi=pct_byindiv_2025_new_sensi,
           p_lag_2530_sensi=pct_byindiv_2530_new_sensi,
           p_lag_over30_sensi=pct_byindiv_over30_new_sensi,
           
           lag_rr.chd_Obesity_2025_sensi=rr.chd_Obesity_new2025_sensi,
           lag_rr.isch_Obesity_2025_sensi=rr.isch_Obesity_new2025_sensi,
           lag_rr.haem_Obesity_2025_sensi=rr.haem_Obesity_new2025_sensi,
           
           lag_rr.chd_Obesity_2530_sensi=rr.chd_Obesity_new2530_sensi,
           lag_rr.isch_Obesity_2530_sensi=rr.isch_Obesity_new2530_sensi,
           lag_rr.haem_Obesity_2530_sensi=rr.haem_Obesity_new2530_sensi,
           
           lag_rr.chd_Obesity_over30_sensi=rr.chd_Obesity_newover30_sensi,
           lag_rr.isch_Obesity_over30_sensi=rr.isch_Obesity_newover30_sensi,
           lag_rr.haem_Obesity_over30_sensi=rr.haem_Obesity_newover30_sensi,
           
           #sensi day
           p_lag_under20_sensi_day=pct_byindiv_under20_new_sensi_day,
           p_lag_2025_sensi_day=pct_byindiv_2025_new_sensi_day,
           p_lag_2530_sensi_day=pct_byindiv_2530_new_sensi_day,
           p_lag_over30_sensi_day=pct_byindiv_over30_new_sensi_day,
           
           lag_rr.chd_Obesity_2025_sensi_day=rr.chd_Obesity_new2025_sensi_day,
           lag_rr.isch_Obesity_2025_sensi_day=rr.isch_Obesity_new2025_sensi_day,
           lag_rr.haem_Obesity_2025_sensi_day=rr.haem_Obesity_new2025_sensi_day,
           
           lag_rr.chd_Obesity_2530_sensi_day=rr.chd_Obesity_new2530_sensi_day,
           lag_rr.isch_Obesity_2530_sensi_day=rr.isch_Obesity_new2530_sensi_day,
           lag_rr.haem_Obesity_2530_sensi_day=rr.haem_Obesity_new2530_sensi_day,
           
           lag_rr.chd_Obesity_over30_sensi_day=rr.chd_Obesity_newover30_sensi_day,
           lag_rr.isch_Obesity_over30_sensi_day=rr.isch_Obesity_newover30_sensi_day,
           lag_rr.haem_Obesity_over30_sensi_day=rr.haem_Obesity_newover30_sensi_day,
           
           Year=Year+6) %>% dplyr::select(Groups_red,Year,scenario,p_lag_under20:lag_rr.haem_Obesity_over30_sensi_day)
  
  
  
  dataccxx <- datacc3 %>% left_join(nb2, by=c("Groups_red","Year","scenario")) 
  
  dataccxx2 <- setDT(dataccxx)[, `:=` (
      # In people with BMI of 20 kg/m² or higher (according to the ref ERFC 2011) #
      # so we need to use Potentioal Impact Fraction (PIF) (formula in "Zapata-Diomedi-Population attributable fraction: names, types and issues with incorrect interpretation of relative risks"
      # with p=proportion of source population exposed to the factor of interest / here BMI>20
      
      # as we don't have a BMI trend, ((prop of under 20*1)+(prop of g1*RRg1 + etc)) DONT CHANGE 
      # if we had a trend, i will have to calculate 2 pif: ONE for the baseline as I do, and one with the "constant" part that will change every year based on the previous one.
      PIF_chd=(((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021))-
                 ((p_lag_under20*rr.chd_Obesity_under20)+(p_lag_2025*lag_rr.chd_Obesity_2025)+(p_lag_2530*lag_rr.chd_Obesity_2530)+(p_lag_over30*lag_rr.chd_Obesity_over30)))/
        ((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021)),
      
      PIF_isch=(((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021))-
                  ((p_lag_under20*rr.isch_Obesity_under20)+(p_lag_2025*lag_rr.isch_Obesity_2025)+(p_lag_2530*lag_rr.isch_Obesity_2530)+(p_lag_over30*lag_rr.isch_Obesity_over30)))/
        ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021)),
      
      PIF_haem=(((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021))-
                  ((p_lag_under20*rr.haem_Obesity_under20)+(p_lag_2025*lag_rr.haem_Obesity_2025)+(p_lag_2530*lag_rr.haem_Obesity_2530)+(p_lag_over30*lag_rr.haem_Obesity_over30)))/
        ((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021)),
      
      #year
      PIF_chd_year=(((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021))-
                 ((p_lag_under20_year*rr.chd_Obesity_under20)+(p_lag_2025_year*lag_rr.chd_Obesity_2025_year)+(p_lag_2530_year*lag_rr.chd_Obesity_2530_year)+(p_lag_over30_year*lag_rr.chd_Obesity_over30_year)))/
        ((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021)),
      
      PIF_isch_year=(((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021))-
                  ((p_lag_under20_year*rr.isch_Obesity_under20)+(p_lag_2025_year*lag_rr.isch_Obesity_2025_year)+(p_lag_2530_year*lag_rr.isch_Obesity_2530_year)+(p_lag_over30_year*lag_rr.isch_Obesity_over30_year)))/
        ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021)),
      
      PIF_haem_year=(((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021))-
                  ((p_lag_under20_year*rr.haem_Obesity_under20)+(p_lag_2025_year*lag_rr.haem_Obesity_2025_year)+(p_lag_2530_year*lag_rr.haem_Obesity_2530_year)+(p_lag_over30_year*lag_rr.haem_Obesity_over30_year)))/
        ((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021)),
      
      #sensiHALL
      PIF_chd_sensiHALL=(((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021))-
                       ((p_lag_under20_sensiHALL*rr.chd_Obesity_under20)+(p_lag_2025_sensiHALL*lag_rr.chd_Obesity_2025_sensiHALL)+(p_lag_2530_sensiHALL*lag_rr.chd_Obesity_2530_sensiHALL)+(p_lag_over30_sensiHALL*lag_rr.chd_Obesity_over30_sensiHALL)))/
        ((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021)),
      
      PIF_isch_sensiHALL=(((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021))-
                        ((p_lag_under20_sensiHALL*rr.isch_Obesity_under20)+(p_lag_2025_sensiHALL*lag_rr.isch_Obesity_2025_sensiHALL)+(p_lag_2530_sensiHALL*lag_rr.isch_Obesity_2530_sensiHALL)+(p_lag_over30_sensiHALL*lag_rr.isch_Obesity_over30_sensiHALL)))/
        ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021)),
      
      PIF_haem_sensiHALL=(((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021))-
                        ((p_lag_under20_sensiHALL*rr.haem_Obesity_under20)+(p_lag_2025_sensiHALL*lag_rr.haem_Obesity_2025_sensiHALL)+(p_lag_2530_sensiHALL*lag_rr.haem_Obesity_2530_sensiHALL)+(p_lag_over30_sensiHALL*lag_rr.haem_Obesity_over30_sensiHALL)))/
        ((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021)),
      
      #sensi
      PIF_chd_sensi=(((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021))-
                      ((p_lag_under20_sensi*rr.chd_Obesity_under20)+(p_lag_2025_sensi*lag_rr.chd_Obesity_2025_sensi)+(p_lag_2530_sensi*lag_rr.chd_Obesity_2530_sensi)+(p_lag_over30_sensi*lag_rr.chd_Obesity_over30_sensi)))/
        ((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021)),
      
      PIF_isch_sensi=(((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021))-
                       ((p_lag_under20_sensi*rr.isch_Obesity_under20)+(p_lag_2025_sensi*lag_rr.isch_Obesity_2025_sensi)+(p_lag_2530_sensi*lag_rr.isch_Obesity_2530_sensi)+(p_lag_over30_sensi*lag_rr.isch_Obesity_over30_sensi)))/
        ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021)),
      
      PIF_haem_sensi=(((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021))-
                       ((p_lag_under20_sensi*rr.haem_Obesity_under20)+(p_lag_2025_sensi*lag_rr.haem_Obesity_2025_sensi)+(p_lag_2530_sensi*lag_rr.haem_Obesity_2530_sensi)+(p_lag_over30_sensi*lag_rr.haem_Obesity_over30_sensi)))/
        ((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021)),
      
          #sensi day
          PIF_chd_sensi_day=(((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021))-
                           ((p_lag_under20_sensi_day*rr.chd_Obesity_under20)+(p_lag_2025_sensi_day*lag_rr.chd_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*lag_rr.chd_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*lag_rr.chd_Obesity_over30_sensi_day)))/
            ((pct_byindiv_under20_2021*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.chd_Obesity_over30_2021)),
          
          PIF_isch_sensi_day=(((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021))-
                            ((p_lag_under20_sensi_day*rr.isch_Obesity_under20)+(p_lag_2025_sensi_day*lag_rr.isch_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*lag_rr.isch_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*lag_rr.isch_Obesity_over30_sensi_day)))/
            ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.isch_Obesity_over30_2021)),
          
          PIF_haem_sensi_day=(((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021))-
                            ((p_lag_under20_sensi_day*rr.haem_Obesity_under20)+(p_lag_2025_sensi_day*lag_rr.haem_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*lag_rr.haem_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*lag_rr.haem_Obesity_over30_sensi_day)))/
            ((pct_byindiv_under20_2021*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*rr.haem_Obesity_over30_2021)))][, `:=` (
              
              
              #TEST REVIWER 4 Miettinen FORMULA on sensi 55 rules
              PIF_chd_sensi_day_r4=(((pct_byindiv_under20_2021*0.86*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.chd_Obesity_over30_2021))-
                               ((p_lag_under20_sensi_day*0.86*rr.chd_Obesity_under20)+(p_lag_2025_sensi_day*0.86*lag_rr.chd_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*1.14*lag_rr.chd_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*1.14*lag_rr.chd_Obesity_over30_sensi_day)))/
                ((pct_byindiv_under20_2021*0.86*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.chd_Obesity_over30_2021)),
              
              PIF_isch_sensi_day_r4=(((pct_byindiv_under20_2021*0.86*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.isch_Obesity_over30_2021))-
                                ((p_lag_under20_sensi_day*0.86*rr.isch_Obesity_under20)+(p_lag_2025_sensi_day*0.86*lag_rr.isch_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*1.14*lag_rr.isch_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*1.14*lag_rr.isch_Obesity_over30_sensi_day)))/
                ((pct_byindiv_under20_2021*0.86*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.isch_Obesity_over30_2021)),
              
              PIF_haem_sensi_day_r4=(((pct_byindiv_under20_2021*0.86*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.haem_Obesity_over30_2021))-
                                ((p_lag_under20_sensi_day*0.86*rr.haem_Obesity_under20)+(p_lag_2025_sensi_day*0.86*lag_rr.haem_Obesity_2025_sensi_day)+(p_lag_2530_sensi_day*1.14*lag_rr.haem_Obesity_2530_sensi_day)+(p_lag_over30_sensi_day*1.14*lag_rr.haem_Obesity_over30_sensi_day)))/
                ((pct_byindiv_under20_2021*0.86*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.haem_Obesity_over30_2021)),
              
              
      #TEST REVIWER 4 Miettinen FORMULA
      PIF_chd_r4=(((pct_byindiv_under20_2021*0.86*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.chd_Obesity_over30_2021))-
                       ((p_lag_under20*0.86*rr.chd_Obesity_under20)+(p_lag_2025*0.86*lag_rr.chd_Obesity_2025)+(p_lag_2530*1.14*lag_rr.chd_Obesity_2530)+(p_lag_over30*1.14*lag_rr.chd_Obesity_over30)))/
        ((pct_byindiv_under20_2021*0.86*rr.chd_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.chd_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.chd_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.chd_Obesity_over30_2021)),
      
      PIF_isch_r4=(((pct_byindiv_under20_2021*0.86*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.isch_Obesity_over30_2021))-
                        ((p_lag_under20*0.86*rr.isch_Obesity_under20)+(p_lag_2025*0.86*lag_rr.isch_Obesity_2025)+(p_lag_2530*1.14*lag_rr.isch_Obesity_2530)+(p_lag_over30*1.14*lag_rr.isch_Obesity_over30)))/
        ((pct_byindiv_under20_2021*rr.isch_Obesity_under20)+(pct_byindiv_2025_2021*rr.isch_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.isch_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.isch_Obesity_over30_2021)),
      
      PIF_haem_r4=(((pct_byindiv_under20_2021*0.86*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.haem_Obesity_over30_2021))-
                        ((p_lag_under20*0.86*rr.haem_Obesity_under20)+(p_lag_2025*0.86*lag_rr.haem_Obesity_2025)+(p_lag_2530*1.14*lag_rr.haem_Obesity_2530)+(p_lag_over30*1.14*lag_rr.haem_Obesity_over30)))/
        ((pct_byindiv_under20_2021*0.86*rr.haem_Obesity_under20)+(pct_byindiv_2025_2021*0.86*rr.haem_Obesity_2025_2021)+(pct_byindiv_2530_2021*1.14*rr.haem_Obesity_2530_2021)+(pct_byindiv_over30_2021*1.14*rr.haem_Obesity_over30_2021)))]
  
  datacchi <- setDT(dataccxx2)[, `:=` (
      #we assume a lag time of 6 years (ERFC 2011: 5.7 years [SD 3.0–9.0])) but we have already mismatch the data earlier
      #we assume that the RR is constant
      chd.rate.nrj    = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ chd.rate * PIF_chd),
      isch.rate.nrj   = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ isch.rate * PIF_isch),
      haem.rate.nrj   = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ haem.rate * PIF_haem))][, `:=` (
      stroke.rate.nrj = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ isch.rate.nrj + haem.rate.nrj))][, `:=` (
      cvd.rate.nrj    = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ chd.rate.nrj + stroke.rate.nrj),
      #year
      chd.rate.nrj_year  = case_when(Year<2022  ~ 0,
                                    Year>=2022 ~ chd.rate * PIF_chd_year),
      isch.rate.nrj_year = case_when(Year<2022  ~ 0,
                                    Year>=2022 ~ isch.rate * PIF_isch_year),
      haem.rate.nrj_year   = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ haem.rate * PIF_haem_year))][, `:=` (
      stroke.rate.nrj_year = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ isch.rate.nrj_year + haem.rate.nrj_year))][, `:=` (
      cvd.rate.nrj_year    = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ chd.rate.nrj_year + stroke.rate.nrj_year),
      
      #sensiHALL
      chd.rate.nrj_sensiHALL  = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ chd.rate * PIF_chd_sensiHALL),
      isch.rate.nrj_sensiHALL = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ isch.rate * PIF_isch_sensiHALL),
      haem.rate.nrj_sensiHALL   = case_when(Year<2022  ~ 0,
                                        Year>=2022 ~ haem.rate * PIF_haem_sensiHALL))][, `:=` (
      stroke.rate.nrj_sensiHALL = case_when(Year<2022  ~ 0,
                                            Year>=2022 ~ isch.rate.nrj_sensiHALL + haem.rate.nrj_sensiHALL))][, `:=` (
      cvd.rate.nrj_sensiHALL    = case_when(Year<2022  ~ 0,
                                            Year>=2022 ~ chd.rate.nrj_sensiHALL + stroke.rate.nrj_sensiHALL))][, `:=` (
                                              
      #sensi
      chd.rate.nrj_sensi  = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ chd.rate * PIF_chd_sensi),
      isch.rate.nrj_sensi = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ isch.rate * PIF_isch_sensi),
      haem.rate.nrj_sensi   = case_when(Year<2022  ~ 0,
                                        Year>=2022 ~ haem.rate * PIF_haem_sensi))][, `:=` (
      stroke.rate.nrj_sensi = case_when(Year<2022  ~ 0,
                                       Year>=2022 ~ isch.rate.nrj_sensi + haem.rate.nrj_sensi))][, `:=` (
      cvd.rate.nrj_sensi    = case_when(Year<2022  ~ 0,
                                       Year>=2022 ~ chd.rate.nrj_sensi + stroke.rate.nrj_sensi))][, `:=` (
      
      #sensi
      chd.rate.nrj_sensi_day  = case_when(Year<2022  ~ 0,
                                     Year>=2022 ~ chd.rate * PIF_chd_sensi_day),
      isch.rate.nrj_sensi_day = case_when(Year<2022  ~ 0,
                                     Year>=2022 ~ isch.rate * PIF_isch_sensi_day),
      haem.rate.nrj_sensi_day   = case_when(Year<2022  ~ 0,
                                       Year>=2022 ~ haem.rate * PIF_haem_sensi_day))][, `:=` (
      stroke.rate.nrj_sensi_day = case_when(Year<2022  ~ 0,
                                       Year>=2022 ~ isch.rate.nrj_sensi_day + haem.rate.nrj_sensi_day))][, `:=` (
      cvd.rate.nrj_sensi_day    = case_when(Year<2022  ~ 0,
                                       Year>=2022 ~ chd.rate.nrj_sensi_day + stroke.rate.nrj_sensi_day))][, `:=` (
      
     #sensi REVIWER 4 Miettinen FORMULA
     chd.rate.nrj_sensi_day_r4  = case_when(Year<2022  ~ 0,
                                            Year>=2022 ~ chd.rate * PIF_chd_sensi_day_r4),
     isch.rate.nrj_sensi_day_r4 = case_when(Year<2022  ~ 0,
                                            Year>=2022 ~ isch.rate * PIF_isch_sensi_day_r4),
     haem.rate.nrj_sensi_day_r4   = case_when(Year<2022  ~ 0,
                                              Year>=2022 ~ haem.rate * PIF_haem_sensi_day_r4))][, `:=` (
     stroke.rate.nrj_sensi_day_r4 = case_when(Year<2022  ~ 0,
                                          Year>=2022 ~ isch.rate.nrj_sensi_day_r4 + haem.rate.nrj_sensi_day_r4))][, `:=` (
    cvd.rate.nrj_sensi_day_r4    = case_when(Year<2022  ~ 0,
                                         Year>=2022 ~ chd.rate.nrj_sensi_day_r4 + stroke.rate.nrj_sensi_day_r4))][, `:=` (
                                         
                                         
      #TEST REVIWER 4 Miettinen FORMULA
      chd.rate.nrj_r4  = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ chd.rate * PIF_chd_r4),
      isch.rate.nrj_r4 = case_when(Year<2022  ~ 0,
                                      Year>=2022 ~ isch.rate * PIF_isch_r4),
      ##ATTENTION ICI comme je n'ai pas les RR pour haem pour l'instant ca ne bouge pas
      haem.rate.nrj_r4  = case_when(Year<2022  ~ 0,
                                    Year>=2022 ~ haem.rate * PIF_haem_r4))][, `:=` (
      stroke.rate.nrj_r4 = case_when(Year<2022  ~ 0,
                                     Year>=2022 ~ isch.rate.nrj_r4 + haem.rate.nrj_r4))][, `:=` (
      cvd.rate.nrj_r4    = case_when(Year<2022  ~ 0,
                                     Year>=2022 ~ chd.rate.nrj_r4 + stroke.rate.nrj_r4),
      
      
      #dif.vgt to be comparative with rr.chd_Vegetables: talk about RR for 1 serving (100g/d)
      dif.vgt_comp=dif.vgt/100)][, `:=` (
      
      chd.rate.vgt    = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ chd.rate * rr.chd_Vegetables ^ dif.vgt_comp),
      isch.rate.vgt   = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ isch.rate * rr.isch_Vegetables ^ dif.vgt_comp),
      haem.rate.vgt   = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ haem.rate * rr.haem_Vegetables ^ dif.vgt_comp))][, `:=` (
      stroke.rate.vgt = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ isch.rate.vgt + haem.rate.vgt))][, `:=` (
      cvd.rate.vgt    = case_when(Year<2022  ~ 0,
                                  Year>=2022 ~ chd.rate.vgt + stroke.rate.vgt))][, `:=` (

      # Number of Imputable deaths from energy change
      n.cvd_imput.nrj    = cvd.rate.nrj * pop / 100000,
      n.chd_imput.nrj    = chd.rate.nrj * pop / 100000,
      n.stroke_imput.nrj = stroke.rate.nrj * pop / 100000,
      n.isch_imput.nrj   = isch.rate.nrj * pop / 100000,
      n.haem_imput.nrj   = haem.rate.nrj * pop / 100000,
      
      #year
      n.cvd_imput.nrj_year    = cvd.rate.nrj_year * pop / 100000,
      n.chd_imput.nrj_year    = chd.rate.nrj_year * pop / 100000,
      n.stroke_imput.nrj_year = stroke.rate.nrj_year * pop / 100000,
      n.isch_imput.nrj_year   = isch.rate.nrj_year * pop / 100000,
      n.haem_imput.nrj_year   = haem.rate.nrj_year * pop / 100000,
      
      #sensi
      n.cvd_imput.nrj_sensiHALL    = cvd.rate.nrj_sensiHALL * pop / 100000,
      n.chd_imput.nrj_sensiHALL    = chd.rate.nrj_sensiHALL * pop / 100000,
      n.stroke_imput.nrj_sensiHALL = stroke.rate.nrj_sensiHALL * pop / 100000,
      n.isch_imput.nrj_sensiHALL   = isch.rate.nrj_sensiHALL * pop / 100000,
      n.haem_imput.nrj_sensiHALL   = haem.rate.nrj_sensiHALL * pop / 100000,
      
      #sensi
      n.cvd_imput.nrj_sensi    = cvd.rate.nrj_sensi * pop / 100000,
      n.chd_imput.nrj_sensi    = chd.rate.nrj_sensi * pop / 100000,
      n.stroke_imput.nrj_sensi = stroke.rate.nrj_sensi * pop / 100000,
      n.isch_imput.nrj_sensi   = isch.rate.nrj_sensi * pop / 100000,
      n.haem_imput.nrj_sensi   = haem.rate.nrj_sensi * pop / 100000,
      
      #sensi day
      n.cvd_imput.nrj_sensi_day    = cvd.rate.nrj_sensi_day * pop / 100000,
      n.chd_imput.nrj_sensi_day    = chd.rate.nrj_sensi_day * pop / 100000,
      n.stroke_imput.nrj_sensi_day = stroke.rate.nrj_sensi_day * pop / 100000,
      n.isch_imput.nrj_sensi_day   = isch.rate.nrj_sensi_day * pop / 100000,
      n.haem_imput.nrj_sensi_day   = haem.rate.nrj_sensi_day * pop / 100000,
      
      #sensi day reviewer4 
      n.cvd_imput.nrj_sensi_day_r4    = cvd.rate.nrj_sensi_day_r4 * pop / 100000,
      n.chd_imput.nrj_sensi_day_r4    = chd.rate.nrj_sensi_day_r4 * pop / 100000,
      n.stroke_imput.nrj_sensi_day_r4 = stroke.rate.nrj_sensi_day_r4 * pop / 100000,
      n.isch_imput.nrj_sensi_day_r4   = isch.rate.nrj_sensi_day_r4 * pop / 100000,
      n.haem_imput.nrj_sensi_day_r4   = haem.rate.nrj_sensi_day_r4 * pop / 100000,
      
      #reviewer4 
      n.cvd_imput.nrj_r4    = cvd.rate.nrj_r4 * pop / 100000,
      n.chd_imput.nrj_r4    = chd.rate.nrj_r4 * pop / 100000,
      n.stroke_imput.nrj_r4 = stroke.rate.nrj_r4 * pop / 100000,
      n.isch_imput.nrj_r4   = isch.rate.nrj_r4 * pop / 100000,
      n.haem_imput.nrj_r4   = haem.rate.nrj_r4 * pop / 100000,
      
      # Number of deaths predicted (need to substract the number of death predicted in baseline)
      n.cvd_imput.vgt    = cvd.rate.vgt * pop / 100000,
      n.chd_imput.vgt    = chd.rate.vgt * pop / 100000,
      n.stroke_imput.vgt = stroke.rate.vgt * pop / 100000,
      n.isch_imput.vgt   = isch.rate.vgt * pop / 100000,
      n.haem_imput.vgt   = haem.rate.vgt * pop / 100000,
      
      #Number of death expected
      n.chd=(chd.rate*pop)/ 100000, 
      n.stroke=(stroke.rate*pop)/ 100000,
      n.cvd=(cvd.rate*pop)/ 100000,
      pop.original=(pop),
      
      fusion=paste(Country, Sex, AgeGroups2, IMD_quintile,Year,sep=" "),
      sim=sim)]
  
  datacc <- subset(datacchi, select=c(fusion,Country, Year, scenario, AgeGroups2, Sex, IMD_quintile, sim, 
                                    quant.Energy_out_kcal_Mean.pred,dif.nrj,dif.nrj_year,dif.nrj_day,
                                    n.cvd_imput.nrj:n.haem_imput.vgt, pop, bmi_byindiv_new,bmi_byindiv_new_year,bmi_byindiv_new_sensi_day,bmi_byindiv_new_sensi,bmi_byindiv_new_sensiHALL,
                                    bmi_byindiv_under20_2021,bmi_byindiv_under20_new,bmi_byindiv_under20_new_year,bmi_byindiv_under20_new_sensi_day,bmi_byindiv_under20_new_sensi,bmi_byindiv_under20_new_sensiHALL,
                                    bmi_byindiv_2025_2021,bmi_byindiv_2025_new,bmi_byindiv_2025_new_year,bmi_byindiv_2025_new_sensi_day,bmi_byindiv_2025_new_sensi,bmi_byindiv_2025_new_sensiHALL,
                                    bmi_byindiv_2530_2021,bmi_byindiv_2530_new,bmi_byindiv_2530_new_year,bmi_byindiv_2530_new_sensi_day,bmi_byindiv_2530_new_sensi,bmi_byindiv_2530_new_sensiHALL,
                                    bmi_byindiv_over30_2021,bmi_byindiv_over30_new,bmi_byindiv_over30_new_year,bmi_byindiv_over30_new_sensi_day,bmi_byindiv_over30_new_sensi,bmi_byindiv_over30_new_sensiHALL,
                                    pct_byindiv_under20_2021:pct_byindiv_over30_2021,pct_byindiv_under20_new:pct_byindiv_over30_new_sensi_day,
                                    n.chd,n.stroke,n.cvd,pop.original,
                                    chd.rate.nrj:cvd.rate.vgt))
                                    #pct_byindiv_over30_new_sensi,n.chd,n.stroke,n.cvd,pop.original,weight_grp))
                   
  rm(nb2, dataccxx, datacchi, dataccxx2,trend.Energy_kcal_meal_Mean.sim,trend.Energy_out_kcal_Mean.sim,trend.Mealoutofhome_Mean.sim,trend.Mealoutofhome_year_Mean.sim,trend.totalveg_out_Mean.sim)
 
  datadd <- datacc
  # hist(datadd$BMIss)
  
  # Join results
  gc()
  if (sim==1) {
    results <- datadd
  } else {
    results <- bind_rows(results,datadd)
  }
  
  rm(databb,datacc,datacc0,datacc1, datacc2, datacc3, datadd,rr.ses.sim)
  
  # Just to see the model advancement when it's runing, keep a track on time
  time.new <- difftime(Sys.time(),time.old, units = "secs")
  time.end <- difftime(Sys.time(),time.begin, units= "mins")
  
  print(paste0("Simulation: ",sim,"/",numsim,"   Time spent: ",round(time.new, digits=2), " seconds    Total time: ", round(time.end, digits=2)," minutes"))
  
}
##############"END OF THE monte carlo
# head(results) 
# View(ggt)
saveRDS(results, file=paste("results.paper", toString(Sys.Date()), "rds", sep = "."))




################################################################################
####                              9. RESULTS                                ####
################################################################################
rm(list=ls())

library(haven)
library(data.table)
library(dplyr)
library(ggplot2)

# Functions ###
guardar.archivo <- function(x) {
  saveRDS(x, file=paste0(substitute(x),".rds"))
}
cargar.archivo <- function(x) {
  readRDS(paste0(substitute(x),".rds"))
}


# Load results
results <- cargar.archivo("results.paper.2023-12-21") 
print(paste0("This version of results goes from sim ", min(results$sim)," to sim ",max(results$sim)," and weighs ", format(object.size(results),units="Mb")))



##############################################################################
#Step C: Outputs (e.g., DPPs)																					
##############################################################################

# Number of DPPs (total) ####
gc()
deaths11 <- results[Year>=2022,lapply(.SD, sum, na.rm=TRUE), by = c("Year","scenario","sim"),.SDcols = names(results) %like% "n.cvd|n.chd|n.stroke|n.isch|n.haem"]
#table(deaths$scenario)
deaths <- setDT(deaths11)[, `:=` (scenario1=scenario,
                                  scenario=as.factor(fcase(scenario=="0","Baseline",
                                                          scenario=="1","Consumer response, Full implementation", 
                                                          scenario=="1.compmax","Consumer response 42% compensation, Full implementation", 
                                                          scenario=="1.compmin","Consumer response 11% compensation, Full implementation", 
                                                          scenario=="1.sensi","Consumer response 7.5%, Full implementation", 
                                                          scenario=="1.vgt","1.vgt", 
                                                          scenario=="1rev","1rev", 
                                                          scenario=="1turnover33","Consumer response, Full implementation Turnover 33%", 
                                                          scenario=="1turnover33.compmax","Consumer response 42% compensation, Full implementation Turnover 33%", 
                                                          scenario=="1turnover33.compmin","Consumer response 11% compensation, Full implementation Turnover 33%", 
                                                                               
                                                          scenario=="1turnover59","Consumer response, Full implementation Turnover 59%", 
                                                          scenario=="1turnover59.compmax","Consumer response 42% compensation, Full implementation Turnover 59%", 
                                                          scenario=="1turnover59.compmin","Consumer response 11% compensation, Full implementation Turnover 59%", 
                                                                                      
                                                          scenario=="2","Consumer response, Current implementation", 
                                                          scenario=="2.compmax","Consumer response 42% compensation, Current implementation", 
                                                          scenario=="2.compmin","Consumer response 11% compensation, Current implementation", 
                                                          scenario=="2.sensi","Consumer response 7.5%, Current implementation", 
                                                          scenario=="2.vgt","2.vgt",
                                                          scenario=="2rev","2rev",
                                                          scenario=="2turnover33","Consumer response, Current implementation Turnover 33%",
                                                          scenario=="2turnover33.compmax","Consumer response 42% compensation, Current implementation Turnover 33%", 
                                                          scenario=="2turnover33.compmin","Consumer response 11% compensation, Current implementation Turnover 33%", 
                                                          scenario=="2turnover59","Consumer response, Current implementation Turnover 59%",
                                                          scenario=="2turnover59.compmax","Consumer response 42% compensation, Current implementation Turnover 59%", 
                                                          scenario=="2turnover59.compmin","Consumer response 11% compensation, Current implementation Turnover 59%", 
                                                          scenario=="3","Reformulation WITHOUT COMPENSATION +/- SD, Full implementation",
                                                          scenario=="3.sensi","Reformulation WITHOUT COMPENSATION, Full implementation",
                                                          scenario=="3.comp","Reformulation WITH COMPENSATION, Full implementation",
                                                          scenario=="3.sensi.comp","Reformulation WITH COMPENSATION +/- 20%, Full implementation",
                                                          scenario=="3.compmax","Reformulation WITH 42% compensation, Full implementation",
                                                          scenario=="3.compmin","Reformulation WITH 11% compensation, Full implementation",
                                                          scenario=="3turnover33","Reformulation, Full implementation Turnover 33%",                                 
                                                          scenario=="3turnover33.comp","3turnover33.comp",
                                                          scenario=="3turnover33.comp","3turnover33.comp",
                                                          scenario=="3turnover33.compmax","3turnover33.compmax",
                                                          scenario=="3turnover33.compmin","3turnover33.compmin",
                                                          scenario=="3turnover59","3turnover59",
                                                          scenario=="3turnover59.comp","3turnover59.comp",
                                                          #scenario=="3bis","Reformulation +/- 0, Full implementation",
                                                          scenario=="3bis","Reformulation +/- newSD, Full implementation",
                                                          scenario=="4","Reformulation WITHOUT COMPENSATION +/- 20%, Current implementation",
                                                          scenario=="4.comp","Reformulation WITH COMPENSATION, Current implementation",
                                                          scenario=="4.compmax","Reformulation WITH 42% COMPENSATION, Current implementation",
                                                          scenario=="4.compmin","Reformulation WITH 11% COMPENSATION, Current implementation",
                                                          scenario=="4.sensi","Reformulation, Current implementation",
                                                          scenario=="4.sensi.comp","Reformulation +/- 20% WITH COMPENSATION, Current implementation",
                                                          scenario=="4turnover33","Reformulation, Current implementation Turnover 33%",
                                                          scenario=="4turnover33.comp","4turnover33.comp",
                                                          scenario=="4turnover33.compmax","4turnover33.compmax",
                                                          scenario=="4turnover33.compmin","4turnover33.compmin",
                                                          scenario=="4turnover59","4turnover59",
                                                          scenario=="4turnover59.comp","4turnover59.comp",
                                                          #scenario=="4bis","Reformulation +/- 0%, Current implementation",
                                                          scenario=="4bis","Reformulation +/- newSD, Current implementation",
                                                          scenario=="5","Full implementation", 
                                                          scenario=="5.compmax","Full implementation 42% compensation", 
                                                          scenario=="5.compmin","Full implementation 11% compensation", 
                                                          scenario=="5.sensi","5.sensi", 
                                                          scenario=="5turnover33","5turnover33", 
                                                          scenario=="5turnover33.compmax","5turnover33.compmax", 
                                                          scenario=="5turnover33.compmin","5turnover33.compmin", 
                                                          scenario=="5turnover59","5turnover59", 
                                                          scenario=="6","Current implementation",
                                                          scenario=="6.compmax","Current implementation 42% compensation",
                                                          scenario=="6.compmin","Current implementation 11% compensation",
                                                          scenario=="6.sensi","6.sensi",
                                                          scenario=="6turnover33","6turnover33",
                                                          scenario=="6turnover33.compmax","6turnover33.compmax",
                                                          scenario=="6turnover33.compmin","6turnover33.compmin",
                                                          scenario=="6turnover59","6turnover59")))]

#table(deaths$scenario,useNA = "always")

#Number of deaths expected
gc()
expected <- deaths[Year>=2022 & Year<2042 & scenario1==0,sum(n.cvd,na.rm=TRUE), by = c("scenario1","sim")][, .(scenario1,sim,n.cvd.expected = V1)]
#setDT(expected)
expct <- expected[, as.list(quantile(n.cvd.expected, probs = c(0.5, 0.025, 0.975))), by = scenario1]
paste(expct[1,1], ": ", signif(expct[1,2],4), " [", signif(expct[1,3],4), " ; ", signif(expct[1,4],4),"]",sep="")


#########################TABLE 1
bmicatok <- results[(Year>=2021), 
                    .(Year, scenario, sim,pct_byindiv_over30=pct_byindiv_over30_new*100,weight_grp)][,lapply(.SD,weighted.mean,w=weight_grp),by=c("Year","scenario","sim"),.SDcols = "pct_byindiv_over30"]

#10
# t1bmi10 <- bmicatok %>% filter(scenario %in% c("0","1","2","3","4","5","6") & (Year>=2022 & Year<2032)) %>%
#   group_by(scenario, sim) %>% 
#   summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
#   spread(key=scenario, value=pct_byindiv_over30) %>%
#   transmute(across(`1`:`6`, ~ .-`0`)) %>%
#   gather(key=scenario,value=pct,`1`:`6`) %>%
#   group_by(scenario) #%>%
# #mutate(scenario = factor(scenario,levels=1:6, labels=c("Consumer behaviour, Full implementation", "Consumer behaviour, Current implementation", "Reformulation, Full implementation","Reformulation, Current implementation","Full implementation", "Current implementation")))
# setDT(t1bmi10)
# t1bmi10[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
# tbmi10 <- t1bmi10[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
# bmi_10 <- tbmi10[,.(tbmi10[,1],signif(tbmi10[,2:4],3))]
#20
library(tidyr)
t1bmi <- bmicatok %>% filter(scenario %in% c("0","1","2","3","4","5","6") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30) %>%
  transmute(across(`1`:`6`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1`:`6`) %>%
  group_by(scenario) #%>%
#mutate(scenario = factor(scenario,levels=1:6, labels=c("Consumer behaviour, Full implementation", "Consumer behaviour, Current implementation", "Reformulation, Full implementation","Reformulation, Current implementation","Full implementation", "Current implementation")))
setDT(t1bmi)
t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi <- t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
bmi <- tbmi[,.(tbmi[,1],signif(tbmi[,2:4],3))]


expected.bmi <- bmicatok[scenario==0,mean(pct_byindiv_over30,na.rm=TRUE), by = c("scenario","sim")][, .(pct.expected = V1)]
bmicatok.pct <- setDT(cbind(t1bmi, expected.bmi[1,1]))[,pct.relatif:=((pct/pct.expected)*100)]
bmicatok.pct[, as.list(quantile(pct.relatif, probs = c(0.5, 0.025, 0.975))), by = "scenario"]


###CVD
#10y
dpps_cvd.nrj_10 <- deaths[(Year>=2022 & Year<2032) & scenario1 %in% c("0","1","2","3","4","5","6"),
                          sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                      .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t_10 <- dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt_10 <- t_10[,.(t_10[,1],signif(t_10[,2:4],2))]


#20y
dpps_cvd.nrj <- deaths[(Year>=2022 & Year<2042) & scenario1 %in% c("0","1","2","3","4","5","6"),][,
                       sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                   .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t <- dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt <- t[,.(t[,1],signif(t[,2:4],2))]

test_pct <- merge(dpps_cvd.nrj, expected, by="sim")[,dpps.pct:=(dpps/n.cvd.expected)][,dpps.pct.100:=dpps.pct*100]
t.pct <- test_pct[, as.list(quantile(dpps.pct.100, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt.pct <- t.pct[,.(t.pct[,1],signif(t.pct[,2:4],2))]




#########################APPENDIX TABLE WITH TURNOVER
#20
t1bmi <- bmicatok %>% filter(scenario %in% c("0","1turnover59","2turnover59","3turnover59","4turnover59","5turnover59","6turnover59") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30) %>%
  transmute(across(`1turnover59`:`6turnover59`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1turnover59`:`6turnover59`) %>%
  group_by(scenario) #%>%
#mutate(scenario = factor(scenario,levels=1:6, labels=c("Consumer behaviour, Full implementation", "Consumer behaviour, Current implementation", "Reformulation, Full implementation","Reformulation, Current implementation","Full implementation", "Current implementation")))
setDT(t1bmi)
t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi <- t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
bmi <- tbmi[,.(tbmi[,1],signif(tbmi[,2:4],3))]


#10y
dpps_cvd.nrj_10 <- deaths[(Year>=2022 & Year<2032) & scenario1 %in% c("0","1turnover59","2turnover59","3turnover59","4turnover59","5turnover59","6turnover59"),
                          sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                      .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t_10 <- dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt_10 <- t_10[,.(t_10[,1],signif(t_10[,2:4],2))]
#20y
dpps_cvd.nrj <- deaths[(Year>=2022 & Year<2042) & scenario1 %in% c("0","1turnover59","2turnover59","3turnover59","4turnover59","5turnover59","6turnover59"),
                       sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                   .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t <- dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt <- t[,.(t[,1],signif(t[,2:4],2))]


#########################APPENDIX TABLE WITH compensation
#20y
t1bmi <- bmicatok %>% filter(scenario %in% c("0","1.compmin","2.compmin","5.compmin","6.compmin","1.compmax","2.compmax","5.compmax","6.compmax") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30) %>%
  transmute(across(`1.compmax`:`6.compmin`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1.compmax`:`6.compmin`) %>%
  group_by(scenario) #%>%
#mutate(scenario = factor(scenario,levels=1:6, labels=c("Consumer behaviour, Full implementation", "Consumer behaviour, Current implementation", "Reformulation, Full implementation","Reformulation, Current implementation","Full implementation", "Current implementation")))
setDT(t1bmi)
t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi <- t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
bmi <- tbmi[,.(tbmi[,1],signif(tbmi[,2:4],3))]


dpps_cvd.nrj <- deaths[(Year>=2022 & Year<2042) & scenario1 %in% c("0","1.compmin","2.compmin","5.compmin","6.compmin","1.compmax","2.compmax","5.compmax","6.compmax"),
                       sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                   .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t <- dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt <- t[,.(t[,1],signif(t[,2:4],2))]


#########################APPENDIX TABLE WITH 7.3%
#20
t1bmi <- bmicatok %>% filter(scenario %in% c("0","1.sensi","2.sensi","5.sensi","6.sensi") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30) %>%
  transmute(across(`1.sensi`:`6.sensi`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1.sensi`:`6.sensi`) %>%
  group_by(scenario) 
setDT(t1bmi)
t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi <- t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
bmi <- tbmi[,.(tbmi[,1],signif(tbmi[,2:4],3))]

#10y
dpps_cvd.nrj_10 <- deaths[(Year>=2022 & Year<2032) & scenario1 %in% c("0","1.sensi","2.sensi"),
                          sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                      .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t_10 <- dpps_cvd.nrj_10[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt_10 <- t_10[,.(t_10[,1],signif(t_10[,2:4],2))]
#20y
dpps_cvd.nrj <- deaths[(Year>=2022 & Year<2042) & scenario1 %in% c("0","1.sensi","2.sensi","5.sensi","6.sensi"),
                       sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                   .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t <- dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt <- t[,.(t[,1],signif(t[,2:4],2))]



###ratio comment #41
library(tidyr)
t1bmi <- bmicatok %>% filter(scenario %in% c("0","1","2","3","4","5","6") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30=mean(pct_byindiv_over30),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30) %>%
  transmute(across(`1`:`6`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1`:`6`) %>%
  group_by(scenario) #%>%
#mutate(scenario = factor(scenario,levels=1:6, labels=c("Consumer behaviour, Full implementation", "Consumer behaviour, Current implementation", "Reformulation, Full implementation","Reformulation, Current implementation","Full implementation", "Current implementation")))
setDT(t1bmi)
t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi <- t1bmi[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
bmi <- tbmi[,.(tbmi[,1],signif(tbmi[,2:4],3))]

##
expected.bmi <- bmicatok[scenario==0,mean(pct_byindiv_over30,na.rm=TRUE), by = c("scenario","sim")][, .(sim, pct.expected = V1)]
bmi.consumer.current <- bmicatok[scenario==2,mean(pct_byindiv_over30,na.rm=TRUE), by = c("scenario","sim")][, .(sim,pct.con.current = V1)]
bmi.ref.current <- bmicatok[scenario==4,mean(pct_byindiv_over30,na.rm=TRUE), by = c("scenario","sim")][, .(sim,pct.ref.current = V1)]
bmicatok.ratio1 <- merge(expected.bmi,bmi.consumer.current, by="sim")
bmicatok.ratio <- setDT(merge(bmicatok.ratio1,bmi.ref.current, by="sim"))[,`:=` (pct.con.current=pct.con.current-pct.expected,
                                                                                 pct.ref.current=pct.ref.current-pct.expected)][,ratio:=((pct.ref.current/pct.con.current))]
test <- bmicatok.ratio #[ratio>=0]
test[, as.list(quantile(ratio, probs = c(0.5, 0.025, 0.975)))]


###CVD
dpps_cvd.nrj <- deaths[(Year>=2022 & Year<2042) & scenario1 %in% c("0","1","2","3","4","5","6"),][,
                                                                                                  sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,
                                                                                                                                                                                                              .(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
t <- dpps_cvd.nrj[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt <- t[,.(t[,1],signif(t[,2:4],2))]

test_pct <- merge(dpps_cvd.nrj, expected, by="sim")[,dpps.pct:=(dpps/n.cvd.expected)*100]
t.pct <- test_pct[, as.list(quantile(dpps.pct, probs = c(0.5, 0.025, 0.975))), by = scenario]
tt.pct <- t.pct[,.(t.pct[,1],signif(t.pct[,2:4],2))]

expected.cvd <- deaths[scenario1==0,sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario1","sim")][, .(sim, cvd.expected = V1)]
cvd.consumer.current <- deaths[scenario1==2,sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario1","sim")][, .(sim,cvd.con.current = V1)]
cvd.ref.current <- deaths[scenario1==4,sum(n.cvd_imput.nrj,na.rm=TRUE), by = c("scenario1","sim")][, .(sim,cvd.ref.current = V1)]
cvd.ratio1 <- merge(expected.cvd,cvd.consumer.current, by="sim")
cvd.ratio <- setDT(merge(cvd.ratio1,cvd.ref.current, by="sim"))[,ratio:=((cvd.ref.current/cvd.con.current))]
cvd.ratio[, as.list(quantile(ratio, probs = c(0.5, 0.025, 0.975)))]



##############appendix 6, HALL EQUATION FOR REVIEW 2
# nrj change in sensi Hall

##due to a mistake in previous code, may need to implement scenario 0
# bmicatokHALL <- results[(scenario %in% c("0","1","2","3","4","5","6") & (Year>=2022 & Year<2042)), 
#                         .(Year, scenario, sim,
#                           pct_byindiv_over30_sensiHALL=ifelse(scenario==0,pct_byindiv_over30_new*100,pct_byindiv_over30_new_sensiHALL*100),
#                           weight_grp)][,lapply(.SD,weighted.mean,w=weight_grp),by=c("Year","scenario","sim"),.SDcols = "pct_byindiv_over30_sensiHALL"]
#otherwise do
bmicatokHALL <- results[(Year>=2021), 
                      .(Year, scenario, sim,pct_byindiv_over30_sensiHALL=pct_byindiv_over30_new_sensiHALL*100,weight_grp)][,lapply(.SD,weighted.mean,w=weight_grp),by=c("Year","scenario","sim"),.SDcols = "pct_byindiv_over30_sensiHALL"]

t1bmi_sensiHALL <- bmicatokHALL %>% filter(scenario %in% c("0","1","2","3","4","5","6") & (Year>=2022 & Year<2042)) %>%
  group_by(scenario, sim) %>%
  summarize(pct_byindiv_over30_sensiHALL=mean(pct_byindiv_over30_sensiHALL),na.rm=TRUE) %>%
  spread(key=scenario, value=pct_byindiv_over30_sensiHALL) %>%
  transmute(across(`1`:`6`, ~ .-`0`)) %>%
  gather(key=scenario,value=pct,`1`:`6`) %>%
  group_by(scenario) #%>%
setDT(t1bmi_sensiHALL)
t1bmi_sensiHALL[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = "scenario"]
tbmi_sensiHALL <- t1bmi_sensiHALL[, as.list(quantile(pct, probs = c(0.5, 0.025, 0.975))), by = scenario]

dpps_cvd.nrj_sensiHALL <- deaths[(Year>=2022 & Year<2042 & scenario1 %in% c("0","1","2","3","4","5","6")),
                             sum(n.cvd_imput.nrj_sensiHALL,na.rm=TRUE), by = c("scenario","sim")][scenario!="Baseline", .(scenario,sim,dpps = V1)][,.(scenario,sim,dpps,scenariogrp=ifelse(scenario %like% "Current","Current implementation","Full implementation"))]
dpps_cvd.nrj_sensiHALL[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = scenario]





##############################
#### BY IMD
##############################
library(dplyr)
#Test consommation by IMD
IMDconso <- results %>%
  filter(Year==2021, scenario==0)

gc()
deaths_10IMD2 <- results %>% filter(!(Year<=2021 & Year<2042) & (scenario=="0" | scenario=="5" | scenario=="6"))  %>%
  mutate(pct_byindiv_over30=pct_byindiv_over30_new*100,
         IMD_quintile=as.character(IMD_quintile))  %>% 
  dplyr::select(fusion,IMD_quintile,Year,scenario,sim,n.cvd_imput.nrj,n.cvd,pct_byindiv_over30,weight_grp) 


gc()
deaths_10IMD3 <- deaths_10IMD2 %>% 
  group_by(IMD_quintile,scenario,sim) %>%
  summarize(across(starts_with("n."),sum, na.rm=TRUE)) 

bmicat <- deaths_10IMD2 %>% group_by(IMD_quintile,scenario,sim) %>%
  summarize(across(starts_with("pct_"),weighted.mean, weight_grp,na.rm=TRUE))

  deaths_10IMD <- merge(deaths_10IMD3,bmicat,by=c("IMD_quintile","scenario","sim"))

  

###CVD
dpps.cvd_10IMD <- deaths_10IMD %>%
  filter(scenario=="0" | scenario=="5" | scenario=="6") %>%
  group_by(IMD_quintile, scenario, sim) %>%
  summarize(n.cvd_imput.nrj=sum(n.cvd_imput.nrj),na.rm=TRUE) %>%
  spread(key=c(scenario), value=n.cvd_imput.nrj) %>%
  dplyr::select(!`0`) %>%
  gather(key=scenario,value=dpps,`5`:`6`) %>%
  group_by(scenario,IMD_quintile) %>%
  dplyr::mutate(scenario = factor(scenario,levels=5:6, labels=c("Full implementation", "Current implementation"))) #%>%
#summarize(median = median(dpps), lb = quantile(dpps,0.025), ub = quantile(dpps,0.975))

dpps.cvd_10IMDbase <- deaths_10IMD %>%
  filter(scenario==0) %>%
  group_by(IMD_quintile, scenario, sim) %>%
  summarize(n.cvd=sum(n.cvd),na.rm=TRUE) %>%
  spread(key=c(scenario), value=n.cvd) %>%
  gather(key=scenario,value=dpps,`0`) %>%
  group_by(scenario,IMD_quintile) %>%
  dplyr::mutate(scenario = factor(scenario,levels=0, labels=c("Baseline"))) 

dpps.cvd_10IMD <- rbind(dpps.cvd_10IMDbase,dpps.cvd_10IMD)

#Numbers for the paper
setDT(dpps.cvd_10IMD)
dpps.cvd_10IMD[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = c("scenario","IMD_quintile")]
t <- dpps.cvd_10IMD[, as.list(quantile(dpps, probs = c(0.5, 0.025, 0.975))), by = c("scenario","IMD_quintile")]
#t <- t[IMD_quintile=='1'|IMD_quintile=='5',]
tt <- t[,.(t[,1],t[,2],signif(t[,3:5],3))]



###ratio for paper
expected.bmi.q1 <- setDT(deaths_10IMD)[scenario==0 & IMD_quintile %in% c(1) ,][, .(sim,bmi.exp.q1=pct_byindiv_over30)]
expected.bmi.q5 <- setDT(deaths_10IMD)[scenario==0 & IMD_quintile %in% c(5) ,][, .(sim,bmi.exp.q5=pct_byindiv_over30)]
current.q1 <- deaths_10IMD[scenario==6 & IMD_quintile %in% c(1), ][, .(sim,current.q1=n.cvd_imput.nrj,current.exp.q1=n.cvd,bmi.current.q1=pct_byindiv_over30)]
current.q5 <- deaths_10IMD[scenario==6 & IMD_quintile %in% c(5), ][, .(sim,current.q5=n.cvd_imput.nrj,current.exp.q5=n.cvd,bmi.current.q5=pct_byindiv_over30)]
full.q1 <- deaths_10IMD[scenario==5 & IMD_quintile %in% c(1), ][, .(sim,full.q1=n.cvd_imput.nrj,full.exp.q1=n.cvd,bmi.full.q1=pct_byindiv_over30)]
full.q5 <- deaths_10IMD[scenario==5 & IMD_quintile %in% c(5), ][, .(sim,full.q5=n.cvd_imput.nrj,full.exp.q5=n.cvd,bmi.full.q5=pct_byindiv_over30)]
ratioIMD1 <- merge(expected.bmi.q1,expected.bmi.q5, by=c("sim"))
ratioIMD2 <- merge(ratioIMD1,current.q1, by=c("sim"))
ratioIMD3 <- merge(ratioIMD2,current.q5, by=c("sim"))
ratioIMD4 <- merge(ratioIMD3,full.q1, by=c("sim"))
ratioIMD5 <- setDT(merge(ratioIMD4,full.q5,  by=c("sim")))
ratioIMD <- setDT(ratioIMD5)[,.(ratio.bmi.exp=bmi.exp.q1/bmi.exp.q5,
                                                                 ratio.bmi.current=bmi.current.q1/bmi.current.q5,
                                                                 ratio.bmi.full=bmi.full.q1/bmi.full.q5,
                                                                 ratio.cvd.exp=current.exp.q1/current.exp.q5,
                                                                 ratio.cvd.current=current.q1/current.q5,
                                                                 ratio.cvd.full=full.q1/full.q5)]
ratioIMD[, as.list(quantile(ratio.bmi.exp, probs = c(0.5, 0.025, 0.975)))]
ratioIMD[, as.list(quantile(ratio.bmi.current, probs = c(0.5, 0.025, 0.975)))]
ratioIMD[, as.list(quantile(ratio.bmi.full, probs = c(0.5, 0.025, 0.975)))]
ratioIMD[, as.list(quantile(ratio.cvd.exp, probs = c(0.5, 0.025, 0.975)))]
ratioIMD[, as.list(quantile(ratio.cvd.current, probs = c(0.5, 0.025, 0.975)))]
ratioIMD[, as.list(quantile(ratio.cvd.full, probs = c(0.5, 0.025, 0.975)))]

#to have UI for bmi change
bmicatok.imd <- results[(Year>=2021 & Year<2042) & (scenario=="0" | scenario=="5" | scenario=="6") & IMD_quintile %in% c(1,5),][,
                                                                                                                                .(Year, scenario, IMD_quintile,sim,pct_byindiv_over30=pct_byindiv_over30_new*100,weight_grp)][,
                                                                                                                                                                                                                              lapply(.SD,weighted.mean,w=weight_grp),by=c("Year","scenario","sim", "IMD_quintile"),.SDcols = "pct_byindiv_over30"]

expected.bmi.q1 <- setDT(bmicatok.imd)[scenario==0 & IMD_quintile %in% c(1) ,][, .(sim,Year,bmi.exp.q1=pct_byindiv_over30)]
expected.bmi.q5 <- setDT(bmicatok.imd)[scenario==0 & IMD_quintile %in% c(5) ,][, .(sim,Year,bmi.exp.q5=pct_byindiv_over30)]
current.q1 <- bmicatok.imd[scenario==6 & IMD_quintile %in% c(1), ][, .(sim,Year,bmi.current.q1=pct_byindiv_over30)]
current.q5 <- bmicatok.imd[scenario==6 & IMD_quintile %in% c(5), ][, .(sim,Year,bmi.current.q5=pct_byindiv_over30)]
full.q1 <- bmicatok.imd[scenario==5 & IMD_quintile %in% c(1), ][, .(sim,Year,bmi.full.q1=pct_byindiv_over30)]
full.q5 <- bmicatok.imd[scenario==5 & IMD_quintile %in% c(5), ][, .(sim,Year,bmi.full.q5=pct_byindiv_over30)]
diffIMD1 <- merge(expected.bmi.q1,expected.bmi.q5, by=c("sim","Year"))
diffIMD2 <- merge(diffIMD1,current.q1, by=c("sim","Year"))
diffIMD3 <- merge(diffIMD2,current.q5, by=c("sim","Year"))
diffIMD4 <- merge(diffIMD3,full.q1, by=c("sim","Year"))
diffIMD5 <- setDT(merge(diffIMD4,full.q5,  by=c("sim","Year")))
diffIMD <- setDT(diffIMD5)[,.(sim, Year,
                                pct.bmi.currentq1=bmi.exp.q1-bmi.current.q1,
                                pct.bmi.currentq5=bmi.exp.q5-bmi.current.q5,
                                pct.bmi.fullq1=bmi.exp.q1-bmi.full.q1,
                                pct.bmi.fullq5=bmi.exp.q5-bmi.full.q5)]
diffIMDtest <- diffIMD[Year>2021]
diffIMDtest[, as.list(quantile(pct.bmi.currentq1, probs = c(0.5, 0.025, 0.975)))]
diffIMDtest[, as.list(quantile(pct.bmi.currentq5, probs = c(0.5, 0.025, 0.975)))]
diffIMDtest[, as.list(quantile(pct.bmi.fullq1, probs = c(0.5, 0.025, 0.975)))]
diffIMDtest[, as.list(quantile(pct.bmi.fullq5, probs = c(0.5, 0.025, 0.975)))]

