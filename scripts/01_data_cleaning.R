# Is post-operative non-weight-bearing necessary? Study protocol for a pragmatic
# randomised multicentre trial of operatively treated ankle fracture (DoWeCAST?)

# Packages
library('readxl')
library('tidyverse')
library('testthat')
library('viridis')
library('janitor')
library('broom')

# Data -------------------------------------------------------------------------

data <- read_excel('data/raw_BP/Ankle Trial Anonymous Data Copy.xlsx',
                   sheet = 1,
                   range = 'A1:BM161') %>%
  remove_empty(which = 'cols') %>%
  clean_names() %>%
  separate(randmization, c('id', 'arm')) %>%
  mutate(id = as_factor(id),
         arm = as_factor(arm)) %>%
  rename(omas_01_14d = omas_score_2_weeks2, 
    omas_02_42d = omas_score_6_weeks, 
    omas_03_90d = omas_score_3_months, 
    omas_04_180d = omas_score_6_months, 
    omas_05_365d = omas_score_1_year,
    ankle_01_14d = total_ankle_arc, 
    ankle_02_42d = total_ankle_arc_35, 
    ankle_03_90d = total_ankle_arc_2, 
    ankle_04_180d = total_arc, 
    ankle_05_365d = total_ankle_arc_62, 
    sf_ph_01_14d = sf_36_physical_health_2_weeks, 
    sf_ph_02_42d = sf_36_ph_39, 
    sf_ph_03_90d = sf_36_ph2, 
    sf_ph_04_180d = sf_36_ph_57, 
    sf_ph_05_365d = sf_36_ph_2,
    rtw_02_42d = return_to_work_37, 
    rtw_03_90d = return_to_work_2, 
    rtw_04_180d = return_to_work_55,
    sf_mh_01_14d = sf_36_mental_health,
    sf_mh_02_42d = sf_36_mh2, 
    sf_mh_03_90d = sf_36_mh_50, 
    sf_mh_04_180d = sf_36_6_mh, 
    sf_mh_05_365d = sf_36_mh_65)

#  write_csv(data, paste('data/', Sys.Date(), '_ankle_trial_clean.csv', sep = ''))

# Notes: 
# Patient 151 still noted as an inpatient in original data
# Patient 28 - Complication, declined to participation
# Patient 42 - Complication, declined to participation

# Type of implant column

table1_tbl <- data %>%
  select(arm, age, type_of_implant, surgeon, site, hospital_stay) %>%
  separate(type_of_implant, 
           c('method_a', 'method_b', 'method_c', 'method_d'), 
           sep = ',') %>%
  mutate_at(vars(method_a:method_d), 
            funs(trimws)) %>% 
  unite('type_of_implant', method_a:method_d, sep = '-', na.rm = TRUE) %>%
  mutate_at(vars(type_of_implant, surgeon, site), 
             funs(as_factor))

table1_boot <- filter(table1_tbl,  arm == 'Boot')
table1_cast <- filter(table1_tbl,  arm == 'Cast')

# 4. Table 1 --------------------------------------------------------------
# We can write functions to automate common tasks such as generating a table

# Generate the list of names for the table

name.1 <- function(x, ...) {
  
  var.names <- list()
  
  for (i in seq_along(x)) {
    
    if (is.numeric(x[[i]]) |  lubridate::is.POSIXct(x[[i]])){
      var.names[[i]] <- names(x[i])
    }
    
    if (is.factor(x[[i]])){
      var.names[[i]] <- c(names(x[i]), levels(x[[i]]))
    }
  }
  
  unlist(var.names)
}

# Means(sds) or counts(%)

summary.1 <- function(x, ...) {
  
  summary.list <- list()
  
  for (i in seq_along(x)) {
    
    if (is.numeric(x[[i]])){
      summary.list[[i]] <- paste0(round(mean(x[[i]], na.rm = TRUE), 1),
                                  " \u00B1 ",
                                  round(sd(x[[i]],   na.rm = TRUE), 1))
    }
    
    if (is.factor(x[[i]])){
      summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                        " (",
                                        round(table(x[[i]]) /
                                                sum(table(x[[i]])), 3) * 100,
                                        "%)"))
    }
    
    if (lubridate::is.POSIXct(x[[i]])){
      summary.list[[i]] <- " "
    }
    
  }
  unlist(summary.list)
}

# Min and max

min.max <- function(x, ...) {
  
  min.max.list <- list()
  
  for (i in seq_along(x)) {
    
    if (is.numeric(x[[i]])){
      min.max.list[[i]] <- paste0("(",
                                  round(min(x[[i]], na.rm = TRUE), 1),
                                  ", ",
                                  round(max(x[[i]], na.rm = TRUE), 1),
                                  ")")
    }
    
    if (lubridate::is.POSIXct(x[[i]])){
      min.max.list[[i]] <- paste0("(",
                                  min(x[[i]], na.rm = TRUE),
                                  " to ",
                                  max(x[[i]], na.rm = TRUE),
                                  ")")
    }
    
    if (is.factor(x[[i]])){
      min.max.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
    }
    
  }
  unlist(min.max.list)
}

