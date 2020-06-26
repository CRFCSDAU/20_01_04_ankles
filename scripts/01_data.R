
  library(readxl)
  library(tidyverse)
  library(testthat)
  library(viridis)

# Data -------------------------------------------------------------------------

  data <- read_excel("data/Ankle Trial Anonymous Data.xlsx")

# names(data)
# lapply(data, class)
# lapply(data, table)
# lapply(data, function(x) table(is.na(x)))

  names(data) <- gsub(
    "\\.\\.\\.|\\.\\.|\\.", "_", make.names(tolower(names(data)))
  )
  
# Remove blank rows and columns
  data <- data[, unlist(lapply(data, function(x) !all(is.na(x))))] # blank cols
  data <- data[rowSums(is.na(data)) != ncol(data),] # Remove blank rows
  data <- filter(data, !is.na(randmization))
  
# Inspect data #################################################################
  
  # library(summarytools)
  # view(dfSummary(data))  
  
# Check and clean variables ####################################################
  
# ID, Arm, Consent ----
  data <- rename(data, arm = randmization)
  data$id <- data$arm
  data$id <- gsub("\\D+", "", data$id)
  
  expect_equal(length(unique(data$id)), nrow(data)) # Are IDs unique, 1 row per
  
  data$arm <- gsub("\\d+", "", data$arm) %>%
    gsub("\\.", "", .) %>%
    gsub("_", "", .) %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    factor(levels = c("Cast", "Boot"))
  
# table(data$arm)
  expect_equal(table(data$arm)[[1]], table(data$arm)[[2]])               
   
  
# Consent
  
  expect_equal(table(data$consent)[[1]], nrow(data)) # All obs have consent
  
# Protocol violations and loss to follow-up ----
  
# table(data$violation_and_ltf)
  data$violation[data$violation_and_ltf == 15] <- 1
  data$ltf[data$violation_and_ltf == 12] <- 1
  

  
# Outcomes ----
  
# Primary ----
# Olerud-Molander Ankle Score (OMAS). This score ranges from 0 to 100,
# with 100 representing the normal ankle function 
  
# names(select(data, contains("omas")))
  
  data <- rename(
    data, 
    omas_01_14d = omas_score_2_weeks2, 
    omas_02_42d = omas_score_6_weeks, 
    omas_03_90d = omas_score_3_months, 
    omas_04_180d = omas_score_6_months, 
    omas_05_365d = omas_score_1_year
    )
  
  sum_plot <- function(sub, ...){
    select(data, arm, contains(sub)) %>%
      gather(time, val, -arm) %>%
      ggplot(aes(x = val, fill = arm)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~time, ncol = 1) +
      scale_fill_viridis("", discrete = TRUE, begin = 0.5,
                         end = 0.8, option = "inferno", direction = -1) +
      theme(panel.background = element_rect(fill = "grey20"),
            panel.grid = element_blank(),
            strip.background = element_rect(fill = "grey20"),
            strip.text = element_text(color = "white")) +
      ylab("Density") +
      xlab("Value")
  }
  
  # sum_plot("omas")

  # Missing values
  # map(
  #   select(data, arm, contains("omas")), 
  #   function(x)table(is.na(x))
  # ) # Lots of missing as trial progresses
  
  
# Secondary ----
  
# Complication rate (infection and fixation failure) ----
                                                 # Not enough of these to matter

  # table(data$complication)
  # table(data$complications_note)
  # map(
  #   select(data, contains("complication")), 
  #   function(x)table(is.na(x))
  # )
  # 
  # select(data, id, arm, contains("complication")) %>%
  #   filter(!is.na(complication) | !is.na(complications_note)) %>%
  #   View()
  
  # These need to be split out. 
  
  data$infection <- NA
  # Picks up any complication with a 1, but not 11.   
  # table(data$complication)  
  # table(data$complication[grepl("1(?!1|$)|^1$", data$complication, perl = T)])  
  data$infection[grepl("1(?!1|$)|^1$", data$complication, perl = T)] <- 1 
  # One issue here is that it's not clear who was lost to follow up yet, and so
  # may have had a complication that wasn't picked up                    # QUERY  
  
  # data$nonunion <- NA
  # table(data$complication[grepl("10", data$complication)]) 
  # data$nonunion[grepl("10", data$complication)] <- 1           # No non-unions

  
  # Total arc of ankle motion ----
  # (plantar flexion and dorsal-flection) measured in degrees using a goniometer 
  
  # names(select(data, contains("arc")))
  # names(select(data, contains("ankle")))
  
  
  data <- rename(
    data, 
    ankle_01_14d = total_ankle_arc, 
    ankle_02_42d = total_ankle_arc_35, 
    ankle_03_90d = total_ankle_arc_2, 
    ankle_04_180d = total_arc, 
    ankle_05_365d = total_ankle_arc_62
  )
  
  # sum_plot("ankle_")
  
  # map(
  #   select(data, arm, contains("ankle_")),
  #   function(x)table(is.na(x))
  # ) # Lots of missing as trial progresses
  
  
# RAND 36-Item Short Form Survey (SF-36) scoring ----
  
  # names(select(data, starts_with("sf_")))
  
  data <- rename(
    data, 
    sf_ph_01_14d = sf_36_physical_health_2_weeks, 
    sf_ph_02_42d = sf_36_ph_39, 
    sf_ph_03_90d = sf_36_ph2, 
    sf_ph_04_180d = sf_36_ph_57, 
    sf_ph_05_365d = sf_36_ph_2
  )
  
  # sum_plot("sf_ph")
  
  data <- rename(
    data, 
    sf_mh_01_14d = sf_36_mental_health, 
    sf_mh_02_42d = sf_36_mh2, 
    sf_mh_03_90d = sf_36_mh_50, 
    sf_mh_04_180d = sf_36_6_mh, 
    sf_mh_05_365d = sf_36_mh_65
  )
  
  # sum_plot("sf_mh")
  
  
# The time needed to return to work in days ----
# This doesn't actually seem to be measured in days. Pretty messy overall. 
# At a glance though, more boots were back to work at 6 weeks than casts.   
  # names(select(data, starts_with("return")))
  
  data <- rename(
    data, 
    rtw_02_42d = return_to_work_37, 
    rtw_03_90d = return_to_work_2, 
    rtw_04_180d = return_to_work_55, 
  )
  
  data$rtw_03_90d <- as.numeric(gsub("\\D+", "", data$rtw_03_90d)) 
  data$rtw_04_180d[data$rtw_04_180d > 5 & !is.na(data$rtw_04_180d)] <- 1
  
  tar <- grepl("rtw_", names(data))
  data[tar] <- map(
    data[tar], 
    factor, 
    levels = 0:4, 
    labels = c("No", "Yes", "Not Yet", "Retired", "Unemployed")
  )
  
  # map(data[tar], table)
  
  # select(data, id, arm, contains("rtw_")) %>%
  #   gather(time, val, -arm, - id) %>%
  #   filter(val %in% c("No", "Yes")) %>%
  #   ggplot(aes(y = val, x = time, color = id, group = id)) +
  #   geom_line(alpha = 1, 
  #             position = position_jitter(width = 0.1, height = 0.1)) +
  #   facet_wrap(~arm) +
  #   scale_color_viridis(guide = FALSE, discrete = TRUE) +
  #   theme(panel.background = element_rect(fill = "grey20"),
  #         panel.grid = element_blank(),
  #         strip.background = element_rect(fill = "grey20"),
  #         strip.text = element_text(color = "white")) +
  #   ylab("Density") +
  #   xlab("Value")

  
  
# Postoperative hospitalisation length in days ----
  data$date_of_discharge <- as.POSIXct(
    as.numeric(data$date_of_discharge) * 60 * 60 * 24,
    origin = "1899-12-30"
    )
  
  data$los <- difftime(
    data$date_of_discharge, data$date_of_surgery, units = "days"
    ) %>%
    as.numeric()
  
  # ggplot(data, aes(x = arm, fill = factor(los))) +
  #   geom_bar(position = "fill") +
  #   scale_fill_viridis("", discrete = TRUE, option = "inferno", begin = 0.1, 
  #                      direction = -1) +
  #   theme(panel.background = element_rect(fill = "grey20"),
  #         panel.grid = element_blank(),
  #         strip.background = element_rect(fill = "grey20"),
  #         strip.text = element_text(color = "white")) +
  #   ylab("Proportion") +
  #   xlab("")
  
# Save data ####################################################################
  save(data, file = "data.RData")
  rm(list = ls())
  load("data.RData")