##### Data wrangling for trevallyApp ####

# This code creates the csv files that are inputs for the trevallyApp
# It summarises the data from the thesis metadata in "Trevally.csv" and produces 16 csv files which the app uses

library(openxlsx)

# Set working directory 
setwd("F:/Thesis/welly-trevally/trevallyApp")

# Import sampling data (first tab of main excel spreadsheet) into R
sampling_data <- read.csv("../data/Trevally.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
sampling_data$location.latitude <- as.double(sampling_data$location.latitude)
sampling_data$location.longitude <- as.double(sampling_data$location.longitude)

# Summary of control sequenced data (also separated by season)
sampling_data_control <- sampling_data %>% 
  dplyr::filter(control.successfully.sequenced == "Y") %>% 
  dplyr::group_by(season.caught, location.country, location.coordinate, location.latitude, location.longitude, radius, landing.date, subsample1, subsample2, subsample3) %>% 
  count()

sampling_data_control_winter <- sampling_data_control %>%
  dplyr::filter(season.caught == "winter")
sampling_data_control_spring <- sampling_data_control %>%
  dplyr::filter(season.caught == "spring")
sampling_data_control_summer <- sampling_data_control %>%
  dplyr::filter(season.caught == "summer")
sampling_data_control_autumn <- sampling_data_control %>%
  dplyr::filter(season.caught == "autumn")

write.csv(sampling_data_control, "sampling_data_control.csv")
write.csv(sampling_data_control_winter, "sampling_data_control_winter.csv", row.names=FALSE)
write.csv(sampling_data_control_spring, "sampling_data_control_spring.csv", row.names=FALSE)
write.csv(sampling_data_control_summer, "sampling_data_control_summer.csv", row.names=FALSE)
write.csv(sampling_data_control_autumn, "sampling_data_control_autumn.csv", row.names=FALSE)

# NZ subsampled data
sampling_data_aus <- sampling_data_control %>%
  dplyr::filter(location.country == "australia")
sampling_data_nz_1 <- sampling_data_control %>%
  dplyr::filter(location.country == "new zealand", subsample1 == "Y")
sampling_data_nz_2 <- sampling_data_control %>%
  dplyr::filter(location.country == "new zealand", subsample2 == "Y")
sampling_data_nz_3 <- sampling_data_control %>%
  dplyr::filter(location.country == "new zealand", subsample3 == "Y")

write.csv(sampling_data_aus, "sampling_data_aus.csv", row.names=FALSE)
write.csv(sampling_data_nz_1, "sampling_data_nz_1.csv", row.names=FALSE)
write.csv(sampling_data_nz_2, "sampling_data_nz_2.csv", row.names=FALSE)
write.csv(sampling_data_nz_3, "sampling_data_nz_3.csv", row.names=FALSE)

# COI sequenced data
sampling_data_coi <- sampling_data %>% 
  dplyr::filter(coi.successfully.sequenced == "Y" | coi.sequence.available == "Y") %>% 
  dplyr::group_by(species, location.coordinate, location.latitude, location.longitude, radius, landing.date) %>% 
  count()
sampling_data_coi_georgianus <- dplyr::filter(sampling_data_coi, species == "P. georgianus")
sampling_data_coi_wrighti <- dplyr::filter(sampling_data_coi, species == "P. wrighti")
sampling_data_coi_dentex <- dplyr::filter(sampling_data_coi, species == "P. dentex")

write.csv(sampling_data_coi, "sampling_data_coi.csv", row.names=FALSE)
write.csv(sampling_data_coi_georgianus, "sampling_data_coi_georgianus.csv", row.names=FALSE)
write.csv(sampling_data_coi_wrighti, "sampling_data_coi_wrighti.csv", row.names=FALSE)
write.csv(sampling_data_coi_dentex, "sampling_data_coi_dentex.csv", row.names=FALSE)

# Get population number and sample number for each qma, fma, and stat area for which the control region was sequenced (for population genetics)
manage_group <- sampling_data %>%
  dplyr::filter(control.successfully.sequenced=="Y") %>% 
  dplyr::group_by(location.qma, location.fma, location.stat.area, location.latitude, location.longitude) %>%
  count()

write.csv(manage_group, "manage_group.csv", row.names=FALSE)

# Create table data
table_data <- sampling_data %>%
  filter((control.successfully.sequenced == "Y" | coi.successfully.sequenced == "Y") | (control.successfully.sequenced == "Y" & coi.successfully.sequenced == "Y" | coi.sequence.available == "Y")) %>%
  dplyr::group_by(species, sample.source, season.caught, year.caught, location.stat.area, location.qma, control.successfully.sequenced, coi.successfully.sequenced, tissue.remaining) %>%
  count()

table_data$location.stat.area <- sapply(table_data$location.stat.area, function(x) paste0(" ", x))

write.xlsx(table_data, "table_data.xlsx")
