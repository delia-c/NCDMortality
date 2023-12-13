rm(list = ls())
options(stringsAsFactors = F) 
gc()
library(dplyr)
library(ggplot2)
library(tidyr)
whodata <- read.csv("/Users/yzhu/Downloads/data analysis/bydiseaseandsexdata.csv")
selected_data <- whodata[c("ParentLocationCode", "ParentLocation", "Period", "Dim1", "Dim1ValueCode", "Dim2", "Dim2ValueCode", "FactValueNumeric")]
male_data <- selected_data %>%
  filter(Dim1ValueCode == "MLE")
female_data <- selected_data %>%
  filter(Dim1ValueCode == "FMLE")
bothsex_data <- selected_data %>%
  filter(Dim1ValueCode == "BTSX")

male_2019 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2019)
male_2019 <- sum(male_2019$FactValueNumeric, na.rm = TRUE)

male_2018 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2018)
male_2018 <- sum(male_2018$FactValueNumeric, na.rm = TRUE)

male_2017 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2017)
male_2017 <- sum(male_2017$FactValueNumeric, na.rm = TRUE)

male_2016 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2016)
male_2016 <- sum(male_2016$FactValueNumeric, na.rm = TRUE)

male_2015 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2015)
male_2015 <- sum(male_2015$FactValueNumeric, na.rm = TRUE)

male_2014 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2014)
male_2014 <- sum(male_2014$FactValueNumeric, na.rm = TRUE)

male_2013 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2013)
male_2013 <- sum(male_2013$FactValueNumeric, na.rm = TRUE)

male_2012 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2012)
male_2012 <- sum(male_2012$FactValueNumeric, na.rm = TRUE)

male_2011 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2011)
male_2011 <- sum(male_2011$FactValueNumeric, na.rm = TRUE)

male_2010 <- subset(male_data, Dim1ValueCode == "MLE" & Period == 2010)
male_2010 <- sum(male_2010$FactValueNumeric, na.rm = TRUE)

male_data_frames <- list(
  male_2019 = male_2019,
  male_2018 = male_2018,
  male_2017 = male_2017,
  male_2016 = male_2016,
  male_2015 = male_2015,
  male_2014 = male_2014,
  male_2013 = male_2013,
  male_2012 = male_2012,
  male_2011 = male_2011,
  male_2010 = male_2010
)

male_combined_data <- bind_rows(male_data_frames, .id = "Label")
male_combined_data <- data.frame(
  Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
  Value = c(17855157, 17608364, 17300636, 17015935, 16662051, 16466402, 16315937, 16101875, 15803251, 15568937)
)
male_reshaped_data <- male_combined_data %>%
  pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
ggplot(male_reshaped_data, aes(x = Years, y = Data, color = Value, group = Value)) +
  geom_line() +
  geom_point() +
  labs(title = "female", y = "Data", color = "Value") +
  theme_minimal()


female_2019 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2019)
female_2019 <- sum(female_2019$FactValueNumeric, na.rm = TRUE)

female_2018 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2018)
female_2018 <- sum(female_2018$FactValueNumeric, na.rm = TRUE)

female_2017 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2017)
female_2017 <- sum(female_2017$FactValueNumeric, na.rm = TRUE)

female_2016 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2016)
female_2016 <- sum(female_2016$FactValueNumeric, na.rm = TRUE)

female_2015 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2015)
female_2015 <- sum(female_2015$FactValueNumeric, na.rm = TRUE)

female_2014 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2014)
female_2014 <- sum(female_2014$FactValueNumeric, na.rm = TRUE)

female_2013 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2013)
female_2013 <- sum(female_2013$FactValueNumeric, na.rm = TRUE)

female_2012 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2012)
female_2012 <- sum(female_2012$FactValueNumeric, na.rm = TRUE)

female_2011 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2011)
female_2011 <- sum(female_2011$FactValueNumeric, na.rm = TRUE)

female_2010 <- subset(female_data, Dim1ValueCode == "FMLE" & Period == 2010)
female_2010 <- sum(female_2010$FactValueNumeric, na.rm = TRUE)

female_data_frames <- list(
  female_2019 = female_2019,
  female_2018 = female_2018,
  female_2017 = female_2017,
  female_2016 = female_2016,
  female_2015 = female_2015,
  female_2014 = female_2014,
  female_2013 = female_2013,
  female_2012 = female_2012,
  female_2011 = female_2011,
  female_2010 = female_2010
)

female_combined_data <- bind_rows(female_data_frames, .id = "Label")
female_combined_data <- data.frame(
  Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
  Value = c(15396287, 15248092, 14951647, 14664057, 14422296, 14146937, 13971946, 13809227, 13612210, 13453607)
)
female_reshaped_data <- female_combined_data %>%
  pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
ggplot(female_reshaped_data, aes(x = Years, y = Data, color = "red", group = Value)) +
  geom_line() +
  geom_point() +
  labs(title = "female", y = "Data", color = "red") +
  theme_minimal()

male_reshaped_data$Gender <- "Male"
female_reshaped_data$Gender <- "Female"
combined_data <- rbind(male_reshaped_data, female_reshaped_data)
ggplot(combined_data, aes(x = Years, y = Data, color = Gender, group = Gender)) +
  geom_line() +
  geom_point()
  labs(title = "Male vs Female Data Over Years", y = "Data", color = "Gender") +
  theme_minimal()
  
  
  btsx_2019 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2019)
  btsx_2019 <- sum(btsx_2019$FactValueNumeric, na.rm = TRUE)
  
  btsx_2018 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2018)
  btsx_2018 <- sum(btsx_2018$FactValueNumeric, na.rm = TRUE)
  
  btsx_2017 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2017)
  btsx_2017 <- sum(btsx_2017$FactValueNumeric, na.rm = TRUE)
  
  btsx_2016 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2016)
  btsx_2016 <- sum(btsx_2016$FactValueNumeric, na.rm = TRUE)
  
  btsx_2015 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2015)
  btsx_2015 <- sum(btsx_2015$FactValueNumeric, na.rm = TRUE)
  
  btsx_2014 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2014)
  btsx_2014 <- sum(btsx_2014$FactValueNumeric, na.rm = TRUE)
  
  btsx_2013 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2013)
  btsx_2013 <- sum(btsx_2013$FactValueNumeric, na.rm = TRUE)
  
  btsx_2012 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2012)
  btsx_2012 <- sum(btsx_2012$FactValueNumeric, na.rm = TRUE)
  
  btsx_2011 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2011)
  btsx_2011 <- sum(btsx_2011$FactValueNumeric, na.rm = TRUE)
  
  btsx_2010 <- subset(bothsex_data, Dim1ValueCode == "BTSX" & Period == 2010)
  btsx_2010 <- sum(btsx_2010$FactValueNumeric, na.rm = TRUE)
  
  btsx_data_frames <- list(
    btsx_2019 = btsx_2019,
    btsx_2018 = btsx_2018,
    btsx_2017 = btsx_2017,
    btsx_2016 = btsx_2016,
    btsx_2015 = btsx_2015,
    btsx_2014 = btsx_2014,
    btsx_2013 = btsx_2013,
    btsx_2012 = btsx_2012,
    btsx_2011 = btsx_2011,
    btsx_2010 = btsx_2010
  )
  
  btsx_combined_data <- bind_rows(btsx_data_frames, .id = "Label")
  btsx_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(33251434, 32856424, 32252266, 31679997, 31084341, 30613328, 30287878, 29911096, 29415458, 29022537)
  )
  btsx_reshaped_data <- btsx_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  ggplot(btsx_reshaped_data, aes(x = Years, y = Data, color = "red", group = Value)) +
    geom_line() +
    geom_point() +
    labs(title = "btsx", y = "Data", color = "red") +
    theme_minimal()
  
  male_reshaped_data$Gender <- "Male"
  female_reshaped_data$Gender <- "Female"
  btsx_reshaped_data$Gender <- "BothSex"
  combined_data <- rbind(male_reshaped_data, female_reshaped_data, btsx_reshaped_data)
  ggplot(combined_data, aes(x = Years, y = Data, color = Gender, group = Gender)) +
    geom_line() +
    geom_point() +
  labs(title = "NCD mortality rates between males and females", y = "NCD mortality rates", color = "Gender") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(t = 10))) +
    theme(legend.position = "right")

#=====================================================================================================
  male_Cardio_2019 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2019)
  male_Cardio_2019 <- sum(male_Cardio_2019$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2018 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2018)
  male_Cardio_2018 <- sum(male_Cardio_2018$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2017 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2017)
  male_Cardio_2017 <- sum(male_Cardio_2017$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2016 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2016)
  male_Cardio_2016 <- sum(male_Cardio_2016$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2015 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2015)
  male_Cardio_2015 <- sum(male_Cardio_2015$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2014 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2014)
  male_Cardio_2014 <- sum(male_Cardio_2014$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2013 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2013)
  male_Cardio_2013 <- sum(male_Cardio_2013$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2012 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2012)
  male_Cardio_2012 <- sum(male_Cardio_2012$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2011 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2011)
  male_Cardio_2011 <- sum(male_Cardio_2011$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_2010 <- subset(male_data, Dim2ValueCode == "GHE110" & Period == 2010)
  male_Cardio_2010 <- sum(male_Cardio_2010$FactValueNumeric, na.rm = TRUE)
  
  male_Cardio_data_frames <- list(
    male_Cardio_2019 = male_Cardio_2019,
    male_Cardio_2018 = male_Cardio_2018,
    male_Cardio_2017 = male_Cardio_2017,
    male_Cardio_2016 = male_Cardio_2016,
    male_Cardio_2015 = male_Cardio_2015,
    male_Cardio_2014 = male_Cardio_2014,
    male_Cardio_2013 = male_Cardio_2013,
    male_Cardio_2012 = male_Cardio_2012,
    male_Cardio_2011 = male_Cardio_2011,
    male_Cardio_2010 = male_Cardio_2010
  )
  
  male_Cardio_combined_data <- bind_rows(male_Cardio_data_frames, .id = "Label")
  male_Cardio_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(9323253, 9223809, 9082609,8939476, 8752858,8670429,8608908,8503878,8341727, 8224342)
  )
  male_Cardio_reshaped_data <- male_Cardio_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  ggplot(male_Cardio_reshaped_data, aes(x = Years, y = Data, color = Value, group = Value)) +
    geom_line() +
    geom_point() +
    labs(title = "male", y = "Data", color = "Value") +
    theme_minimal()
  
  male_MA_2019 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2019)
  male_MA_2019 <- sum(male_MA_2019$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2018 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2018)
  male_MA_2018 <- sum(male_MA_2018$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2017 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2017)
  male_MA_2017 <- sum(male_MA_2017$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2016 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2016)
  male_MA_2016 <- sum(male_MA_2016$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2015 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2015)
  male_MA_2015 <- sum(male_MA_2015$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2014 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2014)
  male_MA_2014 <- sum(male_MA_2014$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2013 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2013)
  male_MA_2013 <- sum(male_MA_2013$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2012 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2012)
  male_MA_2012 <- sum(male_MA_2012$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2011 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2011)
  male_MA_2011 <- sum(male_MA_2011$FactValueNumeric, na.rm = TRUE)
  
  male_MA_2010 <- subset(male_data, Dim2ValueCode == "GHE061" & Period == 2010)
  male_MA_2010 <- sum(male_MA_2010$FactValueNumeric, na.rm = TRUE)
  
  male_MA_data_frames <- list(
    male_MA_2019 = male_MA_2019,
    male_MA_2018 = male_MA_2018,
    male_MA_2017 = male_MA_2017,
    male_MA_2016 = male_MA_2016,
    male_MA_2015 = male_MA_2015,
    male_MA_2014 = male_MA_2014,
    male_MA_2013 = male_MA_2013,
    male_MA_2012 = male_MA_2012,
    male_MA_2011 = male_MA_2011,
    male_MA_2010 = male_MA_2010
  )
  
  male_MA_combined_data <- bind_rows(male_MA_data_frames, .id = "Label")
  male_MA_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(5242881, 5144122, 5044353, 4967104, 4860695, 4785235, 4717577, 4660786, 4574059, 4504482)
  )
  male_MA_reshaped_data <- male_MA_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  ggplot(male_MA_reshaped_data, aes(x = Years, y = Data, color = Value, group = Value)) +
    geom_line() +
    geom_point() +
    labs(title = "male", y = "Data", color = "Value") +
    theme_minimal()

  
  male_Res_2019 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2019)
  male_Res_2019 <- sum(male_Res_2019$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2018 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2018)
  male_Res_2018 <- sum(male_Res_2018$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2017 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2017)
  male_Res_2017 <- sum(male_Res_2017$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2016 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2016)
  male_Res_2016 <- sum(male_Res_2016$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2015 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2015)
  male_Res_2015 <- sum(male_Res_2015$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2014 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2014)
  male_Res_2014 <- sum(male_Res_2014$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2013 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2013)
  male_Res_2013 <- sum(male_Res_2013$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2012 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2012)
  male_Res_2012 <- sum(male_Res_2012$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2011 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2011)
  male_Res_2011 <- sum(male_Res_2011$FactValueNumeric, na.rm = TRUE)
  
  male_Res_2010 <- subset(male_data, Dim2ValueCode == "GHE117" & Period == 2010)
  male_Res_2010 <- sum(male_Res_2010$FactValueNumeric, na.rm = TRUE)
  
  male_Res_data_frames <- list(
    male_Res_2019 = male_Res_2019,
    male_Res_2018 = male_Res_2018,
    male_Res_2017 = male_Res_2017,
    male_Res_2016 = male_Res_2016,
    male_Res_2015 = male_Res_2015,
    male_Res_2014 = male_Res_2014,
    male_Res_2013 = male_Res_2013,
    male_Res_2012 = male_Res_2012,
    male_Res_2011 = male_Res_2011,
    male_Res_2010 = male_Res_2010
  )
  
  male_Res_combined_data <- bind_rows(male_Res_data_frames, .id = "Label")
  male_Res_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(2319889, 2296743, 2257358, 2219918, 2190960, 2181960, 2184017, 2155309, 2133238, 2109029)
  )
  male_Res_reshaped_data <- male_Res_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  ggplot(male_Res_reshaped_data, aes(x = Years, y = Data, color = Value, group = Value)) +
    geom_line() +
    geom_point() +
    labs(title = "male", y = "Data", color = "Value") +
    theme_minimal()

  male_DA_2019 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2019)
  male_DA_2019 <- sum(male_DA_2019$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2018 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2018)
  male_DA_2018 <- sum(male_DA_2018$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2017 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2017)
  male_DA_2017 <- sum(male_DA_2017$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2016 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2016)
  male_DA_2016 <- sum(male_DA_2016$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2015 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2015)
  male_DA_2015 <- sum(male_DA_2015$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2014 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2014)
  male_DA_2014 <- sum(male_DA_2014$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2013 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2013)
  male_DA_2013 <- sum(male_DA_2013$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2012 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2012)
  male_DA_2012 <- sum(male_DA_2012$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2011 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2011)
  male_DA_2011 <- sum(male_DA_2011$FactValueNumeric, na.rm = TRUE)
  
  male_DA_2010 <- subset(male_data, Dim2ValueCode == "GHE080" & Period == 2010)
  male_DA_2010 <- sum(male_DA_2010$FactValueNumeric, na.rm = TRUE)
  
  male_DA_data_frames <- list(
    male_DA_2019 = male_DA_2019,
    male_DA_2018 = male_DA_2018,
    male_DA_2017 = male_DA_2017,
    male_DA_2016 = male_DA_2016,
    male_DA_2015 = male_DA_2015,
    male_DA_2014 = male_DA_2014,
    male_DA_2013 = male_DA_2013,
    male_DA_2012 = male_DA_2012,
    male_DA_2011 = male_DA_2011,
    male_DA_2010 = male_DA_2010
  )
  
  male_DA_combined_data <- bind_rows(male_DA_data_frames, .id = "Label")
  male_DA_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(969133.8, 943690, 916316, 889437.3, 857538.3, 828778.2, 805435.8, 781902.3, 754227.9,  731085.3)
  )
  male_DA_reshaped_data <- male_DA_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  ggplot(male_DA_reshaped_data, aes(x = Years, y = Data, color = Value, group = Value)) +
    geom_line() +
    geom_point() +
    labs(title = "male", y = "Data", color = "Value") +
    theme_minimal()
  
  male_Cardio_reshaped_data$Cause <- "Cardiovascular diseases"
  male_Res_reshaped_data$Cause <- "Respiratory diseases"
  male_MA_reshaped_data$Cause <- "Malignant neoplasms"
  male_DA_reshaped_data$Cause <- "Diabetes mellitus"
  cause_combined_data <- rbind(male_Cardio_reshaped_data, male_Res_reshaped_data, male_MA_reshaped_data, male_DA_reshaped_data)
gg1 <-  ggplot(cause_combined_data, aes(x = Years, y = Data, color = Cause, group = Cause)) +
    geom_line() +
    geom_point() +
    labs(title = "Male NCD mortality rates across causes", y = "NCD mortality rates", color = "Cause") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(t = 10))) +
    theme(legend.position = "right")
  
  
  
  
  #=====================================================================================================
  female_Cardio_2019 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2019)
  female_Cardio_2019 <- sum(female_Cardio_2019$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2018 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2018)
  female_Cardio_2018 <- sum(female_Cardio_2018$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2017 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2017)
  female_Cardio_2017 <- sum(female_Cardio_2017$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2016 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2016)
  female_Cardio_2016 <- sum(female_Cardio_2016$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2015 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2015)
  female_Cardio_2015 <- sum(female_Cardio_2015$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2014 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2014)
  female_Cardio_2014 <- sum(female_Cardio_2014$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2013 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2013)
  female_Cardio_2013 <- sum(female_Cardio_2013$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2012 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2012)
  female_Cardio_2012 <- sum(female_Cardio_2012$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2011 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2011)
  female_Cardio_2011 <- sum(female_Cardio_2011$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_2010 <- subset(female_data, Dim2ValueCode == "GHE110" & Period == 2010)
  female_Cardio_2010 <- sum(female_Cardio_2010$FactValueNumeric, na.rm = TRUE)
  
  female_Cardio_data_frames <- list(
    female_Cardio_2019 = female_Cardio_2019,
    female_Cardio_2018 = female_Cardio_2018,
    female_Cardio_2017 = female_Cardio_2017,
    female_Cardio_2016 = female_Cardio_2016,
    female_Cardio_2015 = female_Cardio_2015,
    female_Cardio_2014 = female_Cardio_2014,
    female_Cardio_2013 = female_Cardio_2013,
    female_Cardio_2012 = female_Cardio_2012,
    female_Cardio_2011 = female_Cardio_2011,
    female_Cardio_2010 = female_Cardio_2010
  )
  
  female_Cardio_combined_data <- bind_rows(female_Cardio_data_frames, .id = "Label")
  female_Cardio_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(8540578, 8509807, 8370869, 8215660, 8106037, 7981666, 7906874,  7834920, 7727222, 7663575)
  )
  female_Cardio_reshaped_data <- female_Cardio_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")
  
  female_MA_2019 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2019)
  female_MA_2019 <- sum(female_MA_2019$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2018 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2018)
  female_MA_2018 <- sum(female_MA_2018$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2017 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2017)
  female_MA_2017 <- sum(female_MA_2017$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2016 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2016)
  female_MA_2016 <- sum(female_MA_2016$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2015 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2015)
  female_MA_2015 <- sum(female_MA_2015$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2014 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2014)
  female_MA_2014 <- sum(female_MA_2014$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2013 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2013)
  female_MA_2013 <- sum(female_MA_2013$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2012 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2012)
  female_MA_2012 <- sum(female_MA_2012$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2011 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2011)
  female_MA_2011 <- sum(female_MA_2011$FactValueNumeric, na.rm = TRUE)
  
  female_MA_2010 <- subset(female_data, Dim2ValueCode == "GHE061" & Period == 2010)
  female_MA_2010 <- sum(female_MA_2010$FactValueNumeric, na.rm = TRUE)
  
  female_MA_data_frames <- list(
    female_MA_2019 = female_MA_2019,
    female_MA_2018 = female_MA_2018,
    female_MA_2017 = female_MA_2017,
    female_MA_2016 = female_MA_2016,
    female_MA_2015 = female_MA_2015,
    female_MA_2014 = female_MA_2014,
    female_MA_2013 = female_MA_2013,
    female_MA_2012 = female_MA_2012,
    female_MA_2011 = female_MA_2011,
    female_MA_2010 = female_MA_2010
  )
  
  female_MA_combined_data <- bind_rows(female_MA_data_frames, .id = "Label")
  female_MA_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(4053765, 3982429, 3886877, 3815844, 3730199, 3651362, 3574769, 3521728, 3469186, 3417886)
  )
  female_MA_reshaped_data <- female_MA_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")

  
  
  female_Res_2019 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2019)
  female_Res_2019 <- sum(female_Res_2019$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2018 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2018)
  female_Res_2018 <- sum(female_Res_2018$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2017 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2017)
  female_Res_2017 <- sum(female_Res_2017$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2016 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2016)
  female_Res_2016 <- sum(female_Res_2016$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2015 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2015)
  female_Res_2015 <- sum(female_Res_2015$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2014 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2014)
  female_Res_2014 <- sum(female_Res_2014$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2013 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2013)
  female_Res_2013 <- sum(female_Res_2013$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2012 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2012)
  female_Res_2012 <- sum(female_Res_2012$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2011 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2011)
  female_Res_2011 <- sum(female_Res_2011$FactValueNumeric, na.rm = TRUE)
  
  female_Res_2010 <- subset(female_data, Dim2ValueCode == "GHE117" & Period == 2010)
  female_Res_2010 <- sum(female_Res_2010$FactValueNumeric, na.rm = TRUE)
  
  female_Res_data_frames <- list(
    female_Res_2019 = female_Res_2019,
    female_Res_2018 = female_Res_2018,
    female_Res_2017 = female_Res_2017,
    female_Res_2016 = female_Res_2016,
    female_Res_2015 = female_Res_2015,
    female_Res_2014 = female_Res_2014,
    female_Res_2013 = female_Res_2013,
    female_Res_2012 = female_Res_2012,
    female_Res_2011 = female_Res_2011,
    female_Res_2010 = female_Res_2010
  )
  
  female_Res_combined_data <- bind_rows(female_Res_data_frames, .id = "Label")
  female_Res_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(1817009, 1791069, 1752017, 1714252, 1694937, 1656198, 1658478, 1641835, 1631207, 1607830)
  )
  female_Res_reshaped_data <- female_Res_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")

  
  female_DA_2019 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2019)
  female_DA_2019 <- sum(female_DA_2019$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2018 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2018)
  female_DA_2018 <- sum(female_DA_2018$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2017 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2017)
  female_DA_2017 <- sum(female_DA_2017$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2016 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2016)
  female_DA_2016 <- sum(female_DA_2016$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2015 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2015)
  female_DA_2015 <- sum(female_DA_2015$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2014 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2014)
  female_DA_2014 <- sum(female_DA_2014$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2013 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2013)
  female_DA_2013 <- sum(female_DA_2013$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2012 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2012)
  female_DA_2012 <- sum(female_DA_2012$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2011 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2011)
  female_DA_2011 <- sum(female_DA_2011$FactValueNumeric, na.rm = TRUE)
  
  female_DA_2010 <- subset(female_data, Dim2ValueCode == "GHE080" & Period == 2010)
  female_DA_2010 <- sum(female_DA_2010$FactValueNumeric, na.rm = TRUE)
  
  female_DA_data_frames <- list(
    female_DA_2019 = female_DA_2019,
    female_DA_2018 = female_DA_2018,
    female_DA_2017 = female_DA_2017,
    female_DA_2016 = female_DA_2016,
    female_DA_2015 = female_DA_2015,
    female_DA_2014 = female_DA_2014,
    female_DA_2013 = female_DA_2013,
    female_DA_2012 = female_DA_2012,
    female_DA_2011 = female_DA_2011,
    female_DA_2010 = female_DA_2010
  )
  
  female_DA_combined_data <- bind_rows(female_DA_data_frames, .id = "Label")
  female_DA_combined_data <- data.frame(
    Years = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
    Value = c(984935.4, 964785.8, 941884.1, 918300.8, 891124.3, 857711, 831825.4, 810744.7, 784595.1, 764316.6)
  )
  female_DA_reshaped_data <- female_DA_combined_data %>%
    pivot_longer(cols = -Years, names_to = "Value", values_to = "Data")

  
  female_Cardio_reshaped_data$Cause <- "Cardiovascular diseases"
  female_Res_reshaped_data$Cause <- "Respiratory diseases"
  female_MA_reshaped_data$Cause <- "Malignant neoplasms"
  female_DA_reshaped_data$Cause <- "Diabetes mellitus"
  fe_cause_combined_data <- rbind(female_Cardio_reshaped_data, female_Res_reshaped_data, female_MA_reshaped_data, female_DA_reshaped_data)
gg2 <-  ggplot(fe_cause_combined_data, aes(x = Years, y = Data, color = Cause, group = Cause)) +
    geom_line() +
    geom_point() +
    labs(title = "Female NCD mortality rates across causes", y = "NCD mortality rates", color = "Cause") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(t = 10))) +
    theme(legend.position = "right")

gg1_y_limits <- layer_scales(gg1)$y$get_limits()
gg2_y_limits <- layer_scales(gg2)$y$get_limits()

# Determine the common y-axis limits
y_limits <- range(c(gg1_y_limits, gg2_y_limits))

# Apply the common y-axis limits to both plots using coord_cartesian
gg1 <- gg1 + coord_cartesian(ylim = y_limits)
gg2 <- gg2 + coord_cartesian(ylim = y_limits)

# Arrange the plots using grid.arrange or cowplot's plot_grid
library(gridExtra)  # If using grid.arrange
grid.arrange(gg1, gg2, ncol = 1)  # Arrange plots vertically

# OR

combined_plot <- plot_grid(gg1, gg2, labels = c("A", "B"), ncol = 2) 
combined_plot


