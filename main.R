# Include neccesary packages
library(readr)    # To read csv data
library(dplyr)    # To manipulate data
# library(magrittr) # for piping functionality
library(tidyr)    # to create tidy data
library(stringr)  # for string functionality
library(ggplot2)  # for visualization
library(htmlTable)# for pretty tables

# Placeholder to store api query in future development
# 
# Until API is functional data should be downloaded from https://data.medicare.gov/Hospital-Compare/Patient-survey-HCAHPS-Hospital/dgck-syfz
# Click Export and save as CSV
#
# load raw data
hcahps_raw <- read_csv("Patient_survey__HCAHPS__-_Hospital.csv",
                       col_types = cols(
                        `HCAHPS Answer Percent` = col_number(), 
                        `HCAHPS Answer Percent Footnote` = col_skip(),
                        `HCAHPS Linear Mean Value` = col_number(),
                        `Measure End Date` = col_date(format = "%m/%d/%Y"),
                        `Measure Start Date` = col_date(format = "%m/%d/%Y"),
                        `Number of Completed Surveys` = col_number(),
                        `Number of Completed Surveys Footnote` = col_skip(),
                        `Patient Survey Star Rating` = col_number(),
                        `Patient Survey Star Rating Footnote` = col_skip(),
                        `Survey Response Rate Percent` = col_number(),
                        `Survey Response Rate Percent Footnote` = col_skip())
                       )

# Isolate Leikert response data in long and wide formats
# For some reason, this leikert data is split into separate categories as separate measures rather than single variable factor data
hcahps_long_resp <- hcahps_raw %>%
  filter(!str_detect(`HCAHPS Measure ID`, "LINEAR|STAR")) %>%
  select(`Provider ID`, `HCAHPS Measure ID`, `HCAHPS Answer Percent`)

hcahps_wide_resp <- hcahps_long_resp %>% 
  spread('HCAHPS Measure ID', 'HCAHPS Answer Percent')

# Isolate linear response data in long and wide formats
hcahps_long_linear <- hcahps_raw %>%
  filter(str_detect(`HCAHPS Measure ID`, "LINEAR")) %>%
  select(`Provider ID`, `HCAHPS Measure ID`, `HCAHPS Linear Mean Value`)

hcahps_wide_linear <- hcahps_long_linear %>%
  spread('HCAHPS Measure ID', 'HCAHPS Linear Mean Value')

# Isolate star response data in long and wide formats
hcahps_long_star <- hcahps_raw %>%
  filter(str_detect(`HCAHPS Measure ID`, "STAR")) %>%
  select('Provider ID',`HCAHPS Measure ID`, `Patient Survey Star Rating`)

hcahps_wide_star <- hcahps_long_star %>%
  spread('HCAHPS Measure ID', `Patient Survey Star Rating`)

# Create a new dataframe to join with the wide datasets
hcahps_wide <- hcahps_raw %>% 
  select(`Provider ID`,`Hospital Name`, Address, City, State, `ZIP Code`,
         `County Name`, `Phone Number`, Location, `Number of Completed Surveys`,
         `Survey Response Rate Percent`, `Measure Start Date`, `Measure End Date`) %>% 
  distinct() %>%
  left_join(hcahps_wide_resp, by = "Provider ID") %>%
  left_join(hcahps_wide_linear, by = "Provider ID") %>%
  left_join(hcahps_wide_star, by = "Provider ID")

hcahps_wide <- extract(hcahps_wide, Location, c("coords"), "\\(([^)]+)\\)")
hcahps_wide <- separate(hcahps_wide, coords, c("Latitude","Longitude"), ",")

write_csv(hcahps_wide,"hcahps_wide.csv", delim = ",")

# Output Percentile Tables

lvls <- c(.5, .75, .9) #Desired percentile levels can be modified here
benchmarks <- t(sapply(hcahps_wide_resp[2:length(hcahps_wide_resp)], quantile, lvls, na.rm = T))
htmlTable(benchmarks)

# Visualizations
ggplot(hcahps_long_resp, aes_string(x = "`HCAHPS Measure ID`", y = "`HCAHPS Answer Percent`")) +
  geom_boxplot() + coord_flip()

ggplot(hcahps_long_linear, aes_string(x = "`HCAHPS Measure ID`", y = "`HCAHPS Linear Mean Value`")) +
  geom_boxplot() + coord_flip()

ggplot(hcahps_long_star, aes_string(x = "`HCAHPS Measure ID`", y = "`Patient Survey Star Rating`")) +
  geom_boxplot() + coord_flip()
df <- reshape2::melt(round(cor(hcahps_wide_linear[-1], use = "complete.obs"),2))

levels(df$Var1) <- c("Clean","NurseCom","DocCom",
                     "StaffResp","Pain","Meds","Disch",
                     "CareTrns","HspRating","Quiet","Recmd")
levels(df$Var2) <- c("Clean","NurseCom","DocCom",
                     "StaffResp","Pain","Meds","Disch",
                     "CareTrns","HspRating","Quiet","Recmd")

ggplot(df, aes(x = Var1, y = Var2, fill = value, labels = value)) +
  geom_tile() +
  geom_text(aes(label=value, size=value), color="white")
