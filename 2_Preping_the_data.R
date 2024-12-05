###Reading in the data, filtering, and creating data frame for analysis

### importing all the pre filtered data sets (pre filtered because it was 20+GB and doesnt all load into R)

library(osfr)
library(readr)

# Authenticate and retrieve the project
osf_auth("FQ5MB6ryJmdnIPAbVl1WXC4G1gCYnmQ9QfsR4t5hVt80r7wsANTYoVWMThjvdSscfRagbI")
project <- osf_retrieve_node("au49d")

# List files in the "ebird" folder of the OSF project
ebird_files <- osf_ls_files(project, path = "ebird", n_max = 25)

# Create a temporary directory for downloading files
temp_dir <- tempdir()

# Filter the file list to include only those of interest 
snowy_owl_file <- ebird_files[ebird_files$name == "filtered_Snowy_Owl.csv", ]
checklist_files <- ebird_files[grepl("_checklists\\.csv$", ebird_files$name), ]

# Download the files
snowy_owl_path <- osf_download(snowy_owl_file, path = temp_dir, conflicts = "overwrite")$local_path
filtered_Snowy_Owl <- read_csv(snowy_owl_path)

downloaded_files <- osf_download(checklist_files, path = temp_dir, conflicts = "overwrite")

# Read all downloaded checklist files into a list of data frames
checklists <- lapply(downloaded_files$local_path, read_csv)

# Combine all checklists into a single data frame
filtered_all_checklists <- do.call(rbind, checklists)
unlink(downloaded_files)

# As with the temperature data this needs to be ran according to the bands (NW, UCW, LCW, SW) (NC, UCC, LCC, SC) (NE, UCE, LCE, SE), so minimum 4 data frames have to be created

##################### NW #####################
#### break the checklist data up into areas, filtering for the correct dates, and amalgamte by year.
Checklists_NW <- filtered_all_checklists %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>% 
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area, filtering for the correct dates, and amalgamte by year.
Snowy_owl_NW <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>%  
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "NW")

#### combining the observation data and the checklist data together, and using that to standradize observations with num checlists as effort 
df_NW <- left_join(Snowy_owl_NW, Checklists_NW, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

### visualising the data
hist(df_NW$stadradized_count, main = "Histogram")
boxplot(df_NW$stadradized_count, main = "Boxplot")
qqnorm(df_NW$stadradized_count, main = "Normal Q-Q plot")

### removing outliers detected visually via the boxplot above
df_NW_no_outliers <- subset(df_NW, stadradized_count <= 0.08)

##### exporting df to upload to git cause its now a small file
write.csv(df_NW, "df_NW.csv", row.names = FALSE)



##################### UCW #####################
#### break the checklist data up into area
Checklists_UCW <- filtered_all_checklists %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>% 
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_UCW <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>%  
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "UCW")


df_UCW <- left_join(Snowy_owl_UCW, Checklists_UCW, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_UCW$stadradized_count, main = "Histogram")
boxplot(df_UCW$stadradized_count, main = "Boxplot")
qqnorm(df_UCW$stadradized_count, main = "Normal Q-Q plot")

## no outliars

##### exporting df
write.csv(df_UCW, "df_UCW.csv", row.names = FALSE)





















##################### LCW #####################
#### break the checklist data up into area
Checklists_LCW <- filtered_all_checklists %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>% 
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_LCW <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>%  
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "LCW")


df_LCW <- left_join(Snowy_owl_LCW, Checklists_LCW, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_LCW$stadradized_count, main = "Histogram")
boxplot(df_LCW$stadradized_count, main = "Boxplot")
qqnorm(df_LCW$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_LCW_no_outliers <- subset(df_LCW, stadradized_count <= 0.04)

##### exporting df
write.csv(df_LCW, "df_LCW.csv", row.names = FALSE)


##################### SW ########################
Checklists_SW <- filtered_all_checklists %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>% 
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_SW <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>%  
  filter(LONGITUDE > -140, LONGITUDE < -110) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "SW")


df_SW <- left_join(Snowy_owl_SW, Checklists_SW, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_SW$stadradized_count, main = "Histogram")
boxplot(df_SW$stadradized_count, main = "Boxplot")
qqnorm(df_SW$stadradized_count, main = "Normal Q-Q plot")

### no outliers 

##### exporting df
write.csv(df_SW, "df_SW.csv", row.names = FALSE)





##################### NC #####################
#### break the checklist data up into area
Checklists_NC <- filtered_all_checklists %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>% 
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_NC <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>%  
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "NC")


df_NC <- left_join(Snowy_owl_NC, Checklists_NC, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_NC$stadradized_count, main = "Histogram")
boxplot(df_NC$stadradized_count, main = "Boxplot")
qqnorm(df_NC$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_NC_no_outliers <- subset(df_NC, stadradized_count <= 0.4)

##### exporting df
write.csv(df_NC, "df_NC.csv", row.names = FALSE)












##################### UCC #####################
#### break the checklist data up into area
Checklists_UCC <- filtered_all_checklists %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>% 
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_UCC <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>%  
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "UCC")


df_UCC <- left_join(Snowy_owl_UCC, Checklists_UCC, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)


hist(df_UCC$stadradized_count, main = "Histogram")
boxplot(df_UCC$stadradized_count, main = "Boxplot")
qqnorm(df_UCC$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_UCC_no_outliers <- subset(df_UCC, stadradized_count <= 0.3)

##### exporting df
write.csv(df_UCC, "df_UCC.csv", row.names = FALSE)




##################### LCC #####################
#### break the checklist data up into area
Checklists_LCC <- filtered_all_checklists %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>% 
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_LCC <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>%  
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "LCC")


df_LCC <- left_join(Snowy_owl_LCC, Checklists_LCC, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_LCC$stadradized_count, main = "Histogram")
boxplot(df_LCC$stadradized_count, main = "Boxplot")
qqnorm(df_LCC$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_LCC_no_outliers <- subset(df_LCC, stadradized_count <= 0.09)


##### exporting df
write.csv(df_LCC, "/df_LCC.csv", row.names = FALSE)

##################### SC ########################
Checklists_SC <- filtered_all_checklists %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>% 
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_SC <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>%  
  filter(LONGITUDE > -110, LONGITUDE < -80) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "SC")


df_SC <- left_join(Snowy_owl_SC, Checklists_SC, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

hist(df_SC$stadradized_count, main = "Histogram")
boxplot(df_SC$stadradized_count, main = "Boxplot")
qqnorm(df_SC$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_SC_no_outliers <- subset(df_SC, stadradized_count <= 0.04)

##### exporting df
write.csv(df_SC, "/df_SC.csv", row.names = FALSE)




##################### NE #####################
#### break the checklist data up into area
Checklists_NE <- filtered_all_checklists %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>% 
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_NE <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 55, LATITUDE < 60) %>%  
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "NE")


df_NE <- left_join(Snowy_owl_NE, Checklists_NE, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)


hist(df_NE$stadradized_count, main = "Histogram")
boxplot(df_NE$stadradized_count, main = "Boxplot")
qqnorm(df_NE$stadradized_count, main = "Normal Q-Q plot")

## no outliers 

##### exporting df
write.csv(df_NE, "/df_NE.csv", row.names = FALSE)






##################### UCE #####################
#### break the checklist data up into area
Checklists_UCE <- filtered_all_checklists %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>% 
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_UCE <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 50, LATITUDE < 55) %>%  
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "UCE")


df_UCE <- left_join(Snowy_owl_UCE, Checklists_UCE, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)


hist(df_UCE$stadradized_count, main = "Histogram")
boxplot(df_UCE$stadradized_count, main = "Boxplot")
qqnorm(df_UCE$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
df_UCE_no_outliers <- subset(df_UCE, stadradized_count <= 0.1)

##### exporting df
write.csv(df_UCE, "/df_UCE.csv", row.names = FALSE)


##################### LCE #####################
#### break the checklist data up into area
Checklists_LCE <- filtered_all_checklists %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>% 
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_LCE <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 45, LATITUDE < 50) %>%  
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "LCE")


df_LCE <- left_join(Snowy_owl_LCE, Checklists_LCE, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)


hist(df_LCE$stadradized_count, main = "Histogram")
boxplot(df_LCE$stadradized_count, main = "Boxplot")
qqnorm(df_LCE$stadradized_count, main = "Normal Q-Q plot")

### removing outliers 
# no outliers detected

##### exporting df
write.csv(df_LCE, "/df_LCE.csv", row.names = FALSE)

##################### SE #####################
#### break the checklist data up into area
Checklists_SE <- filtered_all_checklists %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>% 
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  mutate(List_Count = 1) %>% #making a variable i can sum()
  group_by(obs_year) %>% 
  summarise(Total_Checklists = sum(List_Count))

#### Break OBS data into area
Snowy_owl_SE <- filtered_Snowy_Owl %>% 
  filter(LATITUDE > 40, LATITUDE < 45) %>%  
  filter(LONGITUDE > -80, LONGITUDE < -50) %>% 
  filter(obs_month < 4 | obs_month > 10) %>% # setting up the date ranges (not sure the selected dates are correct)
  group_by(obs_year) %>% 
  summarise(Total_Count = sum(`OBSERVATION COUNT`)) %>% 
  mutate(AREA_ID = "SE")

## adding checklist and obs data together
df_SE <- left_join(Snowy_owl_SE, Checklists_SE, by = "obs_year") %>% 
  mutate(stadradized_count = Total_Count / Total_Checklists)

## checking the data
hist(df_SE$stadradized_count, main = "Histogram", breaks = 30)
boxplot(df_SE$stadradized_count, main = "Boxplot")
qqnorm(df_SE$stadradized_count, main = "Normal Q-Q plot")
qqline(df_SE$stadradized_count)

### removing outliers 
df_SE_no_outliers <- subset(df_SE, stadradized_count <= 0.048)


##### exporting df
write.csv(df_SE, "/df_SE.csv", row.names = FALSE)





###### reading in temprature data and setting it up #####

Temprature_data <- read_csv("Temperature.csv")

temp_data <- Temprature_data %>% 
  group_by(Year) %>%  
  summarise(NW = mean(NW),UCW= mean(UCW),LCW = mean(LCW),SW = mean(SW),NC = mean(NC),UCC = mean(UCC),LCC = mean(LCC),SC = mean(SC),NE = mean(NE),UCE = mean(UCE),LCE = mean(LCE),SE = mean(SE)) %>% 
  rename(obs_year = Year)

#### combining the temprature data with ebird data

final_df_NW <- left_join(temp_data, df_NW_no_outliers, by = "obs_year")

final_df_UCW <- left_join(temp_data, df_UCW, by = "obs_year")

final_df_LCW <- left_join(temp_data, df_LCW_no_outliers, by = "obs_year")

final_df_SW <- left_join(temp_data, df_SW, by = "obs_year")

final_df_NC <- left_join(temp_data, df_NC_no_outliers, by = "obs_year")

final_df_UCC <- left_join(temp_data, df_UCC_no_outliers, by = "obs_year")

final_df_LCC <- left_join(temp_data, df_LCC_no_outliers, by = "obs_year")

final_df_SC <- left_join(temp_data, df_SC_no_outliers, by = "obs_year")

final_df_NE <- left_join(temp_data, df_NE, by = "obs_year")

final_df_UCE <- left_join(temp_data, df_UCE_no_outliers, by = "obs_year")

final_df_LCE <- left_join(temp_data, df_LCE, by = "obs_year")

final_df_SE <- left_join(temp_data, df_SE_no_outliers, by = "obs_year")


# running the MLR models 
