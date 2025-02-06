# ---------------
# Title: Focal_per_male_total
# Date: 26 jun 2024
# Author: mgranellruiz and Josefien Tankink
# Goal: Create a dataframe for the males of the RTS, in where we selected only the focals that were done when the male was
# present. Then calculated how much gr or bgr happened during each interaction with adult females
# ---------------

# library ---------------------
library(hrbrthemes)
library(broom)
library(ggplot2)
library(dplyr)
library(car)
library(tibble)
library(lme4)
library(lubridate)
library(tidyr)
source("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/Functions.R")

# Data -----------
# Josefien
# male_table <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Raising the stakes/Male_table.csv") %>%
#  mutate(StartDate_mb = as.Date(StartDate_mb , format = "%d/%m/%Y"),
#         EndDate_mb = as.Date(EndDate_mb , format = "%d/%m/%Y"))
# ff <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Raising the stakes/Cleaned_focal.csv") %>%
#  change_group_names("Group") %>%
#  mutate(Date = ymd(Date))

# Maria
male_table <- read.csv("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/establishment_grooming_exchanges/Data/Male_table.csv") %>%
  mutate(StartDate_mb = ymd(StartDate_mb),
         EndDate_mb = ymd(EndDate_mb))
ff <- read.csv("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/establishment_grooming_exchanges/Data/Cleaned_focal.csv") %>%
  change_group_names("Group") %>%
  mutate(Date = ymd(Date))

range(ff$Date)
hist(subset(ff, ff$Group == "NH")$Date, breaks = "weeks")

# So now we want to select only observations within the study period of that male
# Initialize an empty list to store the dataframes
male_dfs <- list()

# Loop through each row in the male_table dataframe
for(i in seq_len(nrow(male_table))) {
  # Extract the male's name, start date, end date, group
  male_name <- male_table$AnimalCode[i]
  start_date <- as.Date(male_table$StartDate_mb[i])
  end_date <- as.Date(male_table$EndDate_mb[i])
  group <- male_table$Group_mb[i]
  #male_status <- t$MaleStatus[i]

  # Create a unique key combining male_name and male_status
  male_key <- paste(male_name,
                    #male_status,
                    group, sep = "_")

  # Filter 'f' to get observations within the start and end dates and for the specific group
  male_df <- subset(ff, Date >= start_date & Date <= end_date & Group == group &
                      (IDIndividual1 == male_name | nchar(IDIndividual1) == 4))

  # Calculate the day of residence
  # Ensure Date is in Date format; adjust if your Date column is not already in this format
  male_df$Date <- as.Date(male_df$Date)
  #Day = as.numeric(ymd(Date) - ymd(StartDate_mb)),
  male_df$Day <- as.integer(difftime(male_df$Date, start_date, units = "days")) + 1

  # Store the dataframe in the list with the unique key
  male_dfs[[male_key]] <- male_df
}

# Clean up the environment from variables used in the loop
rm(end_date, group, i, male_df, male_name, start_date, male_key)#, male_status)

# clean the male_dfs, that is combine the males that have been immigrants or residents in different groups
# remove the males that have had no affiliative interactions

# Extract AnimalCode_MaleStatus from names and create a list of data frames grouped by this key
male_dfs_grouped <- split(male_dfs, sapply(names(male_dfs), function(x) {
  paste(strsplit(x, "_")[[1]][1:2], collapse = "_") }))

# Function to bind data frames within each group, ignoring those with 0 rows
bind_dfs <- function(dfs) {
  non_empty_dfs <- Filter(function(df) nrow(df) > 0, dfs)
  if(length(non_empty_dfs) > 0) {
    return(do.call(rbind, non_empty_dfs))
  } else {
    return(NULL)
  }
}

# Apply the function to each group
male_dfs_filtered <- lapply(male_dfs_grouped, bind_dfs) %>%
  Filter(Negate(is.null), .) # Remove NULL entries from the result

# function to help us define the cummulative duration
reset_cumsum <- function(Behaviour, Duration) {
  cum_duration <- numeric(length(Duration))  # Initialize result vector
  current_sum <- 0                           # Track the running sum

  for (i in seq_along(Duration)) {
    if (Behaviour[i] == "Affiliative") {
      current_sum <- 0  # Reset the running sum on Affiliative
    } else {
      current_sum <- current_sum + Duration[i]  # Add Duration to running sum
    }
    cum_duration[i] <- current_sum  # Store result
  }
  return(cum_duration)
}

# Define a function to process each dataframe
process_dataframe <- function(df, key) {
  # Extract male_id from the key
  # Assuming the key is in the format 'male_name_status'
  male_id <- unlist(strsplit(key, "_"))[1]
  # Ensure the data is sorted
  df_sorted <- df %>%
    arrange(Date, Time, IDIndividual1) %>%
    mutate(IDIndividual1 = as.character(IDIndividual1),
           IDIndividual2 = as.character(IDIndividual2))

  # Create shifted columns for comparison
  df_sorted <- df_sorted %>%
      group_by(IDIndividual1) %>% # group by focal
      mutate(
        prev_IDIndividual2 = lag(IDIndividual2, default = first(IDIndividual2))) %>%
      mutate(prev_IDIndividual2 = ifelse(prev_IDIndividual2 == "", NA, prev_IDIndividual2)) %>%
      fill(prev_IDIndividual2, .direction = "down") %>%
      mutate(
        prev_Behaviour = lag(Behaviour, default = first(Behaviour)),
        prev_Duration = lag(Duration, default = 0)
      ) %>%
      # Calculate the cumulative sum of Duration for non-Affiliative behaviours
      mutate(cum_duration = reset_cumsum(Behaviour, Duration)) %>%
      # Mark rows with new date and reset at Affiliative behavior
      mutate(temp_new_date = ifelse(Date != lag(Date, default = first(Date)), TRUE, ifelse(prev_Behaviour == "Affiliative", FALSE, NA))) %>%
      fill(temp_new_date, .direction = "down") %>%
      mutate(temp_new_date = replace_na(temp_new_date, FALSE)) %>%
      # Mark rows with Moving behavior and reset at Affiliative behavior
      mutate(temp_moving = ifelse(Behaviour == "Moving", TRUE, ifelse(prev_Behaviour == "Affiliative", FALSE, NA))) %>%
      fill(temp_moving, .direction = "down") %>%
      mutate(temp_moving = replace_na(temp_moving, FALSE)) %>%
      mutate(moved_since_last_affiliative = temp_moving) %>%
      # Incorporate the new logic for change_flag
      mutate(
        change_flag =
          case_when(
            Behaviour == "Affiliative" & prev_Behaviour == "Affiliative" & IDIndividual2 == prev_IDIndividual2 ~ FALSE,
            temp_new_date |
              moved_since_last_affiliative |
              IDIndividual1 != lag(IDIndividual1) |
              is.na(prev_IDIndividual2) |
              cum_duration > 60                      ~ TRUE,
            TRUE ~ FALSE
          ),
        BehaviourFocalType = ifelse(grepl("bgr", BehaviourFocal), "bgr", ifelse(grepl("gr", BehaviourFocal), "gr", NA))
      ) %>%
      mutate(change_flag = replace_na(change_flag, FALSE),
             InteractionID = cumsum(change_flag))

  # Calculate Duration for "bgr" and "gr"
  duration_affiliative_bgr <- df_sorted %>%
    filter(Behaviour == "Affiliative" & BehaviourFocalType == "bgr") %>%
    group_by(Date, IDIndividual1, IDIndividual2, InteractionID, Day) %>%
    summarize(Duration_bgr = sum(Duration))  %>%
    mutate(Duration_bgr = coalesce(Duration_bgr, 0))

  duration_affiliative_gr <- df_sorted %>%
    filter(Behaviour == "Affiliative" & BehaviourFocalType == "gr") %>%
    group_by(Date, IDIndividual1, IDIndividual2, InteractionID, Day) %>%
    summarize(Duration_gr = sum(Duration))  %>%
    mutate(Duration_gr = coalesce(Duration_gr, 0))

  # Merge the Dataframes
  final_df <- merge(duration_affiliative_bgr, duration_affiliative_gr, by = c("IDIndividual1", "IDIndividual2", "Date", "InteractionID", "Day"), all = TRUE)

  # Extract unique IDs from IDIndividual1
  unique_ids <- unique(final_df$IDIndividual1)

  # Additional filtering step
  final_df <- final_df %>%
   filter((IDIndividual1 == male_id & nchar(IDIndividual2) == 4) |
             (nchar(IDIndividual1) == 4 & IDIndividual2 == male_id))

  return(final_df)
}

# Apply the modified function to each dataframe in the male_dfs list and retain names
processed_dfs <- setNames(
  lapply(names(male_dfs_filtered), function(male_id) process_dataframe(male_dfs_filtered[[male_id]], male_id)),
  names(male_dfs_filtered)
)


# Function to determine if an ID is male or female
is_male <- function(ids) {
  sapply(ids, function(id) nchar(id) == 3 || nchar(id) > 4)
}

# Remove dataframes that have 0 entries
processed_dfs <- Filter(function(df) nrow(df) > 0, processed_dfs)

# Process and aggregate dataframes
# ignore warning
processed_dfs_aggregated <- lapply(processed_dfs, function(df) {
  male_ids <- is_male(df$IDIndividual1)
  switch_condition <- df$IDIndividual1 == ifelse(!male_ids, df$IDIndividual1, df$IDIndividual2)

  df %>%
    mutate(MaleID = ifelse(male_ids, df$IDIndividual1, df$IDIndividual2),
           FemaleID = ifelse(!male_ids, df$IDIndividual1, df$IDIndividual2),
           Duration_bgr = ifelse(switch_condition, df$Duration_gr, df$Duration_bgr),
           Duration_gr = ifelse(switch_condition, df$Duration_bgr, df$Duration_gr)) %>%
    group_by(MaleID, FemaleID, Date, InteractionID, Day) %>%
    summarize(across(c(Duration_bgr, Duration_gr), sum, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(across(c(MaleID, FemaleID), as.character))
})

# Add the 'Group' column and combine into one dataframe
big_dataframe <- bind_rows(mapply(function(df, name) {
  mutate(df, Group = sub(".*_", "", name))
}, processed_dfs_aggregated, names(processed_dfs_aggregated), SIMPLIFY = FALSE))

bd <- big_dataframe %>%
  left_join(.,male_table %>% dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb),
            by = c("MaleID" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(!is.na(StartDate_mb),
         Date > StartDate_mb & Date < EndDate_mb # remove returnees
  ) %>%
  mutate(
    Duration_bgr = replace_na(Duration_bgr, 0),
    Duration_gr = replace_na(Duration_gr, 0),
    Day = as.numeric(ymd(Date) - ymd(StartDate_mb))
  )

# filter not juveniles
lh <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Life history/fast_factchecked_LH_27112024.csv")

bd_adults <- bd %>% left_join(lh[,c("AnimalCode", "DOB_estimate")] %>% distinct(), by=c("FemaleID"= "AnimalCode")) %>%
  mutate(femaleage = as.numeric(Date - as.Date(DOB_estimate)) / 365.25) %>% 
  filter(femaleage > 3.5)


table(bd_adults$Status)

habituation <- data.frame(
  MaleID = c("Aar","Dok","Eiv","Flu", "Man", "Mom", "Nel", "Pro", "Rya", "Sey", "Tam","Ted", "Tow","Umb","Vul", "War", "Win", "Xia","Xin", "Yaz",
             "Apa", "Bob", "Boo", "Bra", "Buk", "Cai", "Dal", "Dix", "Dri", "Fur", "Hav", "Kno", "Kny", "Lif", "Nak", "Nge", "Oke", "PlainJane", "Pom",
             "Pru", "Que", "Sho", "Tch", "Tev", "Tot", "Vla", "Vry"),
  Habituation = c("H","H","UH","H", "UH", "UH","UH", "H", "UH", "H", "UH", "H","UH","H","H","H","UH","H","H","H",
                  "H", "UH", "UH", "UH", "H", "UH", "H", "H", "UH", "H", "UH", "UH", "UH", "H", "H", "H", "UH", "H", "H", "H", "H", "H", "H", "UH", "UH", "H", "H")
)

bd_adults <- bd_adults %>%
  left_join(habituation, by="MaleID") %>% mutate(Habituation = ifelse(Status == "Resident", "H", Habituation)) %>%
  mutate(Habituation = case_when(
    MaleID == "Oke" & Group == "KB" & Status == "Immigrant" ~ "UH",
    MaleID == "Oke" & Group == "IF" ~ "H",
    TRUE ~ Habituation
  ))

write.csv(bd_adults, "C:/Users/josef/Documents/PhD/IVP DATA/Raising the stakes/Focal_per_male_total.csv", row.names = F)
