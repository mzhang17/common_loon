#read iButton data
## ----load library--------------------
library(readxl)
library(tidyverse)

## ----read xlsx fetched from iButtons--------------------
read_iButton <- function(file_path){
  read_excel(file_path, 
                     skip = 21, 
                     col_names = TRUE) %>% 
    slice(1:(n()-1))
}

read_all_iButton <- function(folder_path){
  file_list <- list.files(path = folder_path, 
                          pattern = "\\.xlsx$", 
                          full.names = TRUE)
  dataframe_list <- lapply(file_list, read_iButton)
  file_names <- basename(file_list)
  names(dataframe_list) <- sub("\\.xlsx$", "", file_names)
  return(dataframe_list)
}

## ----get cutoff time--------------------
get_cutoff_times <- function(path) {
  read_csv(path, col_types = cols(
    site = col_character(),
    date = col_character(),
    time = col_character()
  )) %>%
    mutate(
      site = str_trim(site),
      date = str_trim(date),
      time = str_trim(time),
      
      # Handle common Excel-format quirks
      date = parse_date_time(date, 
                             orders = c("ymd", "mdy", "dmy")),
      time = parse_date_time(time, 
                             orders = c("HMS", "HM", "I:M p", "I:M:S p")) %>% 
        format("%H:%M:%S"),
      
      datetime = ymd_hms(paste(date, time), quiet = TRUE)
    ) %>%
    filter(!is.na(datetime)) %>%
    select(site, datetime) %>%
    deframe()
}

## ----filter cutoff time--------------------
filter_all_iButton <- function(data_list, cutoff_times) {
  
  # Helper function to apply to each item in the list
  filter_one <- function(df_name) {
    df <- data_list[[df_name]]
    
    # Extract site name (e.g., remove _I or _E suffix)
    site <- sub("_[IE]$", "", df_name)
    cutoff <- cutoff_times[[site]]
    
    if (is.null(cutoff)) {
      warning(paste("No cutoff found for site:", site))
      return(df)
    }
    
    df %>%
      mutate(
        Date = as.character(Date),
        Time = as.character(Time),
        DateTime = ymd_hms(paste(Date, Time), quiet = TRUE)
      ) %>%
      filter(DateTime <= cutoff) %>%
      select(-DateTime)
  }
  
  # Apply to all data frames in the list
  result <- setNames(
    lapply(names(data_list), filter_one),
    names(data_list)
  )
  
  return(result)
}

## ----plot--------------------
plot_site <- function(site_name_base, data_list) {
  # Build full names for _I and _E
  df_I_name <- paste0(site_name_base, "_I")
  df_E_name <- paste0(site_name_base, "_E")
  
  # Get the dataframes
  df_I <- data_list[[df_I_name]]
  df_E <- data_list[[df_E_name]]
  
  # Add a column to identify source
  df_I <- df_I %>%
    mutate(Source = "Internal")
  df_E <- df_E %>%
    mutate(Source = "External")
  
  # Combine both
  df_all <- bind_rows(df_I, df_E) %>%
    mutate(
      DateTime = ymd_hms(paste(Date, Time)),
      Value = as.numeric(Value)
    )
  
  # Plot
  ggplot(df_all, aes(x = DateTime, y = Value, color = Source)) +
    geom_line() +
    geom_point(size = 1) +
    labs(
      title = site_name_base,
      x = "DateTime",
      y = "Temperature (°C)",
      color = "Sensor"
    ) +
    theme_minimal()
}

#----------trial----------------------
all_data_ls <- read_all_iButton("iButton25/")
cutoff_times_test <- get_cutoff_times("iButton25/cutoff_times.csv")
filter_all_data_ls <- filter_all_iButton(all_data_ls, cutoff_times_test)
plot_site("DeerRiverFlowDam_predated", filter_all_data_ls)
plot_site("LakeKushaquaIsland_abandoned", filter_all_data_ls)
plot_site("LittleClearPondIsle_predated", filter_all_data_ls)
