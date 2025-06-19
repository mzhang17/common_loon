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

file_list <- list.files(path = "iButton25/", 
                        pattern = "\\.xlsx$", 
                        full.names = TRUE)

dataframe_list <- lapply(file_list, read_iButton)

file_names <- basename(file_list)
names(dataframe_list) <- sub("\\.xlsx$", "", file_names)

## ----filter cutoff time--------------------
cutoff_times <- read_csv("iButton25/cutoff_times.csv", col_types = cols(
  site = col_character(),
  date = col_character(),
  time = col_character()
)) %>%
  mutate(
    site = str_trim(site),
    datetime = ymd_hms(paste(date, time))
  ) %>%
  select(site, datetime) %>%
  deframe()

filter_cutoff <- function(df_name) {
  df <- dataframe_list[[df_name]]
  site <- sub("_[IE]$", "", df_name)
  cutoff <- cutoff_times[[site]]
  
  if (is.null(cutoff)) {
    warning(paste("No cutoff found for site:", site))
    return(df)
  }
  
  df <- df %>%
    mutate(DateTime = ymd_hms(paste(Date, Time))) %>%
    filter(DateTime <= cutoff) %>%
    select(-DateTime)
  return(df)
}

filtered_list <- setNames(
  lapply(names(dataframe_list), filter_cutoff),
  names(dataframe_list))

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

plot_site("DeerRiverFlowDam_predated", filtered_list)
plot_site("LakeKushaquaIsland_abandoned", filtered_list)
