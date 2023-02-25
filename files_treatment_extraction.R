library(dplyr)
library(TSA)
library(ggplot2)

## Listing files
arquivos <- list.files('ecg/')

## Definig breaks number
breaks <- seq(0,0.5, length.out = 100)

## Function for extracting data
extract_periodogram <- function(file){
  data <- read.csv(paste0('ecg/', file))
  
  # scale to z dist
  scale_wf <- scale(data$waveform)
  
  # Periodogram
  periodogram_data <- periodogram(scale_wf, plot = FALSE)
  
  # Periodogram treatment 
  periodogram_df <- 
    data.frame(
      freq = periodogram_data$freq,
      spec = periodogram_data$spec,
      file = file
    ) |> 
    filter(spec <= 0.5) |> 
    mutate(label = cut(freq, breaks)) |> 
    group_by(label) |> 
    summarise(spec =sum(spec)) |> 
    tidyr::pivot_wider(names_from = label, values_from = spec) |> 
    mutate(file_name = file)
  
  return(periodogram_df)
}

# Apply function to all files
all_data <- lapply(arquivos, extract_periodogram)

# bind rows
final_files <- bind_rows(all_data)

# defining selected registers

selected_reg <- read.csv('data 2-11-2021.csv')$archive

final_files <-
final_files |> 
  mutate(
    selected = ifelse(file_name %in% selected_reg, 1, 0)
  ) |> 
  mutate(
    across(everything(), ~tidyr::replace_na(.x, 0))
  )
  


write.csv(final_files, 'df_for_modeling.csv')




