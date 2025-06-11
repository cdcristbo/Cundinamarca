library(readxl)
library(stringr)
library(stringi)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# ICET

# Define the folder path
folder_path <- "DatosAnuario"

# Get a list of all Excel files in the folder
excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read and bind all Excel files
base <- excel_files %>%
  map_df(~ {
    file_name <- tools::file_path_sans_ext(basename(.x)) # Extract file name without extension
    read_excel(.x) %>%
      filter(!is.na(...2)) %>%  # Filter rows where the second column is not NA
      setNames(.[1, ]) %>%      # Set the first row as column names
      filter(FUENTE != "FUENTE") %>% # Filter out rows where FUENTE equals "FUENTE"
      mutate(Anuario = file_name)    # Add a new column with the file name
  })

# View the combined dataset
print(base)
# Save the combined results to a CSV file
write.csv(base, "~/Gobernacion/Anuario/all_indicators.csv", row.names = FALSE)

DEP <- read_excel("~/Gobernacion/Anuario/Departamentos.xlsx") %>% 
  distinct(`N°`,Nombre) %>% 
  mutate(`N°` = as.numeric(`N°`))

#base2 = read_excel("~/Gobernacion/Anuario/all_indicators.xlsx")

ICET <- read_excel("~/Gobernacion/Anuario/Departamentos.xlsx") %>% 
  filter(!is.na(`N°`)) %>% 
  mutate(`N°` = as.numeric(`N°`)) %>% 
  distinct(`N°`,Nombre) %>%
  rename(INDICADOR = Nombre) %>%
  mutate(INDICADOR = str_to_lower(INDICADOR)) %>% 
  mutate(INDICADOR = stri_trans_general(INDICADOR, "Latin-ASCII")) %>% 
  
  left_join(base2  %>% 
              mutate(INDICADOR = str_to_lower(INDICADOR))%>%
              mutate(INDICADOR = str_replace_all(INDICADOR, "vÃ­as", "vias")) %>% 
            mutate(INDICADOR = stri_trans_general(INDICADOR, "Latin-ASCII")) %>% 
              distinct(COD_MUN,INDICADOR, Anuario) ) %>% 
  mutate(count = ifelse(is.na(COD_MUN), 0, 1)) %>% 
  filter(count == 1) %>% 
  distinct(INDICADOR, `N°`) 



basefinal = base2 %>% 
  mutate(INDICADOR = str_to_lower(INDICADOR))%>%
  mutate(INDICADOR = str_replace_all(INDICADOR, "vÃ­as", "vias")) %>% 
  mutate(INDICADOR = stri_trans_general(INDICADOR, "Latin-ASCII")) %>% 
  filter(INDICADOR %in% unique(ICET$INDICADOR)) 
