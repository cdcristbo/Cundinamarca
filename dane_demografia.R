
process_and_export_data <- function(poblacion) {
  # Set the column names using the 6th row
  colnames(poblacion) <- poblacion[6,]
  
  # Filter the data
  poblacion <- poblacion %>% 
    filter(COD_DPTO == 25 & `ÁREA GEOGRÁFICA` == "Total")
  
  # Select the first 7 columns
  poblacion <- poblacion[, 1:7]
  
  # Read the TerriData_Dim1 Excel file
  TerriData_Dim1 <- read_excel("TerriData_Dim1.xlsx") %>%  
    filter(Indicador %in% c("Categoría ley 617 de 2000",
                            "Densidad poblacional",
                            "Población total",
                            "Extensión")) %>% 
    select(-Mes)
  
  # Split TerriData_Dim1 into different datasets by Indicador column
  split_data <- split(TerriData_Dim1, TerriData_Dim1$Indicador)
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add the first sheet with the total data
  addWorksheet(wb, "Total")
  writeData(wb, "Total", TerriData_Dim1)
  
  # Add sheets for each split dataset
  for (name in names(split_data)) {
    addWorksheet(wb, name)
    writeData(wb, name, split_data[[name]])
  }
  
  # Save the workbook
  saveWorkbook(wb, "BASE_DESCRIPCION_GENERAL.xlsx", overwrite = TRUE)
}
