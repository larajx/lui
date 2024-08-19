#' read_xlplate
#'
#' Allows the plater::read_plate() function to look into excel sheets.
#'
#' @param excel_file_path The file path to the excel file
#' @param sheet_name The sheet name to use the read_plate() function on.
#' @return Returns a tibble containing the plate variables.
#' @export
read_xlplate <- function(excel_file_path, sheet_name) {
  # Load necessary libraries
  library(dplyr)
  library(readxl)
  library(writexl)
  library(plater)

  # Read the Excel file
  df <- read_excel(excel_file_path, sheet = sheet_name)

  # Create a temporary file
  temp_file <- tempfile(fileext = ".csv")

  # Write the dataframe to the temporary CSV file
  write_csv(df, temp_file)

  # Use plater to read the CSV file
  plated_df <- read_plate(temp_file)

  # Delete the temporary file
  file.remove(temp_file)

  return(plated_df)
}
