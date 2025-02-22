library(dplyr)
library(openxlsx)
library(purrr)

#' @title Split Dataframe into Excel Files and Sheets
#' @description Splits a dataframe into separate Excel files based on one column and organizes sheets within each file based on another column.
#' @param df Dataframe to be split.
#' @param file_col (Optional) Column used to determine separate Excel files. If NULL, all sheets go into one file.
#' @param sheet_col Column used to determine different sheets within each Excel file.
#' @param output_dir Directory where Excel files should be saved (default: "split2xls/").
#' @param gene_col Name of the SYMBOL gene column for annotation (default: "gene").
#' @param gene_annotation Logical. If TRUE, merges additional gene annotation data from uniprot and refseq (default: TRUE).
#' @param overwrite Logical. If TRUE, overwrites existing Excel files (default: TRUE).
#' @return Creates Excel files in the specified output directory.
#' @export
split2xls <- function(df,
                      file_col = NULL,
                      sheet_col,
                      output_dir = file.path(getwd(), "split2xls/"),
                      gene_col = "gene",
                      gene_annotation = TRUE,
                      overwrite = TRUE) {

  # Ensure sheet_col exists
  if (!(sheet_col %in% colnames(df))) {
    stop("Error: Dataframe is missing required column: ", sheet_col)
  }

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load gene summary data
  annotation_file <- system.file("extdata", "gene_functions_uniprot_refseq.csv", package = "thepubr")

  if (gene_annotation && file.exists(annotation_file)) {
    gene_summary <- read.csv(annotation_file)

    # Merge gene annotation if gene column exists
    if (gene_col %in% colnames(df)) {
      df <- df %>%
        left_join(gene_summary, by = setNames("gene", gene_col))
    } else {
      message("Warning: Gene column not found in the dataframe. Skipping annotation.")
    }
  }

  # If file_col is NULL, create a single Excel file
  if (is.null(file_col)) {
    xls <- createWorkbook()

    # Process unique sheets
    unique_sheets <- df %>% distinct(.data[[sheet_col]]) %>% pull()
    walk(unique_sheets, function(sheet_value) {
      sheet_df <- df %>% filter(.data[[sheet_col]] == sheet_value)
      if (nrow(sheet_df) > 0) {  # Avoid adding empty sheets
        addWorksheet(xls, as.character(sheet_value))
        writeData(xls, sheet = as.character(sheet_value), x = sheet_df)
      }
    })

    # Save workbook
    file_name <- file.path(output_dir, "split_data.xlsx")
    saveWorkbook(xls, file_name, overwrite = overwrite)
    message("Created: ", file_name)

  } else {
    # Get unique file names
    unique_files <- df %>% distinct(.data[[file_col]]) %>% pull()

    # Process each unique file
    walk(unique_files, function(file_value) {
      subset_df <- df %>% filter(.data[[file_col]] == file_value)

      # Create Excel workbook
      xls <- createWorkbook()

      # Process unique sheets
      unique_sheets <- subset_df %>% distinct(.data[[sheet_col]]) %>% pull()
      walk(unique_sheets, function(sheet_value) {
        sheet_df <- subset_df %>% filter(.data[[sheet_col]] == sheet_value)
        if (nrow(sheet_df) > 0) {  # Avoid adding empty sheets
          addWorksheet(xls, as.character(sheet_value))
          writeData(xls, sheet = as.character(sheet_value), x = sheet_df)
        }
      })

      # Save the workbook
      file_name <- file.path(output_dir, paste0(file_value, ".xlsx"))
      saveWorkbook(xls, file_name, overwrite = overwrite)
      message("Created: ", file_name)
    })
  }

  message("Excel files successfully created in: ", output_dir)
}
