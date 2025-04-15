library(dplyr)
library(openxlsx)
library(purrr)
library(glue)

#' @title Split Dataframe into Excel Files and Sheets
#' @description Splits a dataframe into separate Excel files based on one column and organizes sheets within each file based on another column.
#' @param df Dataframe to be split.
#' @param file.col (Optional) Column used to determine separate Excel files. If NULL, all sheets go into one file.
#' @param sheet.col Column used to determine different sheets within each Excel file.
#' @param output.dir Directory and file names where Excel files should be saved (default: "split2xls/").
#' @param gene.col Name of the SYMBOL gene column for annotation (default: "gene").
#' @param gene.annotation Logical. If TRUE, merges additional gene annotation data from uniprot and refseq (default: FALSE).
#' @param overwrite Logical. If TRUE, overwrites existing Excel files (default: TRUE).
#' @return Creates Excel files in the specified output directory.
#' @export
split2xls <- function(df,
                      file.col = NULL,
                      sheet.col,
                      output.dir= file.path(getwd(), "split2xls/split2xls.xlsx"),
                      gene.col = "gene",
                      gene.annotation = FALSE,
                      overwrite = TRUE) {

  # Ensure sheet.col exists
  if (!(sheet.col %in% colnames(df))) {
    stop("Error: Dataframe is missing required column: ", sheet.col)
  }

  # Ensure output directory exists
  if (!dir.exists(output.dir)) {
    dir.create(dirname(output.dir), recursive = TRUE)
  }

  # Load gene summary data
  annotation.file <- system.file("extdata", "gene.functions.from.uniprot.refseq.csv", package = "thepubr")

  if (gene.annotation && file.exists(annotation.file)) {
    gene.summary <- read.csv(annotation.file)

    # Merge gene annotation if gene column exists
    if (gene.col %in% colnames(df)) {
      df <- df %>%
        left.join(gene.summary, by = setNames("gene", gene.col))
    } else {
      message("Warning: Gene column not found in the dataframe. Skipping annotation.")
    }
  }

  # If file.col is NULL, create a single Excel file
  if (is.null(file.col)) {
    xls <- createWorkbook()

    # Process unique sheets
    unique.sheets <- df %>% distinct(.data[[sheet.col]]) %>% pull()
    walk(unique.sheets, function(sheet.value) {
      sheet.df <- df %>% filter(.data[[sheet.col]] == sheet.value)
      if (nrow(sheet.df) > 0) {  # Avoid adding empty sheets
        addWorksheet(xls, as.character(sheet.value))
        writeData(xls, sheet = as.character(sheet.value), x = sheet.df)
      }
    })

    # Save workbook
    saveWorkbook(xls, output.dir, overwrite = overwrite)
    message("Created: ", output.dir)

  } else {
    # Get unique file names
    unique.files <- df %>% distinct(.data[[file.col]]) %>% pull()

    # Process each unique file
    walk(unique.files, function(file.value) {
      subset.df <- df %>% filter(.data[[file.col]] == file.value)

      # Create Excel workbook
      xls <- createWorkbook()

      # Process unique sheets
      unique.sheets <- subset.df %>% distinct(.data[[sheet.col]]) %>% pull()
      walk(unique.sheets, function(sheet.value) {
        sheet.df <- subset.df %>% filter(.data[[sheet.col]] == sheet.value)
        if (nrow(sheet.df) > 0) {  # Avoid adding empty sheets
          addWorksheet(xls, as.character(sheet.value))
          writeData(xls, sheet = as.character(sheet.value), x = sheet.df)
        }
      })

      # Save the workbook
      file.name <- glue("{output.dir}.{file.value}.xlsx")
      saveWorkbook(xls, file.name, overwrite = overwrite)
      message("Created: ", file.name)
    })
  }

  message("Excel files successfully created in: ", output.dir)
}
