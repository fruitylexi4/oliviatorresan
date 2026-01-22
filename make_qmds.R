# Read exif data from images, and create qmds for each file

library(exifr)
library(knitr)
library(glue)
library(tidyverse)

# ------------------------------------------------------------
# Get images in directory (newest first)
# ------------------------------------------------------------
lsfiles <- file.info(dir("images", full.names = TRUE, recursive = TRUE))
lsfiles <- lsfiles[order(lsfiles$mtime, decreasing = TRUE), ]

files <- rownames(lsfiles)

dat <- read_exif(files)

# ------------------------------------------------------------
# Loop safely (in case there are fewer than 9 images)
# ------------------------------------------------------------
for (i in seq_len((nrow(dat)))) {
  
  # ----------------------------------------------------------
  # TITLE (robust to missing EXIF fields)
  # ----------------------------------------------------------
  title <- if ("ObjectName" %in% names(dat)) dat[i, ]$ObjectName else NULL
  
  if (length(title) == 0 || is.na(title)) {
    title <- if ("Title" %in% names(dat)) dat[i, ]$Title else NULL
  }
  
  if (length(title) == 0 || is.na(title)) {
    title <- tools::file_path_sans_ext(basename(dat[i, ]$SourceFile))
  }
  
  title <- stringr::str_to_title(title)
  
  # ----------------------------------------------------------
  # FILE NAME (must be unique and safe)
  # ----------------------------------------------------------
  file_name <- paste0(
    stringr::str_replace_all(title, "[^a-zA-Z0-9]+", "_"),
    ".qmd"
  )
  
  # ----------------------------------------------------------
  # OTHER METADATA
  # ----------------------------------------------------------
  image_location <- dat[i, ]$SourceFile
  
  date <- dat[i, ]$CreateDate
  if (length(date) == 0 || is.na(date)) {
    date <- Sys.time()
  } else {
    date <- lubridate::as_datetime(date)
  }
  
  description <- if ("ImageDescription" %in% names(dat)) dat[i, ]$ImageDescription else ""
  if (length(description) == 0 || is.na(description)) {
    description <- ""
  }
  
  # ----------------------------------------------------------
  # DIRECTORY SETUP
  # ----------------------------------------------------------
  directory <- gsub("^images/", "", dat[i, ]$Directory)
  output_dir <- file.path("img_org", directory)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_file <- file.path(output_dir, file_name)
  
  # ----------------------------------------------------------
  # WRITE QMD (DO NOT OVERWRITE)
  # ----------------------------------------------------------
  if (!file.exists(output_file)) {
    
    glue("
---
title: \"<<title>>\"
author: Olivia T
image: ../../<<image_location>>
description: \"<<description>>\"
categories: [\"<<directory>>\"]
date: <<date>>
format:
  html:
    page-layout: full
---

![](../../<<image_location>>)

", .open = "<<", .close = ">>") %>%
      write_lines(output_file)
  }
}
