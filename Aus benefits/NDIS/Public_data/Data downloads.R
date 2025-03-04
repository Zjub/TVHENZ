# Install and load necessary packages
# install.packages(c("rvest", "httr", "dplyr", "stringr"), dependencies = TRUE)
library(rvest)
library(httr)
library(dplyr)
library(stringr)

# Define the base URL
url <- "https://dataresearch.ndis.gov.au/datasets/participant-datasets"

# Read the webpage content
webpage <- read_html(url)

# Extract all hyperlink nodes
links <- webpage %>%
  html_nodes("a")

# Extract hyperlink text (dataset names)
link_texts <- links %>% html_text(trim = TRUE)

# Extract hyperlink URLs
link_urls <- links %>% html_attr("href")

# Filter for links that contain "/download?attachment"
valid_indices <- grepl("/download\\?attachment$", link_urls)

download_links <- link_urls[valid_indices]
download_names <- link_texts[valid_indices]

# Ensure full URLs (handling relative links)
base_url <- "https://dataresearch.ndis.gov.au"
download_links <- ifelse(grepl("^http", download_links), download_links, paste0(base_url, download_links))

# Clean the hyperlink text to create valid filenames
clean_names <- str_replace_all(download_names, "[^a-zA-Z0-9 _-]", "")
clean_names <- str_replace_all(clean_names, "\\s+", "_")  # Replace spaces with underscores

# Create a folder to store CSV files
output_dir <- "NDIS_CSVs"
dir.create(output_dir, showWarnings = FALSE)

# Download each CSV file
for (i in seq_along(download_links)) {
  file_name <- paste0(clean_names[i], ".csv")
  file_path <- file.path(output_dir, file_name)
  
  # Download the file
  GET(download_links[i], write_disk(file_path, overwrite = TRUE))
  message(paste("Downloaded:", file_name))
}

message("All CSV files have been downloaded successfully.")
