# we need the bins for all NYCHA buildings pre MB conversion. Slight snag is
# that the NYCHA directory only keeps current (non pact) buildings. Need to
# pull the pact building BINs from online pdf to add to the NYCHA directory
# and complete our list of bins. 

library(pdftools)
library(stringr)
library(dplyr)
library(fst)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/rad-pact")

# Load the PDF text
pdf_text <- pdf_text("radpact-guide.pdf")

# Split the text into lines
lines <- unlist(strsplit(pdf_text, "\n"))

# Initialize storage for extracted data
data_list <- list()
current_tds <- NA
current_tr <- NA  # Store Transferred Date

# Function to clean and normalize whitespace
clean_line <- function(line) {
  line <- str_trim(line)  # Trim leading/trailing whitespace
  line <- str_replace_all(line, "\\s+", " ")  # Replace multiple spaces with a single space
  return(line)
}

# Loop through each line to extract relevant data
for (line in lines) {
  line <- clean_line(line)  # Normalize whitespace
  
  # Identify TDS #
  if (str_detect(line, "TDS #")) {
    current_tds <- str_extract(line, "TDS #\\s*(\\d+)") %>% str_remove_all("TDS # ")
  }
  
  # Identify Transferred Date
  if (str_detect(line, "TRANSFERRED DATE:")) {
    current_tr <- str_extract(line, "(\\d{1,2}/\\d{1,2}/\\d{4})")  # Extract MM/DD/YYYY format
  }
  
  # Extract BLDG#, Address, and BIN from table rows
  matches <- str_match(line, "^(\\d{10})\\s+(\\d+)\\s+M?\\s*(.*?)\\s+(\\d{5})\\s+.*?(\\d{7})$")
  
  if (!is.na(matches[1,1])) {
    data_list <- append(data_list, list(data.frame(
      tds_number = current_tds,
      building_number = matches[3],  
      Address = matches[4],  
      ZIP_Code = matches[5],  
      BIN = matches[6],  
      converted_date = current_tr, 
      stringsAsFactors = FALSE
    )))
  }
}

# Combine extracted data into a structured data frame
final_data <- bind_rows(data_list) %>%
  filter(BIN != "3000000" & BIN != "2000000") %>%
  filter(as.numeric(building_number) <55)

# Display final extracted data
print(final_data)

final_data %>%
  mutate(converted_date = mdy(converted_date)) %>%
  filter(year(converted_date) <= 2023) %>%
  select(tds_number) %>%
  n_distinct() # same as other file. 


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/rad-pact")
write.fst(final_data, "pact_bin_data.fst")
