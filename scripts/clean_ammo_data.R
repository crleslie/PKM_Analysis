# ==========================================
# clean_ammo_data.R
# ==========================================

# --- 1. SETUP & LIBRARIES ---
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(writexl)

options(scipen = 999)

# Load the lists and regex patterns from the other file
source("scripts/lookups.R")

# --- 2. HELPER FUNCTIONS ---
# This function applies the caliber search patterns to a column of text
extract_canonical_caliber <- function(text_vec, patterns_df) {
  # Create an empty result vector the exact same length as the input data
  res <- rep(NA_character_, length(text_vec))
  
  # Loop through our search patterns one by one (starting with the longest)
  for (i in seq_len(nrow(patterns_df))) {
    
    # Identify which rows in 'res' are still NA (haven't found a match yet)
    unmatched <- is.na(res)
    if (!any(unmatched)) break # Stop early if every row has a caliber
    
    pat <- patterns_df$pattern[i]
    can <- patterns_df$canonical[i]
    
    # Test only the remaining unmatched text against the current pattern
    matched <- str_detect(text_vec[unmatched], regex(pat, ignore_case = TRUE))
    
    # Double subsetting: Take the blank rows, find the ones that just matched, 
    # and overwrite them with the clean canonical name.
    res[unmatched][matched] <- can
  }
  res
}

# --- 3. CLEANING FUNCTIONS ---
clean_types_and_dates <- function(df) {
  df %>%
    mutate(
      # Parse mixed date formats. The result is wrapped in as.Date to drop times.
      date = as.Date(parse_date_time(date, orders = c("ymd", "mdy"))),
      
      # Bulk apply parse_number() to extract the first numeric value found in these columns
      across(c(price, qty, ppr, weight, velocity, energy), parse_number),
      
      # Bulk apply as.numeric() to decimals
      across(c(bc, sd), as.numeric),
      
      # Bulk apply a custom formatting chain using the '~' (lambda) syntax.
      # The '.' represents the data in the current column being processed.
      across(c(retailer, stock, category), ~trimws(tools::toTitleCase(tolower(.))))
    )
}

clean_brands <- function(df) {
  df %>%
    mutate(
      # Search the product name for the first brand match
      extracted_brand = str_extract(name, regex(brands_regex, ignore_case = TRUE)),
      
      # Fix the capitalization by matching it exactly to your valid_brands list
      brand_corrected = valid_brands[match(tolower(extracted_brand), tolower(valid_brands))],
      
      # Use the corrected brand. If it's NA, fallback to the original scraped brand
      brand = coalesce(brand_corrected, brand),
      
      # Standardize specific aliases into their full brand names
      brand = case_when(
        brand == "Choice" ~ "Choice Ammunition",
        brand == "S&B" ~ "Sellier & Bellot",
        brand == "PPU" ~ "Prvi Partizan",
        TRUE ~ brand # Keep all other brands as they are
      )
    ) %>%
    # Drop the temporary calculation columns
    select(-extracted_brand, -brand_corrected)
}

clean_calibers <- function(df) {
  df %>%
    mutate(
      # 1. Try to find the caliber hidden inside the product name
      caliber_new = extract_canonical_caliber(name, caliber_patterns),
      
      # 2. If NA, try to extract a clean caliber from the scraped caliber column
      caliber_new = coalesce(caliber_new, extract_canonical_caliber(caliber, caliber_patterns)),
      
      # 3. If still NA, fallback to the raw scraped caliber string
      caliber_new = coalesce(caliber_new, caliber),
      
      # Overwrite the original column
      caliber = caliber_new
    ) %>%
    select(-caliber_new) %>%
    # Secondary cleaning chained directly onto the main pipeline:
    mutate(
      caliber = case_when(
        caliber == "45-70 Govt" ~ "45-70 Gov",
        TRUE ~ caliber
      )
    ) %>%
    filter(
      !caliber %in% c("5.56x45mm NATO", "28 Nosler", "280 Ackley Improved", "7mm PRC")
    )
}

clean_category <- function(df) {
  df %>%
    mutate(
      category = case_when(
        # 1. Explicit Overrides (Must go first)
        # Forces Barnes Harvest Collection to Lead-Core, regardless of other words in the name
        brand == "Barnes" & str_detect(name, regex("Harvest Collection", ignore_case = TRUE)) ~ "Lead-Core",
        
        # 2. Positive identifiers for Lead-Free
        # Using word boundaries (\\b) around cx ensures we don't accidentally match "cx" inside another word
        str_detect(name, regex("lead[- ]free|copper|\\bcx\\b", ignore_case = TRUE)) ~ "Lead-Free",
        
        # 3. Fallback to the originally scraped category if no rules were met
        TRUE ~ category
      )
    )
}

clean_identifiers <- function(df) {
  df %>%
    mutate(
      # Force MPN to uppercase and remove stray spaces
      mpn = trimws(toupper(mpn)),
      
      # Keep SKU as character and clean up spacing
      sku = trimws(sku),
      
      # 1. Strip whitespace from UPC
      upc = trimws(upc),
      
      # 2. Fix scientific notation strings (e.g., "1.23457E+11") by converting to numeric,
      # formatting as a plain string with no decimals, and restoring any leading zeros to 12 digits
      upc = case_when(
        str_detect(upc, "[eE]") ~ format(as.numeric(upc), scientific = FALSE, trim = TRUE),
        TRUE ~ upc
      ),
      
      # 3. Pad short UPCs with leading zeros up to 12 digits
      upc = str_pad(upc, width = 12, side = "left", pad = "0")
    )
}

clean_bullet_style <- function(df) {
  # Build a single combined regex pattern from lookups.R
  style_regex <- paste(bullet_style_patterns, collapse = "|")
  
  df %>%
    mutate(
      # Step 1: Blank out suspicious scraped styles (e.g., blanket ELD-X assignments)
      # if "ELD-X" isn't actually mentioned anywhere in the title/name.
      style = case_when(
        style == "ELD-X" & !str_detect(name, regex("ELD-X|ELDX", ignore_case = TRUE)) ~ NA_character_,
        TRUE ~ style
      ),
      
      # Step 2: Extract matching pattern from the 'name' column if 'style' is missing/invalid
      style_from_name = str_extract(name, regex(style_regex, ignore_case = TRUE)),
      
      # Step 3: Map matched raw strings back to canonical keys
      style_canonical = map_chr(style_from_name, function(match) {
        if (is.na(match)) return(NA_character_)
        
        # Find which key in lookups matches the extracted string
        matched_key <- names(bullet_style_patterns)[
          map_lgl(bullet_style_patterns, ~ str_detect(match, regex(.x, ignore_case = TRUE)))
        ]
        
        if (length(matched_key) > 0) matched_key[1] else NA_character_
      }),
      
      # Step 4: Fall back to canonical style from name, then original style, then 'Unknown'
      style = coalesce(style_canonical, style, "Unknown")
    ) %>%
    select(-style_from_name, -style_canonical)
}

# clean_ammo_data.R

impute_ballistics <- function(df) {
  df %>%
    # 1. Build hierarchical grouping key
    mutate(
      primary_group_key = case_when(
        !is.na(brand) & !is.na(mpn) & mpn != "" ~ paste(brand, mpn, sep = "_"),
        !is.na(brand) & !is.na(caliber) & !is.na(weight) & !is.na(style) ~ 
          paste(brand, caliber, weight, style, sep = "_"),
        !is.na(upc) & upc != "" ~ paste("UPC", upc, sep = "_"),
        TRUE ~ NA_character_
      )
    ) %>%
    # 2. Backfill within each matching group
    group_by(primary_group_key) %>%
    mutate(
      across(
        c(velocity, energy, bc, sd),
        ~ {
          vals <- .x[!is.na(.) & . != ""]
          if (length(vals) > 0) vals[1] else .x
        }
      )
    ) %>%
    ungroup() %>%
    select(-primary_group_key)
}

# --- 4. EXECUTION PIPELINE ---
# Get all CSV files in the data folder
file_paths <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)

# map_df loops over file_paths. '.x' represents the current file.
# .id = "source_file" creates a new column containing the originating file name.
raw_data <- file_paths %>%
  set_names(basename(file_paths)) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c")), .id = "source_file")

# Chain the custom functions together into a single pipeline
ammo_clean <- raw_data %>%
  clean_types_and_dates() %>%
  mutate(ppr = price / qty) %>%
  clean_brands() %>%
  clean_calibers() %>%
  clean_category() %>%
  clean_identifiers() %>%
  clean_bullet_style()

# --- Step 2: Run imputation ---
ammo_imputed <- ammo_clean %>%
  impute_ballistics()


evaluate_imputation <- function(df_pre, df_post) {
  ballistic_vars <- c("velocity", "energy", "bc", "sd")
  
  # Calculate metrics for pre-imputation
  pre_stats <- df_pre %>%
    summarise(across(all_of(ballistic_vars), ~ sum(!is.na(.) & . != ""), .names = "pre_{.col}")) %>%
    mutate(
      pre_complete_rows = sum(complete.cases(select(df_pre, all_of(ballistic_vars)))),
      total_rows = nrow(df_pre)
    )
  
  # Calculate metrics for post-imputation
  post_stats <- df_post %>%
    summarise(across(all_of(ballistic_vars), ~ sum(!is.na(.) & . != ""), .names = "post_{.col}")) %>%
    mutate(
      post_complete_rows = sum(complete.cases(select(df_post, all_of(ballistic_vars))))
    )
  
  # Combine into a clean comparison table
  bind_cols(pre_stats, post_stats) %>%
    pivot_longer(
      cols = starts_with(c("pre_", "post_")),
      names_to = c("stage", "variable"),
      names_sep = "_"
    ) %>%
    pivot_wider(names_from = stage, values_from = value) %>%
    mutate(
      gained = post - pre,
      pre_fill_rate = paste0(round((pre / total_rows) * 100, 1), "%"),
      post_fill_rate = paste0(round((post / total_rows) * 100, 1), "%")
    ) %>%
    select(
      Variable = variable,
      `Pre Count` = pre,
      `Pre %` = pre_fill_rate,
      `Post Count` = post,
      `Post %` = post_fill_rate,
      `Net Gain` = gained
    )
}

# Run the evaluation
imputation_summary <- evaluate_imputation(ammo_clean, ammo_imputed)
print(imputation_summary)

# Save the final cleaned/imputed data
saveRDS(ammo_imputed, file = "results/ammo_clean.rds")

write_xlsx(ammo_imputed, path = "results/ammo_clean.xlsx")
