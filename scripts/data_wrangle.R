# Load necessary libraries
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)

# 1. Create a list of all CSV file paths in the 'data' directory
file_paths <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)

# 2. Read and combine all files, forcing all columns to character type, into a single dataframe
# Adding an 'id' column helps track which file each row originated from, 
# which can be useful for debugging scraping errors later.
ammo_data <- file_paths %>%
  set_names(basename(file_paths)) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c")), .id = "source_file")

# Check the initial dimensions and column names
dim(ammo_data)


ammo_clean_step1 <- ammo_data %>%
  mutate(
    # 1. Convert string to Date handling both YYYY-MM-DD and M/D/Y formats
    date = as.Date(parse_date_time(date, orders = c("ymd", "mdy"))),
    
    # 2. Extract numeric values for pricing and inventory
    price = parse_number(price),
    qty = parse_number(qty),
    ppr = parse_number(ppr),
    weight = parse_number(weight),
    
    # 3. Extract numeric values for ballistics (safely returns NA for missing data)
    velocity = parse_number(velocity),
    energy = parse_number(energy),
    bc = as.numeric(bc), 
    sd = as.numeric(sd)  
  ) %>%
  # 4. Standardize text casing for categorical groupings
  mutate(
    across(c(retailer, brand, category, stock), ~trimws(tools::toTitleCase(tolower(.))))
  )




# Your provided list of valid brands
valid_brands <- c('Hornady', 'Barnes', 'Nosler', 'Federal', 'Winchester', 'Remington', 
                  'Norma', 'Fiocchi', 'Sig Sauer', 'Browning', 'Sellier & Bellot', 'S&B', 
                  'PMC', 'PPU', 'Prvi Partizan', 'Berger', 'Lapua', 'Sterling', 'Weatherby',
                  'Sierra', 'Speer', 'Swift', 'Black Hills', 'HSM', 'Buffalo Bore', 
                  'Underwood', 'DoubleTap', 'Sako', "Herter's", 'CCI', 'Aguila', 
                  'Magtech', 'Armscor', 'Igman', 'Choice', 'Atomic', 'Black Sheep',
                  'Fort Scott')

# Sort the list by string length descending. 
# This ensures that "Choice Ammunition" is checked before "Choice" (if it existed), preventing partial matches.
brands_regex <- paste(valid_brands[order(-nchar(valid_brands))], collapse = "|")

ammo_clean_step2 <- ammo_clean_step1 %>%
  mutate(
    # 1. Extract the first matching brand from the 'name' column, ignoring case
    extracted_brand = str_extract(name, regex(brands_regex, ignore_case = TRUE)),
    
    # 2. Map the extracted text back to the exact casing in your valid_brands list
    brand_corrected = valid_brands[match(tolower(extracted_brand), tolower(valid_brands))],
    
    # 3. Overwrite the original brand. 
    # coalesce() uses the original scraped brand as a fallback if no match was found in the name.
    brand = coalesce(brand_corrected, brand)
  ) %>%
  # 4. Remove the temporary columns used for the calculation
  select(-extracted_brand, -brand_corrected)

# Check the results to see the new distribution of brands
table(ammo_clean_step1$brand, useNA = "ifany")
table(ammo_clean_step2$brand, useNA = "ifany")


# 1. Target vector of valid canonical calibers
valid_calibers <- c(
  "17 Mach 2", "17 HMR", "17 WSM", "17 Hornet", "17 Rem",
  "204 Ruger",
  "22 Short", "22 LR", "22 WMR", "22 Hornet", "222 Rem", "223 Rem", "5.56x45mm NATO", "22-250 Rem", "224 Valkyrie",
  "243 Win", "6mm ARC", "6mm Creedmoor", "6mm Rem",
  "25-06 Rem", "257 Roberts", "257 Weatherby Mag",
  "6.5 Grendel", "6.5x55mm Swedish", "260 Rem", "6.5 Creedmoor", "6.5 PRC", "264 Win Mag",
  "6.8 SPC", "270 Win", "270 WSM", "6.8 Western",
  "7mm-08 Rem", "7x57mm Mauser", "280 Rem", "280 Ackley Improved", "7mm Rem Mag", "7mm PRC", "28 Nosler",
  "30 Carbine", "300 Blackout", "30-30 Win", "308 Win", "7.62x51mm NATO", "30-06 Springfield", "300 WSM", "300 Win Mag", "300 PRC", "300 Weatherby Mag", "300 RUM",
  "32 ACP", "7.62x39mm", "7.62x54mmR", "303 British",
  "8x57mm Mauser",
  "338 Federal", "338 Win Mag", "338 Lapua Mag",
  "380 ACP", "9mm Luger", "38 Special", "357 Magnum", "350 Legend", "35 Remington", "35 Whelen",
  "375 H&H Mag",
  "40 S&W", "10mm Auto",
  "44 Special", "44 Magnum",
  "45 ACP", "45 Colt", "450 Bushmaster", "45-70 Gov",
  "50 Beowulf", "500 S&W Mag", "50 BMG"
)

# 2. Construct regex patterns for matching common scraped variations
caliber_patterns <- tibble(canonical = valid_calibers) %>%
  mutate(
    pattern = canonical %>%
      str_replace_all("\\.", "\\\\.") %>%
      str_replace_all("Win", "Win(chester)?") %>%
      str_replace_all("Rem", "Rem(ington)?") %>%
      str_replace_all("Mag", "Mag(num)?") %>%
      str_replace_all("Gov", "Gov(ernment)?") %>%
      str_replace_all("Springfield", "(Springfield)?") %>%
      str_replace_all(" ", "[ -]?") %>%
      { paste0("(?:\\b|\\.)", ., "\\b") }
  ) %>%
  # Sort by character length descending so specific calibers match before shorter substrings
  arrange(desc(nchar(canonical)))

# Helper function to match text against prioritized patterns
extract_canonical_caliber <- function(text_vec, patterns_df) {
  res <- rep(NA_character_, length(text_vec))
  for (i in seq_len(nrow(patterns_df))) {
    unmatched <- is.na(res)
    if (!any(unmatched)) break
    
    pat <- patterns_df$pattern[i]
    can <- patterns_df$canonical[i]
    
    matched <- str_detect(text_vec[unmatched], regex(pat, ignore_case = TRUE))
    res[unmatched][matched] <- can
  }
  res
}

# 3. Apply caliber matching and perform Diff Analysis
ammo_clean_step3 <- ammo_clean_step2 %>%
  mutate(
    # Extract canonical caliber from product 'name', fallback to existing 'caliber'
    caliber_new = extract_canonical_caliber(name, caliber_patterns),
    caliber_new = coalesce(caliber_new, extract_canonical_caliber(caliber, caliber_patterns)),
    caliber_new = coalesce(caliber_new, caliber),
    caliber_update = caliber_new != caliber
  )

# --- COMPARISON ---
cal_comparison <- ammo_clean_step3 %>%
  filter(caliber_update == TRUE) %>%
  select(name, caliber, caliber_new)

# --- DIFF COMPARISON REPORT ---
diff_report <- ammo_clean_step3 %>%
  filter(caliber != caliber_new | (is.na(caliber) & !is.na(caliber_new))) %>%
  count(original_caliber = caliber, updated_caliber = caliber_new, name = "count") %>%
  arrange(desc(count))

total_updated_rows <- sum(diff_report$count)

# Output summary counts
cat("Total rows updated/fixed:", total_updated_rows, "\n\n")
print(diff_report, n = 30)

# Finalize column update
ammo_clean_step3 <- ammo_clean_step3 %>%
  mutate(caliber = caliber_new) %>%
  select(-caliber_new, -caliber_update)
