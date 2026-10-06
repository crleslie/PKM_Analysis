# ==========================================
# lookups.R
# ==========================================
library(dplyr)
library(stringr)

# --- 1. BRANDS ---
valid_brands <- c('Hornady', 'Barnes', 'Nosler', 'Federal', 'Winchester', 'Remington', 
                  'Norma', 'Fiocchi', 'Sig Sauer', 'Browning', 'Sellier & Bellot', 'S&B', 
                  'PMC', 'PPU', 'Prvi Partizan', 'Berger', 'Lapua', 'Sterling', 'Weatherby',
                  'Sierra', 'Speer', 'Swift', 'Black Hills', 'HSM', 'Buffalo Bore', 
                  'Underwood', 'DoubleTap', 'Sako', "Herter's", 'CCI', 'Aguila', 
                  'Magtech', 'Armscor', 'Igman', 'Choice', 'Atomic', 'Black Sheep',
                  'Fort Scott')

# Sort brands from longest to shortest name, then collapse into a single regex string
# separated by "|" (OR). Sorting ensures "Choice Ammunition" is matched before "Choice".
brands_regex <- paste(valid_brands[order(-nchar(valid_brands))], collapse = "|")

# --- 2. CALIBERS ---
valid_calibers <- c(
  "17 Mach 2", "17 HMR", "17 WSM", "17 Hornet", "17 Rem", "204 Ruger",
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

# Build a dataframe that pairs the clean caliber name with a flexible search pattern
caliber_patterns <- tibble(canonical = valid_calibers) %>%
  mutate(
    pattern = canonical %>%
      str_replace_all("\\.", "\\\\.") %>%             # Treat periods as actual periods
      str_replace_all("Win", "Win(chester)?") %>%     # Match "Win" OR "Winchester"
      str_replace_all("Rem", "Rem(ington)?") %>%      
      str_replace_all("Mag", "Mag(num)?") %>%
      str_replace_all("Gov", "(Gov(ernment)?)?") %>%  # Make "Gov/Government" optional for 45-70
      str_replace_all("Springfield", "(Springfield)?") %>%
      str_replace_all(" ", "[ -]?") %>%               # Treat spaces as optional spaces or hyphens
      
      # Wrap pattern in word boundaries
      { paste0("(?:\\b|\\.)", ., "\\b") }
  ) %>%
  # IMPORTANT: Sort by the length of the REGEX PATTERN, not the canonical string length
  mutate(pattern_len = nchar(pattern)) %>%
  arrange(desc(pattern_len)) %>%
  select(-pattern_len)

# Map common raw strings and acronyms to a canonical bullet style category
bullet_style_patterns <- c(
  # Polymer Tipped / Proprietary Tipped
  "ELD-X"                      = "ELD-X|ELDX",
  "ELD-M"                      = "ELD-M|ELDM",
  "V-MAX"                      = "V-MAX|VMAX",
  "A-MAX"                      = "A-MAX|AMAX",
  "TMK"                        = "TMK|Tipped MatchKing",
  "SMK"                        = "SMK|Sierra MatchKing",
  "SST"                        = "SST",
  "TTSX"                       = "TTSX|Tipped TSX",
  "CX"                         = "\\bCX\\b",
  "Ballistic Tip"              = "Ballistic Tip",
  "Polymer Tip"                = "Polymer Tipped|Polymer Tip",
  "Tipped Solid Copper"        = "Tipped Solid Copper",
  
  # Hollow Point & Sub-variants
  "Jacketed Hollow Point"      = "Jacketed Hollow Point|JHP|SJHP|Semi-Jacketed Hollow Point",
  "Hollow Point Boat Tail"     = "Hollow Point Boat Tail|Hollow Point Boat-Tail|HPBT|BTHP",
  "Hollow Point"               = "Hollow Point|\\bHP\\b",
  
  # Soft Point & Sub-variants
  "Pointed Soft Point"         = "Pointed Soft Point|\\bPSP\\b",
  "Jacketed Soft Point"        = "Jacketed Soft Point|\\bJSP\\b",
  "Soft Point"                 = "Soft Point|\\bSP\\b|Spire Point",
  
  # Open Tip / Match
  "Open Tip Match"             = "Open Tip Match|\\bOTM\\b",
  
  # Full Metal Jacket & Solid/Plated
  "Total Metal Jacket"         = "Total Metal Jacket|\\bTMJ\\b",
  "Full Metal Jacket Boat Tail"= "Full Metal Jacket Boat Tail|FMJBT|FMJ-BT",
  "Full Metal Jacket"          = "Full Metal Jacket|\\bFMJ\\b",
  
  # Lead / Cast / Nose Profiles
  "Lead Round Nose"            = "Lead Round Nose|\\bLRN\\b",
  "Round Nose"                 = "Round Nose|\\bRN\\b",
  "Flat Nose"                  = "Flat Nose|\\bFN\\b",
  "Wadcutter"                  = "Semi-Wadcutter|Wadcutter",
  "Hard Cast"                  = "Hard Cast",
  "Monolithic Solid"           = "Monolithic|Solid Copper",
  "Boat Tail"                  = "Boat Tail"
)
