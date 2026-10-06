library(tidyverse)
library(knitr)

# 1. Load cleaned dataset
ammo <- readRDS("results/ammo_clean.rds")

# 2. Basic dataset check
glimpse(ammo)

# 3. Overall price summary per round
ammo %>%
  filter(!is.na(ppr) & ppr > 0) %>%
  summarise(
    total_listings = n(),
    mean_ppr       = mean(ppr, na.rm = TRUE),
    median_ppr     = median(ppr, na.rm = TRUE),
    min_ppr        = min(ppr, na.rm = TRUE),
    max_ppr        = max(ppr, na.rm = TRUE),
    iqr_ppr        = IQR(ppr, na.rm = TRUE)
  )

# 4. Price comparison: Lead vs. Lead-Free by Caliber
lead_vs_free_summary <- ammo %>%
  filter(!is.na(category), !is.na(caliber), !is.na(ppr)) %>%
  group_by(caliber, category) %>%
  summarise(
    count      = n(),
    median_ppr = median(ppr, na.rm = TRUE),
    mean_ppr   = mean(ppr, na.rm = TRUE),
    sd_ppr     = sd(ppr, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(caliber, category)

print(lead_vs_free_summary, n = 32)

# Reshape data to plot differences cleanly
plot_data <- lead_vs_free_summary %>%
  select(caliber, category, median_ppr) %>%
  pivot_wider(names_from = category, values_from = median_ppr) %>%
  rename(Lead_Core = `Lead-Core`, Lead_Free = `Lead-Free`) %>%
  mutate(
    premium = Lead_Free - Lead_Core,
    # Reorder caliber by the Lead-Free price for a logical visual flow
    caliber = reorder(caliber, Lead_Free)
  )


# Dumbbell Chart ------------------------------------------------------------------------------------------------------------------------------------------

ggplot(plot_data) %>% { print(.) } +
  # Connecting line segment
  geom_segment(
    aes(x = Lead_Core, xend = Lead_Free, y = caliber, yend = caliber),
    color = "#a0a0a0", size = 1.2
  ) +
  # Lead-Core points
  geom_point(
    aes(x = Lead_Core, y = caliber, color = "Lead-Core"),
    size = 3
  ) +
  # Lead-Free points
  geom_point(
    aes(x = Lead_Free, y = caliber, color = "Lead-Free"),
    size = 3
  ) +
  # Text label showing the calculated price delta
  geom_text(
    aes(
      # Set X position 0.10 to the right of whichever price is higher
      x = pmax(Lead_Free, Lead_Core, na.rm = TRUE) + 0.1, 
      y = caliber, 
      # Format positive premiums as "+$0.50" and negative as "-$0.25"
      label = ifelse(premium >= 0, sprintf("+$%.2f", premium), sprintf("-$%.2f", abs(premium)))
    ),
    hjust = 0, 
    size = 3.5, 
#    fontface = "bold", 
    color = "#2c3e50"
  ) +
  scale_color_manual(
    name = "Ammunition Type",
    values = c("Lead-Core" = "#2b5c8f", "Lead-Free" = "#d95f02")
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    limits = c(1, 4.5),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  labs(
    title = "Median Price per Round: Lead-Core vs. Lead-Free",
    subtitle = "Comparing factory ammunition prices across common big game calibers",
    x = "Median Price per Round (USD)",
    y = NULL,
    caption = "Source: Scraped Retail Ammunition Data From Major Colorado Retailers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee", linetype = "dashed"),
    plot.title = element_text(face = "bold", size = 14, hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.text.y = element_text(face = "bold", size = 9),
    plot.caption = element_text(size = 8)
  )

ggsave(
  filename = "results/lead_core_vs_free_pricing.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Median Price Table --------------------------------------------------------------------------------------------------------------------------------------

formatted_table <- plot_data %>%
  arrange(desc(Lead_Free)) %>%
  mutate(
    Lead_Core = sprintf("$%.2f", Lead_Core),
    Lead_Free = sprintf("$%.2f", Lead_Free),
    # Move $ before or outside the format string for sign handling
    premium   = ifelse(premium >= 0, sprintf("+$%.2f", premium), sprintf("-$%.2f", abs(premium)))
  ) %>%
  rename(
    Caliber = caliber,
    `Lead-Core ($/rd)` = Lead_Core,
    `Lead-Free ($/rd)` = Lead_Free,
    `Lead-Free Premium` = premium
  )

kable(formatted_table, align = c("l", "r", "r", "r"), caption = "Ammunition Pricing Comparison by Caliber")

# Availability & Options ----------------------------------------------------------------------------------------------------------------------------------

# 1. Deduplicate by MPN (falling back to product name if MPN is missing)
unique_ammo <- ammo_clean %>%
  # Fill missing MPNs with name so distinct() has a reliable key
  filter(!str_detect(style, regex("FMJ|Full Metal", ignore_case = TRUE)),
         !str_detect(name, regex("FMJ|Full Metal", ignore_case = TRUE))) %>%
  mutate(dedup_id = coalesce(upc, name)) %>%
  distinct(dedup_id, .keep_all = TRUE)

write_xlsx(unique_ammo, path = "results/unique_ammo.xlsx")

# 2. Re-aggregate counts based on unique offerings
availability_data <- unique_ammo %>%
  group_by(caliber, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(caliber) %>%
  mutate(total_count = sum(count)) %>%
  ungroup() %>%
  mutate(caliber = reorder(caliber, total_count))

# 3. Build and save the stacked bar chart
p_availability <- ggplot(availability_data, aes(x = count, y = caliber, fill = category)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(count > 10, count, "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    name = "Ammunition Type",
    values = c("Lead-Core" = "#2b5c8f", "Lead-Free" = "#d95f02")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Market Availability: Distinct Products by Caliber",
    subtitle = "Unique product offerings (deduplicated by MPN/Name)",
    x = "Distinct Products Available",
    y = NULL,
    caption = "Source: Scraped Retail Ammunition Data From Major Colorado Retailers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold")
  )

p_availability

ggsave(
  filename = "results/caliber_availability_distinct.png",
  plot = p_availability,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)



# Median Price

