###############################################################
# Assignment 1 — Biodiversity Exploration (Cervidae, BOLD)
# Author: Arwa Sheheryar | Date: Oct 2025
# Goal: Explore BIN & species diversity; compare regions (NA vs EA)
###############################################################

# ================================================================
# RESEARCH QUESTION, MOTIVATION, AND STUDY TYPE
# ---------------------------------------------------------------
# Question:
# How does BIN diversity of deer (Cervidae) differ between North America and Eurasia?
#
# Why this is interesting:
# Deer (Cervidae) shape vegetation, predators, and nutrient cycles across the Holarctic.Comparing BIN diversity between North America and Eurasia helps disentangle geography,history, and sampling. Adjusting for sampling bias keeps differences biological not as artifacts.
#
# Study type:
# Exploratory comparison with a simple confirmatory check.
# - Exploratory: raw BIN richness, effort-adjusted richness (BINs per 100 records),and a geographic diversity map
#
# Hypothesis: Eurasia exhibits higher BIN richness and greater evenness, reflecting deeper evolutionary diversification.
# ================================================================

### PART 1 — LIBRARIES & THEME --------------------------------
  library(tidyverse)    # readr + dplyr + ggplot2 + stringr + forcats
  library(conflicted)   # make function choices explicit
  library(viridis)      # colourblind-friendly palettes for ggplot
#uncomment if needed to install
  #install.packages("iNEXT")
  library(iNEXT)        # coverage-based diversity estimation
#install.packages("vegan")
  library(vegan)        # biodiversity tools (optional later)
#install.packages("Biostrings")
  library(Biostrings)   # sequence-aware objects (COI section, optional)
#install.packages("maps")
  library(maps)         # simple world polygons for basemaps


conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")
conflict_prefer("rename", "dplyr")


theme_set(theme_light())  # consistent, clean plotting theme

### PART 2 — IMPORT (KEEP A SAFE COPY OF ORIGINAL DATA) --------
# Read the data as df_full and keep it untouched original data;for all further processing use copies

df_full <- read_tsv("../data/Cervidae_BOLD.tsv")

#If this was successful, you will see an object in your environment with 2401 observations of 85 variables.

#Have a look at the data check the formatting 

summary(df_full)
class(df_full$coord)   #Here we can see the class of the column coord this will be important for geographical analysis, right now the class is "character"
glimpse(df_full)      # check types and a few example rows
names(df_full)        # exact column names you'll reference later

# (Optional) sanity checks you can keep or remove:
nrow(df_full)        # how many rows (Observations/records) were read?
ncol(df_full)        # how many columns (variables)?
head(df_full, 3)     # the first 3 rows to get an idea of the formatting


### PART 3 — BASIC CLEANUP (coords → lat/lon) ------------------
# Some BOLD exports store coords as TEXT "[lat, lon]".
# Parse to two numeric columns so mapping works.
#   "lat"  = latitude (north–south position)
#   "lon"  = longitude (east–west position)

df_coords <- df_full %>% 
  mutate( 
    # Drop the leading "[" and trailing "]" safely; coerce to character first
    coord_text = stringr::str_sub(as.character(coord), 2, nchar(as.character(coord)) - 1),
    # Split at comma into 2 pieces, trim blank spaces, and convert the resulting character to numeric
    lat = as.numeric(stringr::str_trim(stringr::str_split_fixed(coord_text, ",", 2)[, 1])),
    lon = as.numeric(stringr::str_trim(stringr::str_split_fixed(coord_text, ",", 2)[, 2]))
  )

# Keep a lean analysis of only the columns we need for our research question
bold_sub <- df_coords %>%
  select(processid, bin_uri, family, genus, species,
         region, `country/ocean`, lat, lon, marker_code, nuc)

# Rename to avoid having to use backticks
bold_sub <- bold_sub %>%
  rename(country_ocean = `country/ocean`)

### PART 4 — DEFINE REGIONS (North America vs. Eurasia) --------
# Filter out the NAs in longitude and latitude
# Create a new column using mutate to divide the data into North America and Eurasia using latitude/longitude rules so we can compare them. Manually assigned Aleutian Islands (Alaska) into NA since they technically cross the line into EA but are supposed to be part of NA.
# Filter out the NAs in region and bins

# Rule 1: Northern Hemisphere AND longitude between -170 and -30 → North America
# Rule 2: Northern Hemisphere AND longitude > -30 up to 180 → Eurasia
# Else: leave as NA (no region)

df_use <- bold_sub %>%
  filter(!is.na(lat), !is.na(lon)) %>% 
  mutate(
    region2 = case_when(
      lat >= 0 & lon >= -170 & lon <= -30 ~ "North America",
      lat >= 0 & lon >  -30  & lon <= 180 ~ "Eurasia",
      TRUE ~ NA_character_
    ),
    # # Manual fix of Aleutian chain (>=45°N & >170°E) make it North America.
    region2 = ifelse(lat >= 45 & lon > 170, "North America", region2)
  ) %>% 
  filter(!is.na(region2), !is.na(bin_uri))


# Reproducibility checkpoint (confirms the data has been manipulated correctly and the NAs were removed) 
checkpoint_summary = df_use %>% 
  group_by(region2) %>% 
  summarise(
    total_records = n(),
    missing_lat = sum(is.na(lat)),
    missing_lon = sum(is.na(lon)),
    missing_bin = sum(is.na(bin_uri))
  )

checkpoint_summary

###############################################################
# FIGURE 1 — Geographic distribution of Cervidae BIN records
# Goal: show WHERE records were collected and WHICH BINs occur there
# Color = BIN (top N most common BINs; others → "Other BINs")
# Shape = Region (North America vs Eurasia) to keep context
###############################################################

# 1) Prep points for plotting

df_pts <- df_use %>%
  filter(!is.na(lat), !is.na(lon), !is.na(region2)) %>%
  mutate(
    lon_plot = ifelse(lon > 180, lon - 360, lon),  # fix longitudes > 180
    lat_plot = lat,
    bin_label = coalesce(bin_uri, "BIN unknown")   # readable label for BIN
  )

# 2) Group rare BINs to keep the legend readable (change N as needed)
TOP_N_BINS <- 10
df_pts <- df_pts %>%
  mutate(bin_grp = forcats::fct_lump_n(bin_label, n = TOP_N_BINS, other_level = "Other BINs"))

# 3) Basemap
world <- map_data("world")

# 4) Plot
p_map <- ggplot() +
  geom_polygon(
    data = world, # built-in map data (country outlines)
    aes(long, lat, group = group), #define map structure lon/lat grouped by country
    fill = "grey98", color = "grey80", linewidth = 0.2 #aesthetics of map fill and border
  ) +
  geom_point(
    data = df_pts,
    aes(lon_plot, lat_plot, color = bin_grp, shape = region2), #color points by bin, shape by region
    alpha = 0.6, size = 1.8
  ) +
  coord_quickmap(xlim = c(-170, 180), ylim = c(0, 85)) + #limits of longitude
  scale_color_viridis_d(name = "BIN (top groups)") +
  scale_shape_discrete(name = "Region") +
  labs(
    title    = "Geographic Distribution of Cervidae BIN Records",
    subtitle = paste0("Colors = BIN (top ", TOP_N_BINS, " + 'Other'); Shapes = Region"),
    x = "Longitude", y = "Latitude",
    caption  = "Basemap from {maps}. Longitudes > 180 converted to -180–180."
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 2), #size of color legend and shape legend
    shape = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  theme(panel.grid.minor = element_blank(), #remove minor gridlines clean look
        legend.position  = "bottom")

# 5) Make the BIN legend informative (order by abundance + add counts)
    
 #count how many records each BIN group has and sort most to least frequent
bin_counts <- df_pts %>% count(bin_grp, name = "n_total") %>% arrange(desc(n_total))
#match the legend order/labelling to the data 
df_pts <- df_pts %>% mutate(bin_grp = factor(bin_grp, levels = bin_counts$bin_grp))
bin_breaks <- bin_counts$bin_grp
bin_labels <- paste0(bin_counts$bin_grp, " (n=", bin_counts$n_total, ")")

p_map <- p_map +
  scale_color_viridis_d(name = "BIN (top groups)", breaks = bin_breaks, labels = bin_labels) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 2,
                              title.position = "top"),
         shape = guide_legend(override.aes = list(size = 3, alpha = 1), title.position = "top"))

p_map  # print

###############################################################
# FIGURE 2 — One combined figure:
# Effort-normalized vs Coverage-standardized BIN richness
# - Two methods shown side-by-side for each region
# - iNEXT coverage-standardized bars include 95% CIs
###############################################################

# 1) Effort-normalized (per 100 records)
summary_bins <- df_use %>%
  filter(!is.na(region2)) %>%
  dplyr::group_by(region2) %>%
  dplyr::summarise(
    unique_bins = dplyr::n_distinct(bin_uri),
    n_records   = dplyr::n(),
    bins_per_100 = (unique_bins / n_records) * 100
  ) %>%
  dplyr::ungroup()

# 2) Coverage-standardized (q = 0) via iNEXT
abund_by_region <- df_use %>%
  count(region2, bin_uri, name = "n")

abund_list <- abund_by_region %>%
  group_by(region2) %>%
  summarise(vec = list(setNames(n, bin_uri)), .groups = "drop") %>%
  { setNames(.$vec, .$region2) }

info <- iNEXT::DataInfo(abund_list, datatype = "abundance")
target_cov <- round(min(info$SC), 2)

est_cov <- iNEXT::estimateD(
  abund_list, q = 0, datatype = "abundance",
  base = "coverage", level = target_cov, conf = 0.95
)

fig2_data <- est_cov %>%
  transmute(
    region2  = Assemblage,
    estimate = qD,
    Lower_CL = qD.LCL,
    Upper_CL = qD.UCL
  )

# 3) Combine into a tidy long table for plotting
df_combo <- bind_rows(
  summary_bins %>%
    transmute(
      region2,
      method = "Effort-normalized (per 100 records)",
      value  = bins_per_100,
      lcl    = NA_real_,    # deterministic ratio → no CI
      ucl    = NA_real_
    ),
  fig2_data %>%
    transmute(
      region2,
      method = paste0("Coverage-standardized (q=0, SC≈", target_cov, ")"),
      value  = estimate,
      lcl    = Lower_CL,
      ucl    = Upper_CL
    )
)

# 4) Place labels just above bar tops (or CI tops when present)
pad <- diff(range(df_combo$value, na.rm = TRUE)) * 0.06
df_combo <- df_combo %>%
  mutate(label = round(value, 1),
         label_y = ifelse(is.na(ucl), value + pad, ucl + pad))

# 5) Plot
pd <- position_dodge(width = 0.7)

p_fig4 <- ggplot(df_combo, aes(method, value, fill = region2)) +
  geom_col(width = 0.65, position = pd) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),
                width = 0.14, linewidth = 0.6,
                position = pd, na.rm = TRUE) +
  geom_text(aes(y = label_y, label = label),
            position = pd, size = 3.6, fontface = "bold") +
  scale_y_continuous(limits = c(0, max(df_combo$label_y, na.rm = TRUE) + pad),
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_viridis_d(name = "Region") +
  labs(
    title    = "Cervidae BIN Richness: Effort-Normalized vs Coverage-Standardized",
    subtitle = "Two complementary standardizations shown side-by-side for each region",
    x = NULL,
    y = "BIN richness (per method)",
    caption  = "Coverage-standardized values from iNEXT (q=0) at equal sample coverage; 95% CIs shown where applicable.\nEffort-normalized values are deterministic ratios (no CIs)."
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        axis.text.x      = element_text(size = 9))
p_fig4
###############################################################
# FIGURE 3 — Rank–Abundance Curves of BINs by Region
# Purpose: compare the internal structure of diversity (dominance/evenness)
###############################################################

# Abundance per BIN in each region
rank_abund <- abund_by_region %>%
  group_by(region2) %>%
  arrange(region2, desc(n)) %>% # within each region, sort BINs from most to least frequent
  mutate(rank = row_number()) %>% # give the most common BIN rank = 1, next = 2, etc.
  ungroup()

p_fig3 <- ggplot(rank_abund, aes(rank, n, color = region2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6, alpha = 0.8) +
  scale_y_log10() +
  scale_color_viridis_d(name = "Region") +
  labs(
    title    = "Rank–Abundance Curves of Cervidae BINs by Region",
    subtitle = "Depicts dominance and evenness of BIN distribution within each region",
    x = "BIN rank (1 = most abundant)",
    y = "Number of records (log scale)",
    caption = "Curves based on BIN record frequencies from BOLD; log scale emphasizes rarity patterns."
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")
p_fig3

# Save all figures to figs folder.  ========================
ggsave("../figs/Figure1_Map.png", p_map, width = 10, height = 6, dpi = 300)
ggsave("../figs/Figure2_RichnessComparison.png", p_fig4, width = 9, height = 6, dpi = 300)
ggsave("../figs/Figure3_RankAbundance.png", p_fig3, width = 9, height = 6, dpi = 300)

