
#### Libraries ####
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

#### Setup ####

root  <- "C:/Users/bunch/OneDrive - The Pennsylvania State University/Summer 2026 AI Monitoring Data/State College/MothBox"
sites <- c("Rock Springs", "Veggie Celler")
dates <- seq(as.Date("2026-07-08"), as.Date("2026-07-12"), by = "day")

stopifnot(dir.exists(root))

#### Locate processed export CSVs ####
# Layout: <site>/_processed/<device>_<YYYY-MM-DD>/04_exports/*_exported-*.csv

find_exports <- function(site) {
  proc_dir <- file.path(root, site, "_processed")
  if (!dir.exists(proc_dir)) {
    message("Missing folder: ", proc_dir)
    return(tibble())
  }

  # Night folders directly under _processed, e.g. colorGnat_2026-07-08
  night_dirs <- list.dirs(proc_dir, recursive = FALSE, full.names = TRUE)
  night_tbl <- tibble(night_dir = night_dirs) %>%
    mutate(
      folder = basename(night_dir),
      device = str_match(folder, "^([A-Za-z]+)_\\d{4}-\\d{2}-\\d{2}$")[, 2],
      night  = as.Date(str_match(folder, "_(\\d{4}-\\d{2}-\\d{2})$")[, 2])
    ) %>%
    filter(!is.na(night), night %in% dates)

  if (nrow(night_tbl) == 0) {
    message(site, ": no night folders in date range")
    return(tibble())
  }

  night_tbl %>%
    mutate(path = map(night_dir, ~ list.files(file.path(.x, "04_exports"),
                                              pattern = "\\.csv$",
                                              full.names = TRUE))) %>%
    unnest(path) %>%
    mutate(site = site, file = basename(path)) %>%
    select(site, device, night, file, path)
}

csv_index <- map_dfr(sites, find_exports)

# Keep the most recent export if a night was exported more than once
if (nrow(csv_index) > 0) {
  csv_index <- csv_index %>%
    mutate(mtime = file.mtime(path)) %>%
    group_by(site, device, night) %>%
    slice_max(mtime, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(site, night)
}

#### Coverage check ####

coverage <- expand_grid(site = sites, night = dates) %>%
  left_join(select(csv_index, any_of(c("site", "night", "file"))),
            by = c("site", "night"))
print(coverage, n = Inf)

missing <- filter(coverage, is.na(file))
if (nrow(missing) > 0) {
  message("No processed export CSV for:\n",
          paste(" ", missing$site, missing$night, collapse = "\n"))
}

#### Read and combine ####

moth_data <- csv_index %>%
  mutate(data = map(path, ~ read_csv(.x,
                                     col_types = cols(.default = col_character()),
                                     show_col_types = FALSE))) %>%
  select(site, device, night, source_file = file, data) %>%
  unnest(data) %>%
  type_convert()

#### Fill nights with no CSV from local processed records ####
# Some nights (e.g., Rock Springs) have no export CSV synced locally, but the same
# classifications are in 02_records/current-classifications.ndjson.
# Parsed with regex so no extra packages are needed.

json_field <- function(lines, key) {
  str_match(lines, paste0('"', key, '":"([^"]*)"'))[, 2]
}

read_records_night <- function(site, folder, device, night) {
  f <- file.path(root, site, "_processed", folder, "02_records",
                 "current-classifications.ndjson")
  if (!file.exists(f)) {
    message(site, " ", night, ": no current-classifications.ndjson")
    return(tibble())
  }

  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(lines)]
  ts <- str_match(json_field(lines, "patch_id"),
                  "_(\\d{4})_(\\d{2})_(\\d{2})__(\\d{2})_(\\d{2})_(\\d{2})_")

  tibble(
    site           = site,
    device         = device,
    night          = night,
    source_file    = basename(f),
    occurrenceID   = json_field(lines, "patch_id"),
    name           = json_field(lines, "label"),
    kingdom        = json_field(lines, "kingdom"),
    phylum         = json_field(lines, "phylum"),
    class          = json_field(lines, "class"),
    order          = json_field(lines, "order"),
    family         = json_field(lines, "family"),
    genus          = json_field(lines, "genus"),
    scientificName = json_field(lines, "scientificName"),
    eventDate      = as.Date(paste(ts[, 2], ts[, 3], ts[, 4], sep = "-")),
    eventTime      = parse_time(paste(ts[, 5], ts[, 6], ts[, 7], sep = ":"))
  )
}

fill_missing <- function(site) {
  missing_nights <- missing$night[missing$site == site]
  if (length(missing_nights) == 0) return(tibble())

  proc_dir <- file.path(root, site, "_processed")
  tibble(folder = basename(list.dirs(proc_dir, recursive = FALSE))) %>%
    mutate(
      device = str_match(folder, "^([A-Za-z]+)_\\d{4}-\\d{2}-\\d{2}$")[, 2],
      night  = as.Date(str_match(folder, "_(\\d{4}-\\d{2}-\\d{2})$")[, 2])
    ) %>%
    filter(!is.na(device), night %in% missing_nights) %>%
    pmap_dfr(function(folder, device, night) read_records_night(site, folder, device, night))
}

records_data <- map_dfr(sites, fill_missing)
moth_data    <- bind_rows(moth_data, records_data)

glimpse(moth_data)
print(count(moth_data, site, night, source_file))

#### Insecta by site by hour ####
library(ggplot2)

# Night order: 12:00 through 11:00 next day
hour_levels <- sprintf("%02d:00", c(12:23, 0:11))

# Nights with data per site, for per-night means
nights_per_site <- moth_data %>%
  distinct(site, night) %>%
  count(site, name = "n_nights")

insecta_hourly <- moth_data %>%
  filter(class == "Insecta") %>%
  mutate(
    hour = sprintf("%02d:00", as.integer(substr(as.character(eventTime), 1, 2))),
    # One cluster/subcluster = repeated detections of one individual;
    # fall back to the detection ID where cluster info is missing
    individual = if_else(is.na(cluster_ID),
                         as.character(occurrenceID),
                         paste(cluster_ID, temporal_subcluster_ID, sep = "_"))
  ) %>%
  group_by(site, night, hour) %>%
  summarise(n_individuals = n_distinct(individual), .groups = "drop") %>%
  group_by(site, hour) %>%
  summarise(total = sum(n_individuals), .groups = "drop") %>%
  complete(site = sites, hour, fill = list(total = 0)) %>%
  left_join(nights_per_site, by = "site") %>%
  mutate(
    mean_per_night = total / n_nights,
    hour = factor(hour, levels = hour_levels)
  ) %>%
  droplevels() %>%
  arrange(site, hour)

print(insecta_hourly, n = Inf)

p_insecta_hourly <- ggplot(insecta_hourly,
                           aes(x = hour, y = mean_per_night,
                               color = site, group = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Hour (local time)",
    y = "Unique Insecta clusters per night (mean)",
    color = "Site",
    title = "Insecta by hour",
    subtitle = paste(format(min(dates), "%b %d"), "to", format(max(dates), "%b %d, %Y"),
                     "| repeat detections collapsed by cluster")
  ) +
  theme_bw() +
  theme(legend.position = "top")

print(p_insecta_hourly)

dir.create("figures", showWarnings = FALSE)
ggsave(file.path("figures", "insecta_by_site_by_hour.png"),
       p_insecta_hourly, width = 8, height = 5, dpi = 300)

#### Insecta composition by order, family, genus ####

# Insecta detections with an individual ID (same cluster logic as above)
# and taxon names standardized to sentence case (e.g. "miridae" -> "Miridae")
insecta <- moth_data %>%
  filter(class == "Insecta") %>%
  mutate(
    individual = if_else(is.na(cluster_ID),
                         as.character(occurrenceID),
                         paste(cluster_ID, temporal_subcluster_ID, sep = "_")),
    across(c(order, family, genus, species), ~ str_to_sentence(str_trim(.x))),
    # Some exports store only the epithet ("cinereomedia"); rebuild the binomial
    species = case_when(
      is.na(species) | species == "" ~ NA_character_,
      str_detect(species, " ")       ~ species,
      !is.na(genus)                  ~ paste(genus, str_to_lower(species)),
      TRUE                           ~ NA_character_
    )
  )

# Flag non-insect orders that were labelled class Insecta (e.g. Araneae)
non_insect_orders <- c("Araneae", "Opiliones", "Acari", "Isopoda")
bad_rows <- filter(insecta, order %in% non_insect_orders)
if (nrow(bad_rows) > 0) {
  message(nrow(bad_rows), " detections labelled Insecta have a non-insect order: ",
          paste(unique(bad_rows$order), collapse = ", "), " (excluded)")
  insecta <- filter(insecta, !order %in% non_insect_orders)
}

plot_rank <- function(rank, top_n = 15) {
  # Share of individuals identified to this rank, per site
  id_rate <- insecta %>%
    distinct(site, night, individual, .keep_all = TRUE) %>%
    group_by(site) %>%
    summarise(pct = round(100 * mean(!is.na(.data[[rank]]) & .data[[rank]] != ""))) %>%
    mutate(lab = paste0(site, " ", pct, "%"))

  by_taxon <- insecta %>%
    filter(!is.na(.data[[rank]]), .data[[rank]] != "") %>%
    group_by(site, night, taxon = .data[[rank]]) %>%
    summarise(n_individuals = n_distinct(individual), .groups = "drop") %>%
    group_by(site, taxon) %>%
    summarise(total = sum(n_individuals), .groups = "drop") %>%
    complete(site = sites, taxon, fill = list(total = 0)) %>%
    left_join(nights_per_site, by = "site") %>%
    mutate(mean_per_night = total / n_nights)

  # Keep the top N taxa by combined mean across sites
  top_taxa <- by_taxon %>%
    group_by(taxon) %>%
    summarise(overall = sum(mean_per_night), .groups = "drop") %>%
    slice_max(overall, n = top_n, with_ties = FALSE)

  plot_df <- by_taxon %>%
    filter(taxon %in% top_taxa$taxon) %>%
    mutate(taxon = factor(taxon, levels = rev(top_taxa$taxon)))

  print(arrange(plot_df, desc(mean_per_night)), n = Inf)

  rank_title <- str_to_sentence(rank)

  p <- ggplot(plot_df, aes(x = mean_per_night, y = taxon, fill = site)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    labs(
      x = "Unique Insecta clusters per night (mean)",
      y = NULL,
      fill = "Site",
      title = paste("Insecta by", tolower(rank_title)),
      subtitle = paste0(
        format(min(dates), "%b %d"), " to ", format(max(dates), "%b %d, %Y"),
        " | top ", nrow(top_taxa), " | identified to ", tolower(rank_title), ": ",
        paste(id_rate$lab, collapse = ", ")
      )
    ) +
    theme_bw() +
    theme(legend.position = "top")

  print(p)

  ggsave(file.path("figures", paste0("insecta_by_site_by_", rank, ".png")),
         p, width = 8, height = 6, dpi = 300)

  invisible(p)
}

p_order  <- plot_rank("order")
p_family <- plot_rank("family")
p_genus  <- plot_rank("genus")
p_species <- plot_rank("species")
