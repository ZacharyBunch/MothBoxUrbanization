#### Libraries ####
# install.packages(c("Microsoft365R", "dplyr", "purrr", "readr", "stringr", "tidyr", "jsonlite", "ggplot2"))
library(Microsoft365R)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

#### Setup ####
# Pulls processed export CSVs straight from OneDrive (Microsoft Graph),
# bypassing the local OneDrive sync client.

od_base <- "Summer 2026 AI Monitoring Data/State College/MothBox"
sites   <- c("Rock Springs", "Veggie Celler")
dates   <- seq(as.Date("2026-07-08"), as.Date("2026-07-12"), by = "day")
cache   <- file.path("data", "raw", "mothbox_exports")   # relative to project root

dir.create(cache, recursive = TRUE, showWarnings = FALSE)

#### Connect ####
# First run opens a browser for PSU sign-in; token is cached afterward.
od <- get_business_onedrive()

# Resolve the MothBox folder: own drive path first, then shared-with-me items
get_base_folder <- function(od, path) {
  item <- tryCatch(od$get_item(path), error = function(e) NULL)
  if (!is.null(item)) return(item)

  message("Path not found in your drive; searching shared items...")
  top    <- str_split_1(path, "/")[1]
  shared <- od$list_shared_items()
  hit    <- keep(shared, ~ .x$properties$name == top)
  if (length(hit) == 0) stop("Could not find '", top, "' in your drive or shared items")
  rest <- str_remove(path, paste0("^", top, "/?"))
  if (nzchar(rest)) hit[[1]]$get_item(rest) else hit[[1]]
}

base <- get_base_folder(od, od_base)

#### Locate processed export CSVs ####
# Layout: <site>/_processed/<device>_<YYYY-MM-DD>/04_exports/*_exported-*.csv

find_exports <- function(site) {
  proc <- tryCatch(base$get_item(file.path(site, "_processed")),
                   error = function(e) NULL)
  if (is.null(proc)) {
    message(site, ": no _processed folder")
    return(tibble())
  }

  nights <- proc$list_items() %>%
    as_tibble() %>%
    filter(isdir) %>%
    transmute(
      folder = name,
      device = str_match(folder, "^([A-Za-z]+)_\\d{4}-\\d{2}-\\d{2}$")[, 2],
      night  = as.Date(str_match(folder, "_(\\d{4}-\\d{2}-\\d{2})$")[, 2])
    ) %>%
    filter(!is.na(night), night %in% dates)

  if (nrow(nights) == 0) {
    message(site, ": no night folders in date range")
    return(tibble())
  }

  pmap_dfr(nights, function(folder, device, night) {
    exp_rel <- file.path(site, "_processed", folder, "04_exports")
    exp_dir <- tryCatch(base$get_item(exp_rel), error = function(e) NULL)
    if (is.null(exp_dir)) return(tibble())

    exp_dir$list_items() %>%
      as_tibble() %>%
      filter(!isdir, str_detect(name, "\\.csv$")) %>%
      transmute(site    = site,
                device  = device,
                night   = night,
                file    = name,
                od_path = file.path(exp_rel, name))
  })
}

csv_index <- map_dfr(sites, find_exports)

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

stopifnot("No CSVs found on OneDrive; check paths above" = nrow(csv_index) > 0)

#### Download to local cache ####

csv_index <- csv_index %>%
  mutate(local_path = file.path(cache, file))

walk2(csv_index$od_path, csv_index$local_path, function(src, dest) {
  if (!file.exists(dest)) {
    base$get_item(src)$download(dest, overwrite = TRUE)
    message("Downloaded ", basename(dest))
  }
})

#### Read and combine ####

moth_data <- csv_index %>%
  mutate(data = map(local_path, ~ read_csv(.x,
                                           col_types = cols(.default = col_character()),
                                           show_col_types = FALSE))) %>%
  select(site, device, night, source_file = file, data) %>%
  unnest(data) %>%
  type_convert()

#### Fill nights with no CSV from processed records ####
# Rock Springs nights are classified but were never exported to CSV.
# The same classifications live in 02_records/current-classifications.ndjson.

read_records_night <- function(site, folder, device, night) {
  src  <- file.path(site, "_processed", folder, "02_records",
                    "current-classifications.ndjson")
  dest <- file.path(cache, paste0(folder, "_current-classifications.ndjson"))

  if (!file.exists(dest)) {
    item <- tryCatch(base$get_item(src), error = function(e) NULL)
    if (is.null(item)) {
      message(site, " ", night, ": no current-classifications.ndjson")
      return(tibble())
    }
    item$download(dest, overwrite = TRUE)
    message("Downloaded ", basename(dest))
  }

  lines <- readLines(dest, warn = FALSE)
  recs  <- map(lines[nzchar(lines)], jsonlite::fromJSON, simplifyVector = FALSE)
  ts    <- str_match(map_chr(recs, "patch_id"),
                     "_(\\d{4})_(\\d{2})_(\\d{2})__(\\d{2})_(\\d{2})_(\\d{2})_")

  tibble(
    site        = site,
    device      = device,
    night       = night,
    source_file = basename(src),
    occurrenceID = map_chr(recs, "patch_id"),
    name        = map_chr(recs, ~ .x$label %||% NA_character_),
    kingdom     = map_chr(recs, ~ .x$taxon$kingdom %||% NA_character_),
    phylum      = map_chr(recs, ~ .x$taxon$phylum  %||% NA_character_),
    class       = map_chr(recs, ~ .x$taxon$class   %||% NA_character_),
    order       = map_chr(recs, ~ .x$taxon$order   %||% NA_character_),
    family      = map_chr(recs, ~ .x$taxon$family  %||% NA_character_),
    genus       = map_chr(recs, ~ .x$taxon$genus   %||% NA_character_),
    scientificName = map_chr(recs, ~ .x$taxon$scientificName %||% NA_character_),
    eventDate   = as.Date(paste(ts[, 2], ts[, 3], ts[, 4], sep = "-")),
    eventTime   = parse_time(paste(ts[, 5], ts[, 6], ts[, 7], sep = ":"))
  )
}

fill_missing <- function(site) {
  missing_nights <- missing$night[missing$site == site]
  if (length(missing_nights) == 0) return(tibble())

  proc <- tryCatch(base$get_item(file.path(site, "_processed")),
                   error = function(e) NULL)
  if (is.null(proc)) return(tibble())

  proc$list_items() %>%
    as_tibble() %>%
    filter(isdir) %>%
    transmute(
      folder = name,
      device = str_match(folder, "^([A-Za-z]+)_\\d{4}-\\d{2}-\\d{2}$")[, 2],
      night  = as.Date(str_match(folder, "_(\\d{4}-\\d{2}-\\d{2})$")[, 2])
    ) %>%
    filter(!is.na(device), night %in% missing_nights) %>%
    pmap_dfr(function(folder, device, night) read_records_night(site, folder, device, night))
}

records_data <- map_dfr(sites, fill_missing)

moth_data <- bind_rows(moth_data, records_data)

glimpse(moth_data)
print(count(moth_data, site, night, source_file))

#### Insecta by site by hour ####
library(ggplot2)

# Night order: 12:00 through 11:00 next day
hour_levels <- sprintf("%02d:00", c(12:23, 0:11))

insecta_hourly <- moth_data %>%
  filter(class == "Insecta") %>%
  mutate(hour = sprintf("%02d:00", as.integer(substr(as.character(eventTime), 1, 2)))) %>%
  count(site, hour, name = "n_insecta") %>%
  complete(site = sites, hour, fill = list(n_insecta = 0)) %>%
  mutate(hour = factor(hour, levels = hour_levels)) %>%
  droplevels() %>%
  arrange(site, hour)

print(insecta_hourly, n = Inf)

p_insecta_hourly <- ggplot(insecta_hourly,
                           aes(x = hour, y = n_insecta,
                               color = site, group = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Hour (local time)",
    y = "Insecta detections (summed across nights)",
    color = "Site",
    title = "Insecta detections by hour",
    subtitle = paste(format(min(dates), "%b %d"), "to", format(max(dates), "%b %d, %Y"))
  ) +
  theme_bw() +
  theme(legend.position = "top")

print(p_insecta_hourly)

dir.create("figures", showWarnings = FALSE)
ggsave(file.path("figures", "insecta_by_site_by_hour.png"),
       p_insecta_hourly, width = 8, height = 5, dpi = 300)
