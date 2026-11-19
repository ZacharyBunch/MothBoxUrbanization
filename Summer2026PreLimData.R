
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

stopifnot("No CSVs found; check paths above" = nrow(csv_index) > 0)

moth_data <- csv_index %>%
  mutate(data = map(path, ~ read_csv(.x,
                                     col_types = cols(.default = col_character()),
                                     show_col_types = FALSE))) %>%
  select(site, device, night, source_file = file, data) %>%
  unnest(data) %>%
  type_convert()

glimpse(moth_data)
print(count(moth_data, site, night))

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

# print() is required for plots to display when the script is sourced
print(p_insecta_hourly)

dir.create("figures", showWarnings = FALSE)
ggsave(file.path("figures", "insecta_by_site_by_hour.png"),
       p_insecta_hourly, width = 8, height = 5, dpi = 300)
