### make_network_study (source, study_name = "") create a network_study file in dir or from csv file
### location_report (network_study, locations = "all") network report for specified locations
### network_report  (network_study, networks  = "all") network report for specified networks
### best_alternatives (network_study, locations = "all") best wired/wifi networks for specified locations
### locations (network_study) list locations
### networks  (network_study) list networks

require ("tools")
require ("rjson")
require ("dplyr")
require ("lubridate")
require ("stringr")
require ("ggplot2")

make_network_study <- function (source = getwd(), study_name = "")
{
    if (!file.exists (source)) stop ("source does not exist")
    if (file.info(source)$isdir) 
        study <- df_from_jsons (list_files_with_exts (dir = source, exts = c ("json", "JSON")))
    else  
        study <- read.csv (source)
      
    if (study_name == "")
        study_name <- sprintf ("%s -- %s network study.csv",
                               format (Sys.time(), "%Y-%m-%d"), file_path_sans_ext (basename (source)))

    study <- mutate (study, 
                     location = as.factor (word (file.name, 1, sep = "_")),
                     network  = as.factor (word (file.name, 2, sep = "_")),
                     wifi = grepl ("airport|mesh|GHz|ghz", network),
                     sent.mbps = round(end.sum_sent.bits_per_second / 1e6, digits=2),
                     received.mbps = round(end.sum_received.bits_per_second / 1e6, digits=2),
                     timestamp = strftime (with_tz (dmy_hms (start.timestamp.time), tz = "America/Chicago"),
                                           format = "%Y-%m-%d %R",
                                           usetz = TRUE)) %>%
        arrange (desc(received.mbps)) %>%
        select (location, network, wifi, sent.mbps, received.mbps, timestamp)

    write.csv (file= study_name, study, row.names = FALSE)
    study
}

location_report <- function (network_study, ...) {UseMethod ("location_report")}

location_report.character <- function (network_study, ...)
{
    location_report.data.frame (read.csv (network_study), ...)
}

location_report.data.frame <- function (network.data, locations = "all")
{
    valid.locations <- levels (network.data$location)  

    if (identical (locations, "all")) locations <- valid.locations
    else for (location in locations) if (!location %in% valid.locations) stop ("invalid location")

    filter (network.data, location %in% locations) %>% 
    arrange (location, desc(received.mbps)) %>%
    select (location, network, everything())
}

network_report <- function (network_study, ...) {UseMethod ("network_report")}

network_report.character <- function (network_study, ...)
{
    network_report.data.frame (read.csv (network_study), ...)
}

network_report.data.frame <- function (network.data, networks = "all")
{
    valid.networks <- levels (network.data$network)  
  
    if (identical (networks, "all")) networks <- valid.networks
    else for (network in networks) if (!network %in% valid.networks) stop ("invalid network")
  
    filter (network.data, network %in% networks) %>% 
    arrange (network, desc(received.mbps)) %>%
    select (network, location, everything())
}

best_alternatives <- function (network_study, ...) {UseMethod ("best_alternatives")}

best_alternatives.character <- function (network_study, ...)
{
    best_alternatives.data.frame (read.csv (network_study), ...)
}

best_alternatives.data.frame <- function (network.data, locations = "all")
{
    valid.locations <- levels (network.data$location)
  
    if (identical (locations, "all")) locations <- valid.locations
    else for (location in locations) if (!location %in% valid.locations) stop ("invalid location")
  
    filter (network.data, location %in% locations) %>%
        group_by (location, wifi) %>% 
        slice (which.max (received.mbps)) %>%
        select (-wifi, everything()) %>%
        as.data.frame ()
}

locations <- function (network_study) {UseMethod ("locations")}
locations.character  <- function (network_study) {levels (read.csv (network_study)$location)}
locations.data.frame <- function (network_study) {levels (network_study$location)}

networks <- function (network_study) {UseMethod ("networks")}
networks.character  <-  function (network_study) {levels (read.csv (network_study)$network)}
networks.data.frame <-  function (network_study) {levels (network_study$network)}

make_iPerf_plot <- function (dataset, title = NULL)
{
    mutate (dataset, timestamp = ymd_hm (timestamp)) %>%
    ggplot (aes (timestamp, received.mbps)) + 
    geom_point () +
    labs (x = "Date", y = "iPerf3 received in Mbit/s", title = title) +
    geom_smooth (se = TRUE)
}

make_iPerf_plot_file <- function (plot)
{
  ggsave ("iperf.png", plot)
}

df_from_jsons <- function (json_list)
{
    lapply (json_list,
            function (x)
            {
                rjson::fromJSON (file = x) %>%
                    as.data.frame (stringsAsFactors = FALSE)
            }) %>%

    bind_rows () %>%
    bind_cols (as.data.frame (json_list, stringsAsFactors = FALSE), .) %>%
    rename (file.name = json_list) %>%
    mutate (file.name = basename (file.name))
}

csv_from_jsons <- function (json_list = "", name)
{
  if (identical (json_list, ""))  json_list <- list_files_with_exts (dir = getwd(), exts = c ("json", "JSON"))
  df_from_jsons (json_list) %>%
        write.csv (file = name, row.names = FALSE)
}
