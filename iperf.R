### make_network_study (dir, study_name = "") create a network_study file in dir
### location_report (network_study, locations = "all") network report for specified locations
### network_report  (network_study, networks  = "all") network report for specified networks
### best_alternatives (network_study, locations = "all") best wired/wifi networks for specified locations
### locations (network_study) list locations
### networks  (network_study) list networks

require ("tools")
require ("rjson")
require ("dplyr")
require ("lubridate")

make_network_study <- function (dir = getwd(), study_name = "")
{
    parse.location <- function (string) {substr (string, 1, regexpr ("_", string) - 1)}
    parse.network  <- function (string) {substr (string, regexpr ("_", string) + 1, nchar (string))}

    df_from_jsons <- function (json_list)
    {
        lapply (json_list,
                function (x)
                {
                    fromJSON (file = x) %>%
                       as.data.frame (stringsAsFactors = FALSE) %>%
                       mutate (file.name = basename (file_path_sans_ext (x)))
                }) %>%
            bind_rows ()
    }

    if (study_name == "")
        study_name <- sprintf ("%s -- %s network study.csv",
                               format (Sys.time(), "%Y-%m-%d"), basename (dir))

    study <- df_from_jsons (list_files_with_exts (dir = dir, exts = c ("json", "JSON"))) %>%
             mutate (location = as.factor (parse.location (file.name)),
                     network  = as.factor (parse.network  (file.name)),
                     wifi = grepl ("airport|mesh|GHz|ghz", network),
                     sent.mbps = round(end.sum_sent.bits_per_second / 1e6, digits=2),
                     received.mbps = round(end.sum_received.bits_per_second / 1e6, digits=2),
                     timestamp = strftime (with_tz (dmy_hms (start.timestamp.time), tz = "America/Chicago"),
                                           format = "%Y-%m-%d %R",
                                           usetz = TRUE)) %>%
             arrange (desc(received.mbps)) %>%
             select (location, network, wifi, sent.mbps, received.mbps, timestamp)

    write.csv (file=file.path (dir, study_name), study, row.names = FALSE)
    study
}

location_report <- function (network_study, locations = "all")
{
    network.data <- read.csv (network_study)
    valid.locations <- levels (network.data$location)  

    if (identical (locations, "all")) locations <- valid.locations
    else for (location in locations) if (!location %in% valid.locations) stop ("invalid location")

    filter (network.data, location %in% locations) %>% 
    arrange (location, desc(received.mbps)) %>%
    select (location, network, everything())
}

network_report <- function (network_study, networks = "all")
{
    network.data <- read.csv (network_study)
    valid.networks <- levels (network.data$network)  
  
    if (identical (networks, "all")) networks <- valid.networks
    else for (network in networks) if (!network %in% valid.networks) stop ("invalid network")
  
    filter (network.data, network %in% networks) %>% 
    arrange (network, desc(received.mbps)) %>%
    select (network, location, everything())
}

best_alternatives <- function (network_study, locations = "all")
{
    network.data <- read.csv (network_study)
    valid.locations <- levels (network.data$location)

    if (identical (locations, "all")) locations <- valid.locations
    else for (location in locations) if (!location %in% valid.locations) stop ("invalid location")
  
    filter (network.data, location %in% locations) %>%
    group_by (location, wifi) %>% 
    slice (which.max (received.mbps)) %>%
    select (-wifi, everything()) %>%
    as.data.frame ()
}

locations <- function (network_study) {levels (read.csv (network_study)$location)}
networks <-  function (network_study) {levels (read.csv (network_study)$network)}
