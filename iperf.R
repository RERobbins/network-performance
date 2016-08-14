### make_network_study (dir) create a network_study.csv file in dir based on json files in dir
### location_report (network_study, locations = "all") generate a network report for specified locations
### network_report (network_study, locations = "all") generate a network report for specified networks
### best_alternatives (network_study, locations = "all") summarize best wired/wifi choices for specified locations
### locations (network_study) list all locations in network_study file
### network (network_study) list all networks in network_study file 

require ("tools")
require ("rjson")
require ("dplyr")
require ("lubridate")

best_alternatives <- function (network_study, locations = "all")
{
    network.data <- read.csv (network_study)
    valid.locations <- levels (network.data$location)

    if (identical (locations, "all")) locations <- valid.locations
    else for (location in locations) if (!location %in% valid.locations) stop ("invalid location")
  
    filter (network.data, location %in% locations) %>%
    group_by (location, wifi) %>% 
    slice (which.max(received.mbps)) %>%
    select (-wifi, everything()) %>%
    as.data.frame()
}

locations <- function (network_study) {levels (read.csv(network_study)$location)}

networks <- function (network_study) {levels (read.csv(network_study)$network)}

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

make_network_study <- function (dir = getwd(), study_name = "")
{
    study <- df_from_jsons (list_files_with_exts (dir = dir, exts = c ("json", "JSON"))) %>%
        mutate (location = as.factor (substr (file.name, 1, regexpr ("_", file.name) - 1)),
                network = as.factor (substr (file.name,  regexpr ("_", file.name) + 1, nchar (file.name))),
                wifi = grepl ("airport|mesh|GHz|ghz", network),
                sent.mbps = round(end.sum_sent.bits_per_second / 1e6, digits=2),
                received.mbps = round(end.sum_received.bits_per_second / 1e6, digits=2),
                timestamp = strftime (with_tz (dmy_hms (start.timestamp.time), tz = "America/Chicago"),
                                      format = "%Y-%m-%d %R",
                                      usetz = TRUE)) %>%
        arrange (desc(received.mbps)) %>%
        select (location, network, wifi, sent.mbps, received.mbps, timestamp)
    
    if (study_name == "") study_name <- sprintf ("%s -- %s network study.csv" , format (Sys.time(), "%Y-%m-%d"), basename (dir))
    write.csv (file=file.path (dir, study_name), study, row.names = FALSE)
    study
}

df_from_jsons <- function (json_file_list = (list_files_with_exts (dir = getwd(), exts = c ("json" , "JSON"))))
{
    lapply (json_file_list,
            function (x)
            {as.data.frame (fromJSON (file = x), stringsAsFactors = FALSE) %>%
             mutate (file.name = basename (file_path_sans_ext (x)))
            }) %>%
    bind_rows ()
}
