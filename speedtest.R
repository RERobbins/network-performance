require (stringr)
require (lubridate)
require (dplyr)
require (ggplot2)

make_speedtest_dataset <- function (filename)
{
    read.table(filename, stringsAsFactors = FALSE, sep = '\t', header = TRUE) %>%
  
      rename (from.ip = from_ip,
              server.distance = server_dist,
              server.ping.speed = server_ping,
              download.speed = download,
              upload.speed = upload,
              share.url = share_url) %>%
    
      mutate (start = ymd_hms (start),
              stop = ymd_hms (stop),
              from = as.factor (from),
              from.ip = as.factor (from.ip),
              server = as.factor (server),
              server.distance.unit = as.factor (word (server.distance, 2)),
              server.distance = as.numeric (word (server.distance, 1)),
              server.ping.speed.unit = as.factor (word (server.ping.speed, 2)),
              server.ping.speed = as.numeric (word (server.ping.speed, 1)),
              download.speed.unit = as.factor (word (download.speed, 2)),
              download.speed = as.numeric (word (download.speed, 1)),
              upload.speed.unit = as.factor (word (upload.speed, 2)),
              upload.speed = as.numeric (word (upload.speed, 1)))
}

make_download_plot <- function (dataset, title = NULL)
{ 
    ggplot (dataset, aes (start, download.speed, color = server)) + 
        geom_point() +
        labs (x = "Date", y = "Download Speed in Mbit/s", title = title)
}  
  
make_upload_plot <- function (dataset, title = NULL)
{
    ggplot (dataset, aes (start, upload.speed, color = server)) + 
        geom_point () +
        labs (x = "Date", y = "Upload Speed in Mbit/s", title = title)
}

make_ping_plot <- function (dataset, title = NULL)
{
    ggplot (dataset, aes (start, server.ping.speed, color = server)) + 
        geom_point () +
        labs (x = "Date", y = "Ping Response in ms", title = title)
}

make_plot_list <- function (dataset, title = NULL) {UseMethod ("make_plot_list")}

make_plot_list.character <- function (dataset, title = NULL)
{
    make_speedtest_dataset (dataset) %>% make_plot_list.data.frame(title)
}  

make_plot_list.data.frame <- function (dataset, title = NULL)
{
    list (ping = make_ping_plot (dataset, title = title),
          upload = make_upload_plot (dataset, title = title),
          download = make_download_plot (dataset, title = title))
}

make_plot_files <- function (x, title = NULL) {UseMethod ("make_plot_files")}  

make_plot_files.character <- function (dataset, title = NULL)
{
    make_plot_list.character (dataset, title = title) %>% make_plot_files.list()
}

make_plot_files.data.frame <- function (dataset, title = NULL)
{
    make_plot_list (dataset, title = title) %>% make_plot_files.list()
}

make_plot_files.list <- function (plot_list)
{
    ggsave ("ping.png", plot_list$ping)
    ggsave ("upload.png", plot_list$upload)
    ggsave ("download.png", plot_list$download)
}
