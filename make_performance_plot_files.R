source ('~/R/iTinker Network Performance/speedtest.R')
source ('~/R/iTinker Network Performance/iperf.R')
setwd ('~/data/manchester/speedtest')
make_plot_files ('manchester_speedtest.csv', "manchester")
setwd ('~/data/lakeview/speedtest')
make_plot_files ('lakeview_open-mesh_speedtest.csv', "lakeview")
setwd ('~/data/lakeview/iPerf/')
make_network_study () %>% make_iPerf_plot (title = "lakeview") %>% make_iPerf_plot_file()
