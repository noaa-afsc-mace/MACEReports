#library(MACEReports)
library(magrittr)
#
# load(file = 'C:/Users/mike.levine/Downloads/Maps4Mike/202207_EBS_summer_data.RData')
#
# interval_data = intervals_and_events [[1]]
# #
# # basemap = MACEReports::get_basemap_layers(plot_limits_data = interval_data, bathy = FALSE, contours = c(100,200, 1000))
# #
# # basemap
# #
# plot_limits_data = interval_data
# bathy = FALSE
# contours = c(50, 100,200, 1000)
# management_regions = NULL
# SSL_critical_habitat = NULL
# alaska_3nmi_buffer = NULL
# land_fill_color = '#616161'
# land_outline_color = 'black'
#
# #get some data
# shelikof_xyz = MACEReports::shelikof_biomass%>%
#   dplyr::filter(year == 2021)%>%
#   dplyr::group_by(year, INTERVAL, START_LATITUDE, START_LONGITUDE, LENGTH)%>%
#   dplyr::summarize(BIOMASS = sum(BIOMASS),
#             BIOMASS_NM2 = sum(BIOMASS_NM2))
#
# sticks = MACEReports::build_sf_sticks(x = shelikof_xyz$START_LONGITUDE, y = shelikof_xyz$START_LATITUDE, z = shelikof_xyz$BIOMASS_NM2)
#
# #for testing
# basemap = MACEReports::get_basemap_layers(sticks)
# plot_limits_data = sticks
# basemap

#MACEReports::add_area_labels(lat_dd =  59.501, lon_dd = -178.008, area_name = 'Pervenets Canyon')
#MACEReports::add_area_labels(lat_dd =  58.049, lon_dd = -174.855, area_name = 'Zhemchug Canyon')
