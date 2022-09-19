#' @title Create a MACE-themed basemap in the Bering Sea
#' @description Returns a base map. This map is returned as a ggplot2 object that more complex maps can be built on top of.
#' It provides land, bathymetry, and, optionally, a variety of common layers including the NMFS management areas, 3 NMI buffer regions,
#' and Steller Sea Lion exclusions.
#' This layer will be slightly larger than the extent of \code{plot_limits_data}; users should still use \code{ggplot2::coord_sf}
#' to fine-tune the plot extent.
#' @param plot_limits_data A \code{sf} spatial dataframe; this is required and used to define the base map extent and projection.
#' @param bathy By default, a bathymetric baselayer based on the GEBCO (https://www.gebco.net/) gridded bathymetric dataset
#' is included in the basemap; If \code{FALSE}, bathymetric baselayer will not be included
#' @param management_regions If \code{TRUE}, will add NMFS management regions to basemap
#' @param SSL_critical_habitat If \code{TRUE}, will add Steller Sea Lion critical habitat buffers to basemap
#' @param alaska_3nmi_buffer If \code{TRUE}, will add the ADFG 3 nmi management buffer to basemap
#' @param land_fill_color If you'd like a different fill color on landmasses, specify as required by \code{ggplot2}.
#' @param land_outline_color If you'd like a different outline color on landmasses, specify as required by \code{ggplot2}.
#' @param region If you are working in the GOA- selecting 'goa' will give you a higher-resolution bathymetry layer.
#' @return A list of class \code{ggplot} containing information required for plotting a basemap.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' library(MACEReports)
#'
#' dat = data.frame('x' = c(-152.2, -150.3, -159.4),
#' 'y' = c(55.2, 55.8, 55.6),
#' 'z' = c(7500,40000, 28000),
#' 'species' = c('a', 'a', 'b'))
#'
#' #create an sf dataframe
#' dat = sf::st_as_sf(dat, coords = c('x', 'y'), crs = 4326)

#' #convert CRS to a reasonable projection
#' dat = sf::st_transform(dat, crs = 'EPSG:3338')
#'
#' #return the basemap as a ggplot layer
#' mace_basemap = get_basemap_layers(plot_limits_data = dat)
#'
#' #note that this basemap will replace the usual call to ggplot() as the start of your plot:
#' mace_basemap+
#'  geom_sf(data = dat, aes(color = species))
#'
#' #You can add other commonly used MACE layers to the basemap
#' mace_basemap = get_basemap_layers(plot_limits_data = dat, management_regions = TRUE)
#' mace_basemap
#'
#' mace_basemap = get_basemap_layers(plot_limits_data = dat, alaska_3nmi_buffer = TRUE)
#' mace_basemap
#'
#' mace_basemap = get_basemap_layers(plot_limits_data = dat, SSL_critical_habitat = TRUE)
#' mace_basemap
#'}
#'
#' @export
get_basemap_layers = function(plot_limits_data,
                              bathy = TRUE,
                              management_regions = NULL,
                              SSL_critical_habitat = NULL,
                              alaska_3nmi_buffer = NULL,
                              land_fill_color = '#616161',
                              land_outline_color = 'black',
                              region = NULL){

  #checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
  if (!"sf" %in% class(plot_limits_data) | is.na(sf::st_crs(plot_limits_data)$input)){
    stop('Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!')
  }

  #if an sf dataframe with a valid CRS is present, get the crs
  crs = sf::st_crs(plot_limits_data)$input

  #check if we've already got a collection of shapefiles/rasters for the requested CRS
  #map_dir = paste0('inst/extdata/', stringr::str_remove(crs, ':'))
  map_dir = system.file("extdata", stringr::str_remove(crs, ':'), package = "MACEReports")

  #if the directory exists- we just need to open up the requested files
  if (dir.exists(map_dir)){

    #open up all the layers we need
    ak_land = sf::st_read(paste0(map_dir, '/alaska_land_', stringr::str_remove(crs, ':'), '.gpkg'))
    russia_land =  sf::st_read(paste0(map_dir, '/russia_land_', stringr::str_remove(crs, ':'), '.gpkg'))
    canada_land = sf::st_read(paste0(map_dir, '/canada_land_', stringr::str_remove(crs, ':'), '.gpkg'))

    if (!is.null(management_regions)){
      management_regions_layer = sf::st_read(paste0(map_dir, '/alaska_NMFS_management_regions_',
                                                      stringr::str_remove(crs, ':'), '.gpkg'))
    }

    if (!is.null(SSL_critical_habitat)){
      SSL_critical_habitat_layer = sf::st_read(paste0(map_dir, '/SSL_critical_habitat_',
                                                stringr::str_remove(crs, ':'), '.gpkg'))
    }

    if (!is.null(alaska_3nmi_buffer)){
      alaska_3nmi_buffer_layer = sf::st_read(paste0(map_dir, '/alaska_3nmi_buffer_',
                                                      stringr::str_remove(crs, ':'), '.gpkg'))
    }

    if (bathy == TRUE){

        if (is.null(region)){

          bathy_raster = terra::rast(paste0(map_dir, '/alaska_bathy_raster_', stringr::str_remove(crs, ':'), '.tif'))
        }

      if (!is.null(region)){

        if(region == 'goa'){

          load(file = paste0(map_dir, '/GOA_bathy_raster_EPSG3338.rdata'))

          #note that terra raster has a bug where it can throw an error on first use... and then work fine.
          #see: https://stackoverflow.com/questions/65556253/r-raster-selffinalize-error-causing-failure
          #so, try first in a try statement, then try again. Also note chunk option is 'error_TRUE' so we can bypass bug
          bathy_raster = tryCatch(terra::rast(bathy_raster, type = 'xyz'),
                                  error = function(e)
                                    terra::rast(bathy_raster, type = 'xyz'))



          terra::crs(bathy_raster) = "EPSG:3338"

        }

        if(region != 'goa'){
          bathy_raster = terra::rast(paste0(map_dir, '/alaska_bathy_raster_', stringr::str_remove(crs, ':'), '.tif'))
        }

      }

    }

  }

  #if we don't have anything for the requested crs, build it
  if (!dir.exists(map_dir)){

    message(paste0('Creating new basemap features for ', crs, '.'))

    #open the shapefiles (from 3338- since these are included to start with)
    base_dir = system.file("extdata/EPSG3338/", package = "MACEReports")
    ak_land = sf::st_read(paste0(base_dir, '/alaska_land_EPSG3338.gpkg'))
    russia_land = sf::st_read(paste0(base_dir, '/russia_land_EPSG3338.gpkg'))
    canada_land = sf::st_read(paste0(base_dir, '/canada_land_EPSG3338.gpkg'))
    management_regions_layer = sf::st_read(paste0(base_dir, '/alaska_NMFS_management_regions_EPSG3338.gpkg'))
    SSL_critical_habitat_layer = sf::st_read(paste0(base_dir,'/SSL_critical_habitat_EPSG3338.gpkg'))
    alaska_3nmi_buffer_layer = sf::st_read(paste0(base_dir, '/alaska_3nmi_buffer_EPSG3338.gpkg'))

    #convert to the requested projection
    ak_land = sf::st_transform(ak_land, crs = crs)
    russia_land = sf::st_transform(russia_land, crs = crs)
    canada_land = sf::st_transform(canada_land, crs = crs)
    management_regions_layer = sf::st_transform(management_regions_layer, crs = crs)
    SSL_critical_habitat_layer = sf::st_transform(SSL_critical_habitat_layer, crs = crs)
    alaska_3nmi_buffer_layer = sf::st_transform(alaska_3nmi_buffer_layer, crs = crs)

   if (bathy == TRUE){

      #again, start with the 3338 layer
      bathy_raster = terra::rast(paste0(base_dir, '/alaska_bathy_raster_EPSG3338.tif'))

      #convert it
      bathy_raster = terra::project(bathy_raster, crs, method = "bilinear")

      #up the resolution
      bathy_raster = terra::disagg(bathy_raster, fact = c(5, 5), method = 'near')

    }

  }

  #clip the exent of background layers for plotting

  #limit the extent of the plot to be slightly greater than the plot area
  region_zoom_box = sf::st_as_sfc(sf::st_bbox(plot_limits_data))

  #create a small buffer, around 10% of the total x-axis extent
  p_min = sf::st_point(c(min(sf::st_coordinates(region_zoom_box)[,1]), min(sf::st_coordinates(region_zoom_box)[,2])))
  p_max = sf::st_point(c(max(sf::st_coordinates(region_zoom_box)[,1]), max(sf::st_coordinates(region_zoom_box)[,2])))

  #compute the maximum distance across plot; add a buffer to the plot as 5% of this distance
  dist_buffer = sf::st_distance(p_min, p_max)[[1]] * 0.1

  #if you are working in a geographic coordinate system, add this buffer directly to your coordinates
  #(this is a workaround, as geographic buffers are problematic- not needed if you are in a projected system)
  if (sf::st_is_longlat(region_zoom_box)){
     sf::sf_use_s2(FALSE)
     region_zoom_box =  sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = 'MITRE', mitreLimit = 2)
     sf::sf_use_s2(TRUE)
  }

  # #in projected coordinate systems, simply apply the buffer
  if (!sf::st_is_longlat(region_zoom_box)){
     region_zoom_box =  sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = 'MITRE', mitreLimit = 2)
  }

  #crop the base layers
  ak_land =  sf::st_intersection(sf::st_geometry(ak_land), sf::st_geometry(region_zoom_box))
  russia_land =  sf::st_intersection(sf::st_geometry(russia_land), sf::st_geometry(region_zoom_box))
  canada_land =  sf::st_intersection(sf::st_geometry(canada_land), sf::st_geometry(region_zoom_box))

  if (!is.null(management_regions)){
     management_regions_layer = sf::st_intersection(sf::st_make_valid(sf::st_geometry(management_regions_layer)),
                                                    sf::st_geometry(region_zoom_box))
  }

  if (!is.null(SSL_critical_habitat)){
    SSL_critical_habitat_layer = sf::st_intersection(sf::st_make_valid(sf::st_geometry(SSL_critical_habitat_layer)),
                                                     sf::st_geometry(region_zoom_box))
  }

  if (!is.null(alaska_3nmi_buffer)){
    alaska_3nmi_buffer_layer = sf::st_intersection(sf::st_geometry(alaska_3nmi_buffer_layer), sf::st_geometry(region_zoom_box))
  }


  #if we are using bathy raster, limit it to be sized for the plot too (much faster);
  #and return it as a dataframe to plot with ggplot
  if (bathy == TRUE){

     #set a box that can be used to clip raster to plot extent (much faster plotting!)
     clip = terra::ext(sf::st_bbox(region_zoom_box)[[1]], sf::st_bbox(region_zoom_box)[[3]],
                       sf::st_bbox(region_zoom_box)[[2]], sf::st_bbox(region_zoom_box)[[4]])

    #crop to these dimensions
    bathy_raster_df = terra::crop(bathy_raster, clip, snap = 'near')

    #and use these dimensions to create a dataframe that we can plot with ggplot
    if (is.null(region)){

        bathy_raster_df = terra::as.data.frame(bathy_raster_df, xy = TRUE)%>%
         dplyr::rename('z' = 'lyr.1')

        #define a color scale- you can tweak this to give the right amount of weight to the deep vs shallow areas (right now),
        #theres more weight given to 200 m and up
        bathy_colors = ggplot2::scale_fill_gradientn(values = scales::rescale(c(min(bathy_raster_df$z, na.rm = TRUE),
                                                                                -300, -50, .99,
                                                                                max(bathy_raster_df$z, na.rm = TRUE))),
                                                     colors = c("#737373", "#969696", "#d9d9d9",  "#d9d9d9"),
                                                     #if user wants a legend, present units as positive (depth)
                                                     #instead of negative (altitude)
                                                     labels = function(x){ abs(x) })

    }

    if (!is.null(region)){

      bathy_raster_df = terra::as.data.frame(bathy_raster_df, xy = TRUE)

      #define a more appropriate colorscale for the goa
      bathy_colors = ggplot2::scale_fill_gradientn(values = scales::rescale(c(min(bathy_raster_df$z, na.rm = TRUE),
                                                                              -250,  0,
                                                                     max(bathy_raster_df$z, na.rm = TRUE))),
                                          colors = c("#737373", "#969696", "#d9d9d9",  "#d9d9d9"),
                                          #if user wants a legend, present units as positive (depth)
                                          #instead of negative (altitude)
                                          labels = function(x){ abs(x) })

    }




  }

  #define the basemap ggplot object
  basemap_layers = ggplot2::ggplot()+
    {if (bathy == TRUE) ggplot2::geom_raster(data = bathy_raster_df, ggplot2::aes(x=x,y=y,fill=z))} +
    {if (bathy == TRUE) bathy_colors}+
    {if (!is.null(SSL_critical_habitat)) ggplot2::geom_sf(data = SSL_critical_habitat_layer, color = 'white', fill = 'transparent')}+
    {if (!is.null(alaska_3nmi_buffer)) ggplot2::geom_sf(data = alaska_3nmi_buffer_layer, color = 'white', fill = 'transparent')}+
    {if (!is.null(management_regions)) ggplot2::geom_sf(data = management_regions_layer, color = 'white', fill = 'transparent')}+
    ggplot2::geom_sf(data = ak_land, fill = land_fill_color, color = land_outline_color)+
    ggplot2::geom_sf(data = russia_land, fill = land_fill_color, color = land_outline_color)+
    ggplot2::geom_sf(data = canada_land, fill = land_fill_color, color = land_outline_color)+
    ggplot2::guides(fill = 'none')+
    #set the basemap to standard MACE theme
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text= ggplot2::element_text(size=12),
             axis.title = ggplot2::element_text(size = 12),
             legend.box.background =  ggplot2::element_rect(fill = scales::alpha("white", 0.55), color = 'transparent'),
             legend.key = ggplot2::element_rect(fill = "transparent"),
             legend.text= ggplot2::element_text(color = 'black', size = 12),
             panel.grid.major = ggplot2::element_blank(),
             panel.grid.minor = ggplot2::element_blank(),
             legend.background = ggplot2::element_blank(),
             axis.ticks.x = ggplot2::element_blank(),
             axis.text.x = ggplot2::element_blank(),
             axis.title.x = ggplot2::element_blank(),
             axis.ticks.y = ggplot2::element_blank(),
             axis.text.y = ggplot2::element_blank(),
             axis.title.y = ggplot2::element_blank())

  return(basemap_layers)

}



