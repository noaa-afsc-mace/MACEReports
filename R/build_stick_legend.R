#' @title build_stick_legend
#'
#' @description Accepts a dataframe returned from the \code{build_sf_sticks} function and returns a nicely placed legend bar.
#' This legend is half the height of the tallest value in the dataframe.
#' This object can be plotted using \code{ggplot2::geom_sf()} or base \code{plot}.
#' @param stick_data A 'stick' dataframe returned from \code{build_sf_sticks}.
#' @param legend_pos One of 'right' or 'left'; legend bar will be placed at the corresponding corner.
#' @param legend_color Default is white for standard MACE basemaps; specify alternates in standard \code{ggplot}-compatible formats
#' @param legend_label Defaults to a label in standard MACE units (t/nmi^2); specify alternate if needed to reflect your units
#'
#' @examples
#'
#' library(ggplot2)
#' library(sf)
#'
#' dat = data.frame('x' = c(-152.2, -150.3, -159.4),
#' 'y' = c(55.2, 55.8, 55.6),
#' 'z' = c(75000,400000, 280000),
#' 'species' = c('a', 'a', 'b'))

#' sticks = build_sf_sticks(x = dat$x, y = dat$y, z = dat$z, rotation = 15, crs = 3338)
#' legend = build_stick_legend(stick_data = sticks, legend_pos = 'left', legend_color = 'black')
#' ggplot()+
#'   geom_sf(data = sticks)+
#'   legend
#'
#' @export
build_stick_legend = function(stick_data, legend_pos = 'right', legend_color = 'white', legend_label = NULL){

#checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
if (!"sf" %in% class(stick_data) | is.na(sf::st_crs(stick_data)$input)){
  stop('Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!')
}

#if an sf dataframe with a valid CRS is present, get the crs
crs = sf::st_crs(stick_data)$input

#get x/y positions as the min/max from the data extent
if (legend_pos == 'right'){

  legend_x = max(sf::st_coordinates(stick_data)[,1])
  legend_y = min(sf::st_coordinates(stick_data)[,2])

}

if (legend_pos == 'left'){

  legend_x = min(sf::st_coordinates(stick_data)[,1])
  legend_y = min(sf::st_coordinates(stick_data)[,2])

}

#check: make sure we have a valid legend position
if (legend_pos != 'left' & legend_pos != 'right'){
  stop('please specify "left" or "right" for legend_pos')
}

#get the maximium stick value
biggest_stick = stick_data[which.max(sf::st_length(stick_data)),]
dist = sf::st_length(biggest_stick)

#for legend, scale to 1/2 size of biggest stick
dist = as.numeric(dist) * 0.5

#set a small factor to 'bump' sticks up by (so there's room for text)
bump = dist * .1

#create a line from the x/y position to a height 1/2 the distance of the tallest bar
legend_line = sf::st_linestring(matrix(c(legend_x, (bump + legend_y), legend_x, (bump + legend_y + dist)),
                                                      nrow = 2, ncol = 2, byrow = TRUE))
legend_line = sf::st_geometry(legend_line)

#set the crs on the bars as the same as the baseplots
sf::st_crs(legend_line) = crs

#store the line as ggplot2 object
legend_line = ggplot2::geom_sf(data = legend_line, color = legend_color)

#add the value of the bar, with a prettier print format
legend_value = format(round(biggest_stick$z * 0.5, digits = -2), digits = 1, nsmall = 0, scientific = FALSE, big.mark = ',')

#add this to the bar
if (!is.null(legend_label)){

  #add the value of the bar, with a prettier print format
  legend_value = format(round(biggest_stick$z * 0.5, digits = 0), digits = 1, nsmall = 0, scientific = FALSE, big.mark = ',')

  legend_annotation = ggplot2::annotate(geom = 'text', x = (legend_x - bump), y = legend_y, label = paste(legend_value, legend_label))

  #add both the line and the legend to a ggplot2 object
  legend_line = list(legend_line, legend_annotation)


}

#add the annotation text if requested
if (is.null(legend_label)){

  #assume standard MACE units for stickplot!
  legend_value = format(round((biggest_stick$z/1e3) * 0.5, digits = -1), digits = 1, nsmall = 0, scientific = FALSE, big.mark = ',')

  label_name = deparse(bquote(.(legend_value) * " t/nmi"^2))

  legend_annotation = ggplot2::annotate(geom = 'text', x = (legend_x - bump), y = legend_y,
                                        label = label_name, parse = TRUE)

  #add both the line and the legend to a ggplot2 object
  legend_line = list(legend_line, legend_annotation)

}

#return the legend stick
return(legend_line)

}








