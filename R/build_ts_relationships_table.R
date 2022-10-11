#' @title Create the standard MACE target strength relationships table
#' @description Produce a TS-length relationships table for use in standard cruise reports.
#' This stores all of the commonly used TS-length relationships, but will need to be updated as we
#' add more TS-length  relationships to analyses.
#' Note that this is formatted for use with MS Word output, and references are formatted for use with a \code{.bib}
#' bibliography file. Standard cruise reports include an appropriate \code{.bib} file.
#' @param ts_relationships_used An optional character vector of relationships used in the survey.
#' If used, this will attempt to limit the table to the values used in the survey. Current options are available at:
#' \code{macebase2.ts_relationships}; the most straightforward way to specify is to use the
#' \code{ts_relationships_data$TS_RELATIONSHIP} vector returned in report creation.
#'
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' #build a table for all commonly used TS relationships
#' ts_table_list = build_ts_relationships_table()
#'
#' #return table and caption
#' ts_table = ts_table_list[[1]]
#' ts_caption = ts_table_list[[2]]
#'
#' #limit based on the relationships used (see macebase2.ts_relationships)
#' ts_relationships_list = c('standard_pollock', 'chrysaora_melanaster', 'euphausiids_15_65mm_38khz')
#'
#' ts_table_list = build_ts_relationships_table(ts_relationships_used = ts_relationships_list)
#'
#' #return table and caption
#' ts_table = ts_table_list[[1]]
#' ts_caption = ts_table_list[[2]]
#' }
#'
#' @export
build_ts_relationships_table = function(ts_relationships_used = NULL){

  #########
  #step 1: create a dataframe with all of the possible equations/relationships (WE may need to add to this over time)
  #format is: 'Group'/'TS (dB re 1 m$^2$)'/ 'Length type'/ 'TS derived for which species'/  'Reference'/ 'ts_relationship'

  #define any footnotes first, to append to dataframe below
  krill_footnote= paste0('A = -930.429983; B = 3.21027896; C = 1.74003785; D = 1.36133896 x $10^{-8}$; ',
                         'E = -2.26958555 x $10^{-6}$; F=1.50291244 x $10^{-4}$; G = -4.86306872 x $10^{-3}$; H = 0.0738748423. ',
                         'I = -0.408004891; J = -73.9078690; and $L_0$ = 0.03835',
                         '\\\nIf L < 15 mm, TS = -105 dB; and if L > 65 mm, TS = -73 dB.',
                         '\\\nk = 2','\u03c0', 'fc, where f = 38,000 (frequency in Hz) and c = 1470 (sound speed in m/s). ')


  #build the equations dataframe
  ts_df = data.frame(c('Walleye pollock',
                       'Pacific capelin',
                       'Pacific herring',
                       'Fish with swim bladders',
                       'Fish without swim bladders',
                       'Jellyfish',
                       'Squid',
                       'Eulachon',
                       paste('Pelagic crustaceans^[', krill_footnote,']')),
                     c("TS = 20 log$_{10}$ *L*-66",
                       "TS = 20 log$_{10}$ *L*-70.3",
                       'TS = 20 log$_10$ *L*-2.3 log$_10$(1 + *depth*/10) - 65.4',
                       'TS = 20 log$_10$ *L* - 67.4',
                       'TS = 20 log$_10$ *L* - 83.2',
                       'TS = 10 log$_10$($\\pi$$r^2$) -86.8',
                       'TS = 20 log$_10$ *L* - 75.4',
                       'TS = 20 log$_10$ *L* - 84.5',
                       'TS = *A* * (log$_10 (BkL)/(BkL))^c + D ((kL)^6) + E ((kL)^5) + F ((kL)^4) + G ((kL)^3) + H ((kL)^2) + I (kL) + J + 20$ log$_10( L/L_0)$' ),
                     c('L = fork length',
                       'L = total length',
                       'L = fork length',
                       'L = total length',
                       'L = total length',
                       'r = bell radius',
                       'L = mantle length',
                       'L = total length',
                       'L = total length'),
                     c('*Gadus chalcogrammus*',
                       '*Mallotus catervarius*',
                       '*Clupea harengus*',
                       'Physoclist fishes',
                       '*Pleurogrammus monopterygius*',
                       '*Chrysaora melanaster*',
                       '*Todarodes pacificus*',
                       '*Thaleichthys pacificus*',
                       '*Euphausia superba*'),
                     c('@Foote_Traynor_1988,
                       @Traynor_1996',
                       '@Guttormsen_Wilson_2009',
                       '@Ona_2003',
                       '@Foote_etal_1987',
                       '@Gauthier_Horne_2004',
                       '@DeRobertis_Taylor_2014',
                       '@Kang_et_al_2005',
                       '@Gauthier_Horne_2004',
                       '@Demer_Conti_2005'),
                     c('pollock',
                       'capelin',
                       'herring',
                       'swimbladder_fish',
                       'no_swimbladder',
                       'chrysaora_melanaster',
                       'squid',
                       'eulachon',
                       'euphausiid'))

  #and name columns as in report tables
  colnames(ts_df) = c('Group', 'TS (dB re 1 m$^2$)', 'Length type', 'TS derived for which species',  'Reference', 'ts_relationship')

  ######
  #step 2: limit above table to the currently used ts relationships

  if (!is.null(ts_relationships_used)){

    #keep only the relationships that are in the current survey- this should find matches semi-flexibly
    #and not miss small changes in TS naming conventions
    check_match = function(ts_relationship){

      #check data types: must be a character vector!
      if (class(ts_relationships_used) != "character"){
        stop('ts_relationships_used is not a character vector. Specify a character vector!')
      }

      #find the matching relationship
      ts_match = ts_relationships_used[which(stringr::str_detect(ts_relationships_used, ts_relationship))]

      #if there's a match, return it, if not, return NA
      return(ifelse(length(ts_match) == 1, ts_match, NA))
    }

    #check all the available relationships for a match
    ts_df$ts_applied =  purrr::map_chr(ts_df$ts_relationship, check_match)

    ts_df = ts_df%>%
      dplyr::filter(!is.na(.data$ts_applied))%>%
      #and get rid of the ts_relationship column now for presentation
      dplyr::select(-c(.data$ts_relationship, .data$ts_applied))

  }


  if (is.null(ts_relationships_used)){

    #get rid of the ts_relationship column now for presentation
    ts_df = ts_df%>%
       dplyr::select(-c(.data$ts_relationship))

    }


  #########
  #step 3: make the table

  #define a border above and below caption and at bottom of table
  table_border = officer::fp_border(color="black", width = 0.75)

  #create the basic table + convert markdown to word text
  ts_table = flextable::flextable(ts_df) %>%
    ftExtra::colformat_md( part = 'all')%>%
    #align  text: center justify everything
    flextable::align(align = 'center', part = 'header')%>%
    flextable::align(align = 'center', part = 'body')%>%
    flextable::align(align = 'left', part = 'footer')%>%
    #add horizontal border on top and bottom of table
    flextable::hline_top(part="all", border = table_border)%>%
    flextable::hline_bottom(part="body", border = table_border)%>%
    #fit to maximize table width to 7.5 in
    flextable::width(j = 'Group', width = 1.5)%>%
    flextable::width(j = 'TS (dB re 1 m$^2$)', width = 2)%>%
    flextable::width(j = c('Length type', 'TS derived for which species', 'Reference'), width = 1.33)%>%
    #set the font and font size
    flextable::font(fontname = 'times', part = 'all')%>%
    flextable::fontsize(size = 10, part = 'header')%>%
    flextable::fontsize(size = 10, part = 'body')%>%
    flextable::fontsize(size = 10, part = 'footer')

    #add some caption text
    cap_text = paste0('Target strength (TS) to size relationships from the literature used to allocate 38 kHz acoustic backscatter ',
                      'to most species in this report. The symbols in the equations are as follows: r is the bell radius in cm and L ',
                      'is length in cm for all groups except pelagic crustaceans, in which case L is in mm. ')

  #return table and caption
  return(list(ts_table, cap_text))

}
