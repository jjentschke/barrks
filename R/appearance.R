


#' Get `barrks` default color palettes
#'
#' Get `barrks` default color palettes.
#'
#' @param type Select the desired color palette. There are different variants
#' for particular purposes. Allowed values are `'raster'`, `'diagram_lines'`,
#' `'diagram_fill'`, `'bso_flight'` and `'bso_stages'`.
#'
#' @returns A character vector of hex colors.
#'
#' @examples
#' colors <- barrks_colors()
#'
#' # use the colors of 'barrks' for your individual plot...
#' @seealso [barrks_labels()]
#' @export

barrks_colors <- function(type = 'raster') {

  switch(type,
         raster = default_colors,
         diagram_lines = default_colors_diagram_lines,
         diagram_fill = default_colors_diagram_fill,
         bso_flight = bso_default_colors_flight,
         bso_stages = bso_default_colors_stages)
}



#' Get `barrks` default legend labels
#'
#' Get `barrks` default legend labels.
#'
#' @param type Select the desired legend labels. There are different variants
#' for particular purposes. Allowed values are `'raster'`, `'diagram'`,
#' `'bso_flight'` and `'bso_stages'`.
#'
#' @returns A character vector of labels.
#'
#' @examples
#' labels <- barrks_labels()
#'
#' # use the labels of 'barrks' for your individual plot...
#' @seealso [barrks_colors()]
#'
#' @export

barrks_labels <- function(type = 'raster') {

  switch(type,
         raster = default_labels,
         diagram = default_labels_diagram,
         bso_flight = bso_default_labels_flight,
         bso_stages = bso_default_labels_stages)
}
