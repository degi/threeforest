#' The widget for displaying trees in 3D
#'
#' This widget using THREEJS library for 3D visualization
#'
#' @import htmlwidgets
#'
#' @export
threeforest <- function(forest_obj, setting = list(), width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    forest = forest_obj,
    setting = setting
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'threeforest',
    x,
    width = width,
    height = height,
    package = 'threeforest',
    elementId = elementId
  )
}

#' Shiny bindings for threeforest
#'
#' Output and render functions for using threeforest within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a threeforest
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name threeforest-shiny
#'
#' @export
threeforestOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'threeforest', width, height, package = 'threeforest')
}

#' @rdname threeforest-shiny
#' @export
renderThreeforest <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, threeforestOutput, env, quoted = TRUE)
}
