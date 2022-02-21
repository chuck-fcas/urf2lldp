

# Boost colors
boost_colors_list <- c(
  `teal`      = "#0097a7",
  `navy`      = "#16325f",
  `gray`      = "#c7c8ca",
  `green`     = "#e6e567")

boost_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (boost_colors_list)
  
  boost_colors_list[cols]
}

get_boost_color <- function(x){
  boost_colors_list[x][[1]]
}

boost_palettes <- list(
  `main`  = boost_colors("teal", "navy", "gray", "green")
  
  # `cool`  = drsimonj_cols("blue", "green"),
  # 
  # `hot`   = drsimonj_cols("yellow", "orange", "red"),
  # 
  # `mixed` = drsimonj_cols("blue", "green", "yellow", "orange", "red"),
  # 
  # `grey`  = drsimonj_cols("light grey", "dark grey")
)

boost_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- boost_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_boost <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- boost_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_boost <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- boost_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
