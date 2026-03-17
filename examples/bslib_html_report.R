library(htmltools)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(bslib)

# table
my_table <- function(data){
  datatable(data,
            extensions = c('Select', 'SearchPanes'),
            selection = 'none',
            rownames = FALSE,
            options = list(dom = 'Prtip',
                           searchPanes = list(
                             controls = FALSE,
                             viewCount = FALSE
                           ),
                           columnDefs = list(
                             list(searchPanes = list(show = FALSE),
                                  targets = 2:5)
                           ),
                           pageLength = 5)
  )
}

# plots
set_girafe_defaults(
  opts_hover = opts_hover(css = "opacity:0.8"),
  opts_hover_inv = opts_hover_inv(css = "opacity:0.4"))

my_plot <- function(data, sx){
  girafe(
    filter(data, sex == sx) |>
      ggplot(aes(body_mass, flipper_len, colour=species,
                 data_id = species)) +
      geom_point_interactive(),
    height_svg = 4,
    width_svg = 6
  )
}

# navs
plots_by_sex <- function(data){
  navset_pill(
    nav_panel("Male", 
              card_body(
                fillable=FALSE,
                my_plot(data, "male"))
    ),
    nav_panel("Female",
              card_body(
                fillable=FALSE,
                my_plot(data, "female"))
    )
  )
}

# pages
overview <- nav_panel(
  "Overview",
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    "This page shows all data on penguins",
    my_table(penguins)
  )
)

flipper_length <- nav_panel(
  "Flipper length",
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    "This page examines the flipper length of penguins",
    plots_by_sex(penguins)
  )
)

# report
page_navbar(
  title = "Penguin report",
  fillable = FALSE,
  navbar_options = navbar_options(
    bg = "#3F3685"
  ),
  flipper_length
)
