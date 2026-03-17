library(htmltools)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(bslib)

# data
year <- 2008
location <- "Biscoe"

data <- penguins |>
  filter(year == year,
         island == location) |>
  select(species, sex, bill_len:body_mass)

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
    my_table(data)
  )
)

flipper_length <- nav_panel(
  "Flipper length",
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    "This page examines the flipper length of penguins",
    plots_by_sex(data)
  )
)

# report

output <- page_navbar(
  title = "Penguin report",
  fillable = FALSE,
  navbar_options = navbar_options(
    bg = "#3F3685"
  ),
  overview,
  flipper_length
)

### the following code is experimental:

save_self_contained_html <- function(taglist, output_filepath) {
  # Normalize output path
  output_filepath <- xfun::normalize_path(output_filepath)
  
  # Create a temporary directory for intermediate files
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Temporary HTML file inside temp_dir
  temp_html <- file.path(temp_dir, "temp.html")
  
  # Save the taglist as HTML (with /libs folder if needed)
  htmltools::save_html(taglist, file = temp_html, libdir = file.path(temp_dir, "libs"))
  
  # Remove <!DOCTYPE> lines for pandoc
  input_lines <- readLines(temp_html, warn = FALSE)
  writeLines(input_lines[!grepl("<!DOCTYPE", input_lines, fixed = TRUE)], temp_html)
  
  # Create a minimal template for pandoc
  template <- tempfile(fileext = ".html")
  on.exit(unlink(template), add = TRUE)
  xfun::write_utf8("$body$", template)
  
  # Determine pandoc format
  from <- if (rmarkdown::pandoc_available("1.17")) "markdown_strict" else "markdown"
  
  # Convert to self-contained HTML
  rmarkdown::pandoc_convert(
    input = temp_html,
    from = from,
    output = output_filepath,
    options = c("--embed-resources", "--standalone", "--template", template)
  )
  
  invisible(output_filepath)
}

# save_self_contained_html(output, "examples/bslib_html_report.html")