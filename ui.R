# BSD_2_clause

shinyUI(fluidPage(
  useShinyjs(),
  tags$style(
    "div.outer {
      position: fixed;
      background-color: #bfbfbf;
      top: 0px;
      left: 0;
      right: 0;
      bottom: 0;
      overflow: hidden;
      padding: 0;
    }"
  ),
  div(class = "outer",
    leafletOutput("sp_map", height = "100%", width = "100%")
  )
))
