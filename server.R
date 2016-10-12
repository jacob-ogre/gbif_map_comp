# BSD_2_clause

shinyServer(function(input, output, session) {
  
  output$cur_species <- renderText({
    strsplit(input$sel_species, split=" (", fixed=TRUE)[[1]][1]
  })
  
  get_sci_name <- reactive({
    tmp <- strsplit(input$sel_species, split=" (", fixed=TRUE)[[1]][2]
    gsub(")", "", x=tmp, fixed=TRUE)
  })
  
  output$sci_name <- renderText({
    get_sci_name()
  })
  
  sci_fold <- reactive({
    gsub(" ", "_", x=get_sci_name(), fixed=TRUE)
  })
  
  species <- reactive({
    q <- parseQueryString(session$clientData$url_search)
    if(length(q) == 0) q[[1]] <- "All"
    subd <- full
    if(q[[1]] != "All") {
      idx <- filter(spp_lookup, spp == q[["species"]])$idx
      subd <- subd[idx, ]
    }
    return(subd)
  })
  
  
})
