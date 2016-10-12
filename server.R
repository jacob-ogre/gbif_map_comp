# BSD_2_clause

shinyServer(function(input, output, session) {
  
  output$cur_species <- renderText({
    strsplit(input$sel_species, split = " (", fixed = TRUE)[[1]][1]
  })
  
  get_sci_name <- reactive({
    tmp <- strsplit(input$sel_species, split = " (", fixed = TRUE)[[1]][2]
    gsub(")", "", x = tmp, fixed = TRUE)
  })
  
  output$sci_name <- renderText({
    get_sci_name()
  })
  
  sci_fold <- reactive({
    gsub(" ", "_", x = get_sci_name(), fixed = TRUE)
  })
  
  species <- reactive({
    q <- parseQueryString(session$clientData$url_search)
    if(length(q) == 0) q[['species']] <- "Chelonia mydas"
    return(q[['species']])
  })
  
  
  #########################################################################
  # Now all of the code for the map!                                      
  get_gbif <- function(x) {
    if(x != "") {
      cur_search <- occ_search(scientificName=x, 
                               hasCoordinate=TRUE,
                               limit=250)
      if(!is.null(cur_search$data)) {
        cur_dat <- cur_search$data
        img_dat <- get_image_data(cur_search)
        new_dat <- cur_dat %>% full_join(img_dat, key="key")
        return(new_dat)
      } else {
        info("No records were found.")
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  
  get_image_data <- function(x) {
    n_ent <- length(x$media)
    res <- data_frame(key=rep(NA, n_ent),
                      img=rep(NA, n_ent),
                      lic=rep(NA, n_ent),
                      holder=rep(NA, n_ent))
    for(i in 1:n_ent) {
      if(length(x$media[[i]]) > 0) {
        cur_dat <- c(NA, NA, NA, NA)
        key <- ls(x$media[[i]])
        if(!is.null(key)) { cur_dat[1] <- key }
        img <- x$media[[i]][[1]][[1]]$identifier
        if(!is.null(img)) { cur_dat[2] <- img }
        lic <- x$media[[i]][[1]][[1]]$license
        if(!is.null(lic)) { cur_dat[3] <- lic }
        holder <- x$media[[i]][[1]][[1]]$rightsHolder
        if(!is.null(holder)) { cur_dat[4] <- holder }
        res[i, ] <- cur_dat
      } else {
        res[i, ] <- c(NA, NA, NA, NA)
      }
    }
    res$key <- as.numeric(res$key)
    return(res[complete.cases(res), ])
  }
  
  current_gbif <- reactive({
    withProgress(message = "Getting GBIF records",
                 detail = "Please wait...",
                 value = 0.5, {
                   res <- get_gbif(species())
                   incProgress(0.5)
                 })
    return(res)
  })
  
  cur_zoom <- reactive({
    if (!is.null(input$map_zoom)) {
      input$map_zoom
    } else {
      4
    }
  })

  #######################################################################
  # Now to make the map
  output$sp_map <- renderLeaflet({
    GBdat <- current_gbif()
    if(!is.null(GBdat)) {
      ref <- rep("", length(GBdat$references))
      if(!is.null(GBdat$references)) {
        ref <- ifelse(!is.na(GBdat$references),
                      paste0("<a href = '",
                             GBdat$references,
                             "' target = '_blank'>Specimen record</a>"),
                      paste0("<a href = http://www.gbif.org/occurrence/",
                             GBdat$key,
                             " target = '_blank'>GBIF record</a>"))
      } else {
        ref <- paste0("<a href = http://www.gbif.org/occurrence/",
                      GBdat$key,
                      " target = '_blank'>GBIF record</a>", ref)
      }
      coord_dat <- data_frame(long = GBdat$decimalLongitude,
                              lat = GBdat$decimalLatitude,
                              popup = ref)
    }
    
    lat_mid <- 38
    if(!is.null(coord_dat)) {
      max_lat <- max(coord_dat$lat[coord_dat$lat > 0], na.rm = TRUE)
      min_lat <- min(coord_dat$lat[coord_dat$lat > 0], na.rm = TRUE)
      lat_mid <- (max_lat + min_lat) / 2
    }
    cur_map <- leaflet(data = coord_dat) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -110,
              lat = lat_mid,
              zoom  =  4) %>%
      addMarkers(lng = ~long, lat = ~lat, popup = ~popup,
                 clusterOptions = markerClusterOptions()) %>%
      mapOptions(zoomToLimits  =  "first")

    return(cur_map)
  })
  outputOptions(output, "sp_map", suspendWhenHidden  =  FALSE)
  
  # proxy to add occurrences, if available
  observe({
    GBdat <- current_gbif()
    if(!is.null(GBdat) & dim(GBdat)[1] > 0) {
      ref <- rep("", length(GBdat$references))
      if(!is.null(GBdat$references)) {
        ref <- ifelse(!is.na(GBdat$references),
                      paste0("<a href = '",
                             GBdat$references,
                             "' target = '_blank'>Specimen record</a>"),
                      paste0("<a href = http://www.gbif.org/occurrence/",
                             GBdat$key,
                             " target = '_blank'>GBIF record</a>"))
      } else {
        ref <- paste0("<a href = http://www.gbif.org/occurrence/",
                      GBdat$key,
                      " target = '_blank'>GBIF record</a>", ref)
      }
      coord_dat <- data.frame(long = GBdat$decimalLongitude,
                              lat = GBdat$decimalLatitude,
                              popup = ref)
      
      leafletProxy("sp_map", data = coord_dat) %>%
        clearMarkerClusters() %>%
        addMarkers(lng = ~long, lat = ~lat, popup = ~popup,
                   clusterOptions = markerClusterOptions())
    } else if(dim(GBdat)[1] == 0) {
      
    }
  })
  
  # observe({
  #   input$map_rezoom
  #   leafletProxy("sp_map") %>%
  #     setView(lng = -110,
  #             lat = 32,
  #             zoom  =  4)
  # })
  
})
