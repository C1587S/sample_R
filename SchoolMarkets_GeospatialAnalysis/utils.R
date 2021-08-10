# dependencies
installations <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

packages <- c('corrr', 'data.table', 'ddpcr', 'dplyr', "stringr",
              'foreign', 'ggraph','glmnet', 'haven', 'htmlwidgets',
              'htmltools', 'igraph', 'leaflet','linkcomm', 'maptools', 
              'network', 'RANN', 'raster', 'readr', 'RColorBrewer', 
              'scales', 'sjlabelled', 'sp', 'sf', 'tidygraph', 'GeoRange', 
              'geosphere', 'igraphdata', 'shinydashboard', 'leaflet.extras', 
              'kableExtra', 'formattable', 'shinydashboard', 'usethis')

lapply(packages, installations)

####### Functions geospatial analysis
calc_buffers_v2 <- function(db, radius, save=TRUE){
  "Calculates buffers for schools
  Inpunts: 
      - df (dataframe): Dataframe with longitud and latitud cols. 
      - radius (int): Radius in mts for computing buffers
  Outputs:
      - df_buffers (dataframe): Coordinates, schools, and buffer number
      - buffers_proj (sp). Spatial Object with projected buffers.
  Example:
      - buff_test <- calc_buffers(schools_df, 4000)
  "
  db_sp <- db # create a df to be converted to a spatial object
  ## projections
  unproj <- CRS("+proj=longlat +datum=WGS84")
  proj <- CRS("+init=epsg:6370")  
  coordinates(db_sp) <- c(x="longitud", y="latitud") 
  proj4string(db_sp) <- unproj  
  db_sp <- spTransform(db_sp, proj) 
  ## calculate buffers
  buffers <- gBuffer(db_sp, width=radius) 
  db$buffer <- as.character(over(db_sp, disaggregate(buffers)))
  db <- db %>% arrange(as.numeric(buffer))
  # for graphing buffers
  buffers_proj <- spTransform(buffers, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  # return both df and spdf for graph maps
  buffers_outs <- list("df_buffers"= db, "buffers_proj"=buffers_proj)
  if (save==TRUE){
    dir.create(file.path("../data/buffers")) # create directory if not exists
    kms <- radius/1000
    filename<- paste0("buffers_",kms,"kms.rds")
    saveRDS(buffers_proj, paste0("../data/buffers/projections_", filename))
    saveRDS(buffers_outs, paste0("../data/buffers/df_", filename))
  }
  
  return(buffers_outs)
}

calc_convexhulls <- function(df_buffers){
  "Calculates convexHulls for schools by using buffers
  Inpunts: 
      - df_buffers (dataframe): Dataframe containing buffer number and coordinates 
      - radius (int): Radius in mts for computing buffers
  Outputs:
      - df_for_ch (dataframe): Filtered df used for computing convex hulls.
      - ch_polygons (f). Spatial df with polygons containing convex hulls.
  Example:
      - buff_test <- calc_buffers(schools_df, 4000)
  "
  # Filtered dataframe for computing CH
  df_buffers_filt <- df_buffers %>% 
    group_by(buffer) %>%
    distinct(latitud, longitud) %>% # drop-out CCT with the same coordinates
    summarise(n_schools = n(), longitud = longitud, latitud = latitud,
              `.groups`="drop") %>%
    filter(n_schools>=3) %>% 
    ungroup() %>% 
    select(-n_schools) %>% 
    arrange(as.numeric(buffer))
  
  # CH computation
  hulls_polygons <- df_buffers_filt %>% 
    group_by(buffer) %>% 
    summarise(geometry = st_sfc(st_cast(st_multipoint(cbind(longitud, latitud)), 'POLYGON')),
              ch =st_convex_hull(st_combine(geometry)),
              `.groups`="drop") %>% 
    arrange(as.numeric(buffer)) %>% 
    select(buffer, ch) %>% 
    st_sf()
  
  # return both df and spdf for graph maps
  ch_outs <- list("df_for_ch"= df_buffers_filt, "ch_polygons"=hulls_polygons)
  
  return(ch_outs)
}

compute_spatial_network <- function(select_nodos, select_relations) {
  locs <- data.frame("CCT" = select_nodos$name, 
                     "subgrupo" = select_nodos$sub_grupo,
                     "lat" = select_nodos$lat, "lon"=select_nodos$lon)
  links <- data.frame("From" = select_relations$cct_o,
                      "To" = select_relations$cct_d, 
                      "freq"=select_relations$flujo,
                      "tot_cambian"=select_relations$total_cambian)
  
  n <- graph.data.frame(links, directed=TRUE, vertices=locs)
  network <- get.data.frame(n, "both")
  
  vert <- network$vertices
  coordinates(vert) <- ~ lon + lat
  edges <- network$edges
  edges <- lapply(1:nrow(edges), function(i){
    as(rbind(vert[vert$name==edges[i, "from"], ],
             vert[vert$name==edges[i, "to"], ]),
       "SpatialLines")
  }
  )
  
  for (i in seq_along(edges)){
    edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
  }
  edges <- do.call(rbind, edges)
  
  flujo <- select_relations$flujo
  scaled_flujo <- scale(select_relations$flujo, 2, 10)
  total <-select_relations$total_cambian
  scaled_total <- scale(select_relations$total_cambian, 2, 10)
  weight <- select_relations$total_cambian
  scaled_weight <- scale(select_relations$weight, 2, 10)
  
  lines_sf <- st_as_sf(edges, CRS("+proj=utm +zone=51 ellps=WGS84"))
  lines_df <- as.data.frame(lines_sf)  %>% cbind(flujo, scaled_flujo, 
                                                 total, scaled_total, 
                                                 weight, scaled_weight) %>% 
                                           rename(lines=geometry)
  
  out_graph = list("network"=network, "verts"=vert, "edges"=lines_df)
  
  return(out_graph)
}

map_layers_v2 <- function(vert, df_buffers, proj_buffers, sel_buff, ch_df, unions, 
                          edges, edges_type="flujo", edges_color) {
  # this function does not include buffers layers
  #school icons
  icons <- awesomeIcons(
    icon = "graduation-cap", library = "fa",
    markerColor = "green")
  # Create a categorical palette for communities along the commuting zone
  n_sub <- vert$subgrupo %>% unique() %>% length()
  factpal <- colorFactor(topo.colors(n_sub), vert$subgrupo)
  
  if (edges_type=="flujo"){
    edges_label <- "Flujo: "
    edges_label2 <- edges$flujo
    edges_weight <- edges$scaled_flujo
  } else if (edges_type=="total"){
    edges_label <- "Cambio total: "
    edges_label2 <- edges$total
    edges_weight <- edges$scaled_total
  } else if (edges_type=="peso"){
    edges_label <- "Weight: "
    edges_label2 <- edges$weight
    edges_weight <- edges$scaled_weight
  }
  
  if(missing(sel_buff)) {
    data <- df_buffers
  } else {
    data <- df_buffers %>% filter(buffer==sel_buff)
  }
  ### Map
  leaflet() %>%
    # addTiles() %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa") %>% 
    ## Schools
    addAwesomeMarkers(data=df_buffers, ~longitud, ~latitud, group = "Escuelas",
                      clusterOptions = markerClusterOptions(),
                      label = ~htmlEscape(paste("CCT:", cct, "| Buffer", buffer)),
                      labelOptions = labelOptions(direction = "top", 
                                                  textsize = "10px", textOnly = TRUE),
                      icon=icons) %>%  
    # convexhulls
    addPolygons(data=ch_df$ch_polygons, group = "Envolventes convexas",
                stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.2, fillColor="red") %>% 
    # Unions
    addPolygons(data=unions, weight = 3, fillColor = "yellow", 
                group = "Uniones de envolventes convexas") %>%
    # # Buffers
    addPolygons(data=proj_buffers, weight = 3, fillColor = "green", group = "Buffers") %>%
    # 
    # Networks
    addPolylines(data=edges$lines, weight=edges_weight, label=paste(edges_label, edges_label2),
                 color=edges_color, group = "Aristas") %>%
    addCircleMarkers(data=vert, radius = 6, weight = 6, color=~factpal(subgrupo), 
                     stroke = TRUE, fillOpacity = 0.5,
                     label = ~htmlEscape(paste("CCT:", vert$name, "| Subgrupo:", subgrupo)),
                     labelOptions = labelOptions(direction = "bottom", 
                                                 textsize = "10px", textOnly = TRUE),
                     group = "Nodos") %>% 
    # Additionals
    # Layers control
    addLayersControl(
      overlayGroups = c("Mapa","Escuelas", "Buffers", "Envolventes convexas", 
                        "Uniones de envolventes convexas",  "Nodos", "Aristas"),
      options = layersControlOptions(collapsed = TRUE)) %>% 
    hideGroup(c("Escuelas", "Buffers", "Envolventes convexas", "Uniones de envolventes convexas")) %>% 
    clearBounds()
}

#-----------------------------
# Build graph
#-----------------------------
get_relations_v2 <- function(nodos, df, fil_dist=100) {
  relations <- df %>%
    filter(distancia<fil_dist) %>%
    # filter(numDest >1) %>% 
    mutate(proporcion = numDest/numSalen) %>%  # Tipo page-rank. numSalen 
    mutate(cct_d_u = ifelse(cct_d < cct_o, cct_d, cct_o)) %>%
    mutate(cct_o_u = ifelse(cct_d < cct_o, cct_o, cct_d)) %>%
    group_by(cct_d_u, cct_o_u) %>%
    dplyr::summarise(total_cambian = sum(numSalen),
                     flujo = sum(numDest),
                     weight = flujo /total_cambian, `.groups`="drop") %>%
    ungroup() %>% 
    rename(cct_d = cct_d_u, cct_o = cct_o_u) %>% 
    rename(name = cct_d) %>% 
    left_join(nodos, by= c("name")) %>% 
    rename( cct_d =name , latitud_d = lat, longitud_d = lon) %>% 
    rename(name = cct_o) %>% 
    left_join(nodos, by= c("name")) %>% 
    rename( cct_o =name , latitud_o = lat, longitud_o = lon) %>% 
    na.omit()
  return(relations)
}
get_select_relations <- function(nodos, current_group, relations){
  select_nodos <- nodos %>% filter(grupo == current_group)
  
  # Encontrar las relaciones entre los nodos del mismo grupo
  select_relations_o <- relations %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name)
  
  select_relations_od <- select_relations_o  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name) 
  
  select_relations_d <- relations  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name)
  
  select_relations_do <- select_relations_d  %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name) 
  
  select_relations <- rbind(select_relations_do, select_relations_od) %>% 
    distinct(cct_o, cct_d, .keep_all = TRUE)
  
  return(select_relations)
}
get_select_nodos <- function(select_relations,current_group){
  # Crear nodos 
  u_cct_o <-select_relations%>% distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  
  u_cct_d <- select_relations %>% distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  select_nodos<- rbind(u_cct_d, u_cct_o) %>% distinct(name, .keep_all = TRUE)
  
  select_nodos$grupo <- current_group 
  return(select_nodos)
}

#-----------------------------
# Community Functions
#-----------------------------
get_community_stats <- function(fc, current_group,save=TRUE){
  sizes_fc <- sizes(fc)
  stats <- data.frame(
    algoritm =   algorithm(fc),
    modularity = modularity(fc),
    num_groups =   length(fc) ,
    mean_size = mean(sizes_fc),
    median_size = median(sizes_fc),
    max_size = max(sizes_fc),
    min_size= min(sizes_fc)
  )
  
  if (save==TRUE){
    # create directory if not exists
    dir.create(file.path("./results")) 
    dir.create(file.path("./results/school_clusters")) 
    dir.create(file.path("./results/school_clusters/groups")) 
    dir.create(file.path("./results/school_clusters/groups/select_nodos"))
    
    algorithm <- str_replace(algorithm(fc), " ", "_")
    file_name <- str_c("./results/school_clusters/groups/select_nodos/algostats_",
                       algorithm, "_group_",current_group,".csv")
    write_csv(stats, file_name)
    
  }
  return(stats)
}

get_community_stats_v2 <- function(fc, current_group,save=TRUE, type){
  sizes_fc <- sizes(fc)
  stats <- data.frame(
    algoritm =   algorithm(fc),
    modularity = modularity(fc),
    num_groups =   length(fc) ,
    mean_size = mean(sizes_fc),
    median_size = median(sizes_fc),
    max_size = max(sizes_fc),
    min_size= min(sizes_fc)
  )
  
  if (save==TRUE){
    # create directory if not exists
    dir.create(file.path("../shiny_app/school_markets/results")) 
    
    type_dir <- str_c("../shiny_app/school_markets/results/", type)
    dir.create(file.path(type_dir))
    
    buff_dir <- str_c(type_dir, "/buffer_", current_group)
    dir.create(file.path(buff_dir))
    
    algo_dir <- str_c(buff_dir,"/", algorithm)
    dir.create(file.path(algo_dir))
    
    file_name <- str_c(algo_dir, "/algo_stats.csv")
    sub_df <- select_nodos %>% dplyr::select(name, grupo, sub_grupo) %>% 
      arrange(sub_grupo)
    
    write_csv(stats, file_name)
  }
  return(stats)
}

save_map <- function(select_nodos,algorithm, current_group,num_plot=10){
  top_groups <- select_nodos %>% 
    group_by(sub_grupo) %>% 
    summarise(
      n = n()
    ) %>% arrange(desc(n)) %>% 
    top_n(num_plot) %>% 
    dplyr::select(sub_grupo)
  
  plot_group <- select_nodos %>% 
    filter(!is.na(lat)) %>% 
    inner_join(top_groups)
  
  g <- qmplot(lon, lat, data = plot_group,
              colour = as.factor(sub_grupo), 
              size = I(1), 
              mapcolor = "bw")
  
  file_name <- str_c("../../data/results/school_clusters/maps/",
                     algorithm, "_group_",current_group,".png")
  ggsave(filename = file_name, g)
}

save_subgroups_v2 <- function(fc, select_nodos,algorithm,current_group, save=TRUE, buffer, type){
  fccommunity<- membership(fc)
  
  select_nodos$sub_grupo <- 0
  tam <- nrow(select_nodos)
  for (i in 1:tam){
    escuela <- select_nodos$name[i] 
    select_nodos$sub_grupo[i] <- fccommunity[escuela][[1]]
  }
  
  if (save==TRUE){
    # create directory if not exists
    dir.create(file.path("../shiny_app/school_markets/results")) 
    
    type_dir <- str_c("../shiny_app/school_markets/results/", type)
    dir.create(file.path(type_dir))
    
    buff_dir <- str_c(type_dir, "/buffer_", current_group)
    dir.create(file.path(buff_dir))
    
    # type_dir <- str_c(buff_dir,"/", type)
    # dir.create(file.path(type_dir))
    
    algo_dir <- str_c(buff_dir,"/", algorithm)
    dir.create(file.path(algo_dir))
    

    file_name <- str_c(algo_dir, "/subgroups_members.csv")
    sub_df <- select_nodos %>% dplyr::select(name, grupo, sub_grupo) %>% 
      arrange(sub_grupo)
    
    write_csv(sub_df, file_name)
  }
  return(select_nodos)
}

save_network <- function(nwk, type, current_group, algorithm){
  # create directory if not exists
  dir.create(file.path("../shiny_app/school_markets/results")) 
  
  type_dir <- str_c("../shiny_app/school_markets/results/", type)
  dir.create(file.path(type_dir))
  
  buff_dir <- str_c(type_dir, "/buffer_", current_group)
  dir.create(file.path(buff_dir))

  ntw_dir <- str_c(buff_dir,"/", algorithm)
  dir.create(file.path(ntw_dir))
  
  # save network
  file_name <- str_c(ntw_dir, "/network.rds")
  saveRDS(nwk, file_name)
}
#-----------------------------
# Subgroup stats
#-----------------------------
get_stats_sub_group <- function(current_sub_group, select_nodos){
  sub_group<- select_nodos %>% filter(sub_grupo == current_sub_group)
  
  distance_matrix <- dist(sub_group%>% as.matrix(),
                          method = 'euclidean')%>% as.matrix()
  
  distance_matrix[lower.tri(distance_matrix , diag = TRUE)] <-NA
  
  #Area envolvente convexa
  convex_hull <- CHullAreaEarth(sub_group$lon,  sub_group$lat)
  
  # Estadisticas de la escuela
  num_schools <- nrow(sub_group)
  sub_group <- sub_group %>% 
    mutate(
      privada = ifelse(substr(name, 3,3) == "P", 1, 0)
    )
  
  porcentaje_priv <- sum(sub_group$privada)/num_schools
  
  resp <- data.frame(group =current_sub_group,
                     mean_dist = mean(distance_matrix, na.rm =TRUE), 
                     max_dist =  max(distance_matrix,na.rm =TRUE),
                     min_dist =   min(distance_matrix ,na.rm =TRUE), 
                     median_dist = median(distance_matrix,na.rm =TRUE),
                     convex_hull =  convex_hull,
                     num_elem_subgroup = num_schools,
                     priv =  porcentaje_priv)
  
  return(resp)
}

#-----------------------------
# Subgroup stats
#-----------------------------
get_stats_group_v2 <- function(select_nodos, current_group, algorithm,save=TRUE, type) {
  # algorithm
  sub_results <- data.frame(group = -1,
                            mean_dist = -1,
                            max_dist = -1,
                            min_dist = -1,
                            median_dist = -1,
                            convex_hull  = -1,
                            num_elem_subgroup = -1,
                            priv = -1)
  
  list_groups <- unique(select_nodos$sub_grupo) %>% sort()
  tam <- length(list_groups) 
  for (i in 1:tam){
    curr_group <- list_groups[[i]] 
    r <- get_stats_sub_group(curr_group,select_nodos)
    sub_results <- rbind(sub_results, r)
  }
  
  sub_results <- sub_results %>% filter(priv != -1)
  
  if (save==TRUE){
    # create directory if not exists
    dir.create(file.path("../shiny_app/school_markets/results")) 
    
    type_dir <- str_c("../shiny_app/school_markets/results/", type)
    dir.create(file.path(type_dir))
    
    buff_dir <- str_c(type_dir, "/buffer_", current_group)
    dir.create(file.path(buff_dir))
    
    algo_dir <- str_c(buff_dir,"/", algorithm)
    dir.create(file.path(algo_dir))
    
    
    file_name <- str_c(algo_dir, "/subgroups_stats.csv")
    sub_df <- select_nodos %>% dplyr::select(name, grupo, sub_grupo) %>% 
      arrange(sub_grupo)
    
    write.csv(round(sub_results, 2),file_name)

  }
  
  return(sub_results)
  
}
#-----------------------------
# Network stats
#-----------------------------
get_centrality_stats_v2 <- function(school_network,current_group, save=TRUE){
  # https://rdrr.io/cran/tidygraph/man/centrality.html
  school_network  <- school_network %>%  as_tbl_graph %>% 
    tidygraph::activate(nodes) %>% 
    mutate(
      # alpha = centrality_alpha(weights = weight),
      authority = centrality_authority(weights = weight),
      betweenness = centrality_betweenness(weights = weight),
      eigen = centrality_eigen(weights = weight),
      hub = centrality_hub(weights = weight),
      pagerank = centrality_pagerank(weights = weight),
      subgraph = centrality_subgraph(),
      degree = centrality_degree()
    )
  
  resumen_central <- school_network %>% as_tibble()
  
  if (save==TRUE){

    dir.create(file.path("../shiny_app/school_markets/results")) 
    
    type_dir <- str_c("../shiny_app/school_markets/results/", type)
    dir.create(file.path(type_dir))
    
    buff_dir <- str_c(type_dir, "/buffer_", current_group)
    dir.create(file.path(buff_dir))
    
    file_name <- str_c(buff_dir, "/centrality.csv")
    write_csv(resumen_central, file_name)
  }
  
  
  return(resumen_central)
  
}

#-----------------------------
# Comparison algoritms
#-----------------------------

compare_clustering_algorithms <- function(fc, select_nodos, current_group){
  algorithm <- str_replace(algorithm(fc), " ", "_")
  select_nodos <- save_subgroups(fc, select_nodos,
                                 algorithm, current_group)
  
  #save_map(select_nodos,algorithm, current_group)
  get_stats_group(select_nodos, algorithm, current_group)
  return(get_community_stats(fc, current_group)) 
}

get_new_nodes <- function(algorithm, current_group){
  algorithm <- str_replace(algorithm, " ", "_")
  file_name <- str_c("../../data/results/school_clusters/groups/select_nodos/",
                     algorithm, "_group_",current_group,".csv")
  new_nodos <- read.csv(file_name)
  return(new_nodos)
}


########## Big function
comp_communities <- function(buffer, nodos, df, algorithm, save=T) {
  current_group <- buffer
  relations <- get_relations(nodos, df)
  
  select_relations <- get_select_relations(nodos, current_group, relations)
  select_nodos <- get_select_nodos(select_relations, current_group)
  
  school_network <- graph_from_data_frame(select_relations, directed=FALSE, 
                                          vertices=select_nodos)
  
  tbl_centrality <- get_centrality_stats(school_network, current_group, save=save)
  
  if (algorithm=="fg"){
    fc <<- cluster_fast_greedy(school_network) 
  } else if (algorithm=="wt"){
    fc <<- cluster_walktrap(school_network)
  } else if (algorithm=="lp"){
    fc <<- cluster_label_prop(school_network)
  } else if (algorithm=="le"){
    fc <<- cluster_leading_eigen(school_network)
  } else if (algorithm=="cl"){
    fc <<- cluster_louvain(school_network)
  }
  
  algorithm <- str_replace(algorithm(fc), " ", "_")
  select_nodos <- save_subgroups(fc, select_nodos,
                                 algorithm, current_group, save=save)
  # tables with community members
  mrkt_members_tbl <- tbls_mrkt_members(select_nodos, current_group, algorithm, save=save)
  
  output <- list("mem_list"=mrkt_members_tbl$miembros,
                 "com_stats"=mrkt_members_tbl$com_stats,
                 "algo_stats"=mrkt_members_tbl$algo_stas,
                 "central_stats"=tbl_centrality,
                 "selected_nodos"= select_nodos, 
                 "select_relations"=select_relations,
                 "relations"=relations)
}

###############

## Tables for shiny
tbls_mrkt_members_v2 <- function(select_nodos, current_group, algorithm, save=T, type) {
  # School of each market
  tbl_miembros <- select_nodos %>% rename(CCT=name, Latitud=lat, Longitud=lon, Mercado=sub_grupo) %>% 
    arrange(Mercado) %>% 
    mutate_at(vars(Mercado), funs(factor))
  tbl_miembros <- tbl_miembros[c("CCT", "Mercado", "Latitud", "Longitud" )]
  # community stats
  com_stats <- get_stats_group_v2(select_nodos, current_group, algorithm, save, type)%>% 
    filter(group>0) %>% 
    mutate_at(vars(group), funs(factor)) %>% 
    rename(Mercado=group, DistMed=mean_dist, MaxDist=max_dist, MinDist=min_dist,
           MedDist=median_dist, Area=convex_hull, N=num_elem_subgroup, Privs=priv) %>% 
    select(Mercado, N, everything()) %>% arrange(-N) %>% round_df(4)
  # algorithm stats
  algo_tbl <- get_community_stats_v2(fc, current_group, save, type) %>% 
                    rename(Algoritmo=algoritm, 
                    Modularidad=modularity,
                    Mercados=num_groups,
                    Media=mean_size,
                    Mediana=median_size,
                    Max=max_size,
                    Min=min_size) %>% round_df(2)
  
  out <- list("miembros"=tbl_miembros, "com_stats"=com_stats, "algo_stas"=algo_tbl)
  return(out)
}


comm_stats_rep <- function(fc, algorithm) {
  tbl_mrkt_grl <- get_community_stats(fc, current_group)
  tbl_mrkt_grl <- tbl_mrkt_grl %>% 
    rename(Algoritmo=algoritm, Modularidad=modularity, `Nro. Mercados`=num_groups,
           `Tamaño promedio`=mean_size, `Mediana del tamaño`=median_size, 
           `Mayor tamaño`=max_size, `Menor Tamaño`=min_size) %>% 
    round_df(digits=2)
  return(tbl_mrkt_grl)
}

format_ctl <- function(central_stats) {
  central_stats %>% 
    select(-grupo) %>%
    round_df(1) %>% 
    arrange(-betweenness) %>% 
    select(name, betweenness, subgraph, degree, #alpha,
           pagerank, authority, eigen, hub) %>% 
    rename(CCT=name, Betweenness=betweenness,
           Subgraph=subgraph, Degree=degree, #Alpha=alpha
           Pagerank=pagerank, Authority=authority, Eigen=eigen, Hub=hub) %>% 
    kable("html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
    column_spec(1, width = "4cm") 
}

df_buffers_join <- function(df, df_buffers) {
  # See: http://cumplimientopef.sep.gob.mx/glosario_de_terminos/
  preescolar <- c("CC", "JN") # indígena y general
  secundarias <- c("ES", "ST", "TV") # indígena, general y telesecundaria
  primarias <- c("PB") # indígena y general
  
  cct_o <-df %>% mutate(tipo_educ_o = substr(cct_o, 4,5), tipo_educ_d = substr(cct_o, 4,5)) %>% 
    filter(tipo_educ_o %in% secundarias && tipo_educ_d %in% secundarias) %>% 
    distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  cct_d <-df %>% mutate(tipo_educ_o = substr(cct_o, 4,5), tipo_educ_d = substr(cct_o, 4,5)) %>% 
    filter(tipo_educ_o %in% secundarias && tipo_educ_d %in% secundarias) %>% 
    distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  schools_df <- rbind(cct_d, cct_o) %>% distinct(name, .keep_all = TRUE) %>% 
    rename(cct = name, latitud=lat, longitud=lon)
  
  
  schools_df <- left_join(df_buffers, schools_df)
  return(schools_df)
}

format_mrcds  <- function(com_stats){
  legends <- c("N: Número de escuelas;",
               "DistMed: Distancia media (kms);", 
               "MaxDist: Máxima distancia (kms);",
               "MinDist: Mínima distancia (kms);",
               "MedDist: Distancia media (kms);",
               "Area: Area de Envolvente convexa (kms2);",
               "Privs: Porcentaje de escuelas privadas (%).")
  com_stats %>% mutate(N = color_bar("lightgreen")(N)) %>% 
    kable("html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
    column_spec(1, width = "3cm") %>%
    footnote(number = c(legends))
}

format_algo  <- function(algo_stats){
  algo_stats %>% kable("html", escape = F) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F, position = "left")%>% 
    column_spec(1, width = "4cm") %>% 
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
}

format_mbrs  <- function(mkt_members_tbl){
  mkt_members_tbl %>% kbl(booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), font_size = 8,
                  position = "float_right") %>% 
    scroll_box(width = "230px", height = "520px")
}

format_buff_cts  <- function(dfbuffers){
  # footnotes
  num_total <- dfbuffers %>% distinct(buffer) %>% count()
  num_more1 <- dfbuffers %>% group_by(buffer) %>% 
    summarize(n=n()) %>% filter(n>1) %>% count() 
  num_more2 <- dfbuffers %>% group_by(buffer) %>% 
    summarize(n=n()) %>% filter(n>2) %>% count()
  num_more3 <- dfbuffers %>% group_by(buffer) %>% 
    summarize(n=n()) %>% filter(n>3) %>% count() 
  
  note1 <- paste("Total de zonas de desplazamiento:", num_total)
  note2 <- paste("Zonas de desplazamiento con más de 1 escuela:", num_more1, 
                 "(", round(num_more1*100/num_total, 2), "%)")
  note3 <- paste("Zonas de desplazamiento con más de 2 escuelas:", num_more2, 
                 "(", round(num_more2*100/num_total, 2), "%)")
  note4 <- paste("Zonas de desplazamiento con más de 3 escuelas:", num_more3, 
                 "(", round(num_more3*100/num_total, 2), "%)")
  
  dfbuffers %>% group_by(buffer) %>% 
    rename(`Zona de Deplazamiento`=buffer) %>% 
    summarize(n=n()) %>% 
    arrange(-n) %>% 
    kbl() %>%
    kable_minimal(full_width = F, position = "left") %>% 
    kable_styling(bootstrap_options = c("striped", "hover"), font_size = 10) %>% 
    scroll_box(width = "230px", height = "200px") %>% 
    footnote(alphabet = c(note1, note2, note3, note4))
}
#### Additionals
scale <- function(x, t_min, t_max){
  r_min <- min(x)
  r_max <- max(x)
  z <-(t_max-t_min)/(r_max-r_min)
  x_scaled <- (x-r_min)*z + t_min
  return(x_scaled)
}

round_df <- function(df, digits) {
  # taken from: 
  # https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}