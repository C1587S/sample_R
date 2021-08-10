## Script computing buffers

source('utils.R')
library(leaflet)
library(mapview)
library(htmltools)

#### Load the data
db <- readRDS("../data/master_db_filtered.rds")

#### Preparing the data for computing buffers
db$latitud <- as.numeric(db$latitud)
db$longitud <- as.numeric(db$longitud)

## High School
# schools with migrations data
dic_sec <- readRDS("../data/agregados/agregado_dist_sec_v2.rds")
# unique schools using orig
unique_orig_sec <- dic_sec %>% select(cct_o) %>% unique() %>% rename(cct = cct_o)
# unique schools using dest
unique_dest_sec <- dic_sec %>% select(cct_d) %>% unique() %>% rename(cct = cct_d)
# db secundaria
db_sec <- dplyr::left_join(unique_orig_sec, db) %>% na.omit()

#### Calculate buffers
for (buff in radius_list){
  calc_buffers_v2(db_sec, buff, save=TRUE)  
}

### Compute communities
# Read buffers
dfbuffers <- readRDS("../data/buffers/df_buffers_5kms_sec.rds")$df_buffers
type <- "5kms"
projs <- readRDS("../data/buffers/df_buffers_5kms_sec.rds")$buffers_proj

# buffer list
buff_list <- buffs %>% group_by(buffer) %>%
  summarise(n=n()) %>% 
  filter(n>1) %>% 
  arrange(n) %>%  
  select(buffer) %>% 
  head() %>% 
  pull()

# algorithms list
algo_list <- c("fg", "wt", "lp", "le", "cl")

#  School migration data
df <- readRDS("../data/agregados/agregado_dist_sec_v2.rds")
save <- TRUE
valid_buffers_comm <- c() # stores buffers with at least one connection
i <- 1
for (buff in buff_list){
  current_group <- buff
  
  nodos <- dfbuffers %>% filter(buffer==current_group) %>% rename(grupo=buffer, name=cct, lat=latitud, lon=longitud)
  relations <- get_relations(nodos, df)
  
  select_relations <- get_select_relations(nodos, current_group, relations)
  select_nodos <- get_select_nodos(select_relations, current_group)
  
  if (nrow(select_relations)>0){
    valid_buffers_comm[i] <-current_group
  } else {
    next
  }
  # extract graph and compute centrality stats
  school_network <- graph_from_data_frame(select_relations, directed=FALSE, vertices=select_nodos)
  tbl_centrality <- get_centrality_stats_v2(school_network, current_group, save=save)
  
  # calculate clusters
  for (algo in algo_list){
    print(paste(buff, algo))  
    
    algorithm <- algo
    
    if (algorithm=="fg"){
      fc <<- cluster_fast_greedy(school_network) 
    } else if (algorithm=="wt"){
      fc <- cluster_walktrap(school_network)
    } else if (algorithm=="lp"){
      fc <- cluster_label_prop(school_network)
    } else if (algorithm=="le"){
      fc <- cluster_leading_eigen(school_network)
    } else if (algorithm=="cl"){
      fc <- cluster_louvain(school_network)
    }
    
    algorithm <- str_replace(algorithm(fc), " ", "_")
    select_nodos <- save_subgroups_v2(fc, select_nodos, algorithm, current_group, save=save, buffer=current_group, type=type)
    # tables with community members
    mrkt_members_tbl <- tbls_mrkt_members_v2(select_nodos, current_group, algorithm, save=save, type=type)
    
  }
  i <- i + 1
}

path <- str_c("../shiny_app/school_markets/results/", type, "/buffers_with_comms.csv")
write_csv(data.frame("buffers_with_communities"=valid_buffers_comm), path)


