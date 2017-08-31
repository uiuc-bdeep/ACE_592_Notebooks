PROJECT_NAME <- "Apartments"

#source("/data/projects/library/PathFunctions_cmd.R")
source("/home/rstudio/share/projects/library/PathFunctions_cmd.R")

codeForState <- function (state_name) {
  if (!exists('stateCodeMap')) {
    stateCodeMap = read.csv(p_root('stores/state_code_map.csv'))
  }
  code = stateCodeMap[which(stateCodeMap$state==state_name),"code"]
  if (nchar(code) < 2) {
    code = paste0("0",code)
  }
  return(code)
}

getPOPDataForState <- function (data,state_name) {
  # Get layer name & path
  layer_name = paste0("tl_2016_",codeForState(state_name),"_tabblock10")
  #shapefile_path = root(paste0("CensusBlocks/",codeForState(state_name)))
  shape_object_path = p_root(paste0("stores/shapes/",codeForState(state_name),"_shp.rds"))
  
  if (!file.exists(shape_object_path)) {
    return(NA)
  }
  
  #if (file.exists(paste0(shape_object_path,"_res.rds"))) {
  #  return(NA)
  #}
  
  # Grab just rows for the state we're processing
  state_subset = subset(data,state==state_name)
  
  # If there are no rows for this state, do nothing
  if(dim(state_subset)[1]==0) {
    return(NA)
  } else {
    
    points_raw = state_subset
    points_raw$lat = as.numeric(points_raw$lat)
    points_raw$lon = as.numeric(points_raw$lon)
    source(scripts("PointsOverPolygons.R"),local=TRUE)
    if (!exists("aug_points")) {
      stop("PointsOverPolygons.R failed to create membership data.")
    }
  }
  return(aug_points)
}

# Connect to Mongo
source(scripts('ConnectDB_cmd.R'))

# Find all rows w/o membership data
data = db.raw$find('{"state" : "IL"}')
states = "IL"

# Do POP operation for rows pertaining to each state
print("Adding POP data...")
total_states = length(states)

aug_data_by_state = lapply(states,function(state_name) {
  print(paste0("Working on ",state_name,"..."))
  if (state_name == "") {
    return(NA)
  }
  aug_data = getPOPDataForState(data,state_name)
  return(aug_data)
})

# Remove any states that had no rows in them
aug_data_by_state_no_NAs = aug_data_by_state[which(!is.na(aug_data_by_state))]
aug_data = Reduce(rbind,aug_data_by_state_no_NAs,aug_data_by_state_no_NAs[[1]][c(),])

# Remove rows from collection that didn't have membership data
#db.raw$remove(query='{}',multiple = TRUE)

# Replace them with the rows we just processed
if (db.raw_aug_IL$count() != 0){
  db.raw_aug_IL$drop()
}
db.raw_aug_IL$insert(aug_data)
