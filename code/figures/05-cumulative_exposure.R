filespaths <- dir("/Users/jacobdale/tryitout/rasterbird/data/data-format/bird-warp", recursive = TRUE, full.names = TRUE)
stressor_names <- c("Built1994", "Built2009", "croplands1992", "croplands2005", "2008-inorganic", "2013-inorganic",
                    "2008-invasives", "2013-invasives","2008-night_lights", "2008-ocean_pollution", "2008-plumes_fert",
                    "2008-plumes_pest","2008-population", "2008-shipping", "2013-night_lights", "2013-ocean_pollution",
                    "2013-plumes_fert","2013-plumes_pest", "2013-population", "2013-shipping", "HFP1993_int", "HFP1993.tif", 
                    "HFP2009_int", "HFP2009.tif", "Lights1994", "Lights2009", "NavWater1994", "NavWater2009", "Pasture1993", 
                    "Pasture2009", "Popdensity1990", "Popdensity2010", "Railways", "Roads")
datlog <- function(x) {
  log(x+1)
}
quantNorm <- function(x, quantile99) { 
  x <- x / quantile99 
  x[x > 1] <- 1
  x[x < 0] <- 0
  x
}

dat <- list()
for(i in 1:length(stressor_names)) {
  # Get filepaths for stressor i
  uid <- stringr::str_detect(filespaths, stressor_names[i]) 
  files <- filespaths[uid] 
  
  # Load tif files for stressor i
  dat[[i]] <- lapply(files, stars::read_stars) 
  
  # Get values of stressor i for all birds and store in list
  vals <- list()
  for (j in 1:length(dat[[i]])) {
    r <- as.data.frame(dat[[i]][[j]]) 
    r <- r[,3] 
    vals[[j]] <- r[r>0]
    vals[[j]] <- na.omit(vals[[j]])
  }
  
  # Evaluate 99th percentile for stressor i across all birds
  quantile99 <- unlist(vals) |> quantile(probs = .99, na.rm = T) 
  
  dat[[i]] <- lapply(dat[[i]], datlog)
  # Standardize individual bird rasters based on 99th percentile of stressor i across all birds
  dat[[i]] <- lapply(dat[[i]], quantNorm, quantile99)
  
}
# Restructure data to be stressors by birds
dat_bird <- list()
for(i in 1:length(dat[[1]])) dat_bird[[i]] <- list() 
for(i in 1:length(dat[[1]])) {
  for(j in 1:length(dat)) {
    dat_bird[[i]][[j]] <- dat[[j]][[i]]
  }
}

dat_amgplo <- list()
for(i in 1:1) {
  for(j in 1:34){
    dat_amgplo[[j]] <- dat_bird[[i]][[j]]
  }
}

dat_bairds <- list()
for(i in 2:2) {
  for(j in 1:34){
    dat_bairds[[j]] <- dat_bird[[i]][[j]]
  }
}

dat_buff <- list()
for(i in 3:3) {
  for(j in 1:34){
    dat_buff[[j]] <- dat_bird[[i]][[j]]
  }
}

dat_cack <- list()
for(i in 4:4) {
  for(j in 1:34){
    dat_cack[[j]] <- dat_bird[[i]][[j]]
  }
}

dat_comm <- list()
for(i in 5:5) {
  for(j in 1:34){
    dat_comm[[j]] <- dat_bird[[i]][[j]]
  }
}

dat_glauc <- list()
for(i in 6:6) {
  for(j in 1:34){
    dat_glauc[[j]] <- dat_bird[[i]][[j]]
  }
}

cumul_ap_amgplo <- c(dat_amgplo[[5]], dat_amgplo[[6]], dat_amgplo[[7]],dat_amgplo[[8]],dat_amgplo[[9]],dat_amgplo[[10]],
                     dat_amgplo[[11]],dat_amgplo[[12]],dat_amgplo[[13]],dat_amgplo[[14]],dat_amgplo[[15]],dat_amgplo[[16]],
                     dat_amgplo[[17]],dat_amgplo[[18]],dat_amgplo[[19]],dat_amgplo[[20]],dat_amgplo[[21]], dat_amgplo[[22]],
                     dat_amgplo[[23]],dat_amgplo[[24]],dat_amgplo[[27]],dat_amgplo[[28]],dat_amgplo[[34]],along = "z")

cumul_ap <- c(dat_bairds[[5]], dat_bairds[[6]], dat_bairds[[7]],dat_bairds[[8]],dat_bairds[[9]],dat_bairds[[10]],
              dat_bairds[[11]],dat_bairds[[12]],dat_bairds[[13]],dat_bairds[[14]],dat_bairds[[15]],dat_bairds[[16]],
              dat_bairds[[17]],dat_bairds[[18]],dat_bairds[[19]],dat_bairds[[20]],dat_bairds[[21]], dat_bairds[[22]],
              dat_bairds[[23]],dat_bairds[[24]],dat_bairds[[27]],dat_bairds[[28]],dat_bairds[[34]],along = "z")

cumul_ap <- c(dat_buff[[5]], dat_buff[[6]], dat_buff[[7]],dat_buff[[8]],dat_buff[[9]],dat_buff[[10]],
              dat_buff[[11]],dat_buff[[12]],dat_buff[[13]],dat_buff[[14]],dat_buff[[15]],dat_buff[[16]],
              dat_buff[[17]],dat_buff[[18]],dat_buff[[19]],dat_buff[[20]],dat_buff[[21]], dat_buff[[22]],
              dat_buff[[23]],dat_buff[[24]],dat_buff[[27]],dat_buff[[28]],dat_buff[[34]],along = "z")

cumul_ap <- c(dat_cack[[5]], dat_cack[[6]], dat_cack[[7]],dat_cack[[8]],dat_cack[[9]],dat_cack[[10]],
              dat_cack[[11]],dat_cack[[12]],dat_cack[[13]],dat_cack[[14]],dat_cack[[15]],dat_cack[[16]],
              dat_cack[[17]],dat_cack[[18]],dat_cack[[19]],dat_cack[[20]],dat_cack[[21]], dat_cack[[22]],
              dat_cack[[23]],dat_cack[[24]],dat_cack[[27]],dat_cack[[28]],dat_cack[[34]],along = "z")

cumul_ap <- c(dat_comm[[5]], dat_comm[[6]], dat_comm[[7]],dat_comm[[8]],dat_comm[[9]],dat_comm[[10]],
              dat_comm[[11]],dat_comm[[12]],dat_comm[[13]],dat_comm[[14]],dat_comm[[15]],dat_comm[[16]],
              dat_comm[[17]],dat_comm[[18]],dat_comm[[19]],dat_comm[[20]],dat_comm[[21]], dat_comm[[22]],
              dat_comm[[23]],dat_comm[[24]],dat_comm[[27]],dat_comm[[28]],dat_comm[[34]],along = "z")

cumul_ap <- c(dat_glauc[[5]], dat_glauc[[6]], dat_glauc[[7]],dat_glauc[[8]],dat_glauc[[9]],dat_glauc[[10]],
              dat_glauc[[11]],dat_glauc[[12]],dat_glauc[[13]],dat_glauc[[14]],dat_glauc[[15]],dat_glauc[[16]],
              dat_glauc[[17]],dat_glauc[[18]],dat_glauc[[19]],dat_glauc[[20]],dat_glauc[[21]], dat_glauc[[22]],
              dat_glauc[[23]],dat_glauc[[24]],dat_glauc[[27]],dat_glauc[[28]],dat_glauc[[34]],along = "z")

# "Basic" plot
cumul_sum <- stars::st_apply(cumul_ap_amgplo, c(1,2), sum)
plot(cumul_sum, breaks = "equal")