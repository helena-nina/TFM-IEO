### ------------------------------------------------------------------------ ###
### LBSPR ####
### ------------------------------------------------------------------------ ###

library(LBSPR)
library(ggpubr)
library(cowplot)

#install.packages("devtools")
#devtools::install_github("AdrianHordyk/LBSPR")

### Populate LB_pars common sole

solePars <- new("LB_pars")
solePars@Linf <- 48.9 
solePars@L50 <- 26 
solePars@L95 <- 27.5
solePars@MK <- 1.41 
solePars@M <- 0.31 
solePars@L_units <- "cm"

### We load size composition data for each scenario

#load("~/TFM-IEO/input/freq_list_fmsy.RData")
#load("~/TFM-IEO/input/freq_list_0.5fmsy.RData")
#load("~/TFM-IEO/input/freq_list_fcrash.RData")
load("~/TFM-IEO/input/freq_list_scn4.RData")

### We check that they have been imported correctly
head(freq_list)

### save de data as an .csv

lapply(seq_along(freq_list), function(i) {
  write.csv(freq_list[[i]], file.path("input/LBSPR/freq_list",paste0(names(freq_list)[i],".csv")), row.names = FALSE)
})

### read data

### Specify the complete path
dir_path <- file.path(getwd(), "input/LBSPR/freq_list")

### Get the list of names of CSV files in the folder 
file_names <- list.files(dir_path, pattern = "\\.csv$")

### Read each CSV file in the list and store them in one list
data_freq <- lapply(file_names, function(file_name) {
  file_path <- file.path(dir_path, file_name)
  read.csv(file_path, stringsAsFactors = FALSE)
})


### LB_lengths object read from sole length frequency cvs file and plot

### Set working directory to the folder containing the .csv files

setwd("input/LBSPR/freq_list")

### list of all .csv files in the folder

archivos_csv <- list.files(pattern = "*.csv")

### Use a for loop to read each .csv file one by one and save it in the working environment
### as an object with the same name as the file.

for (file in file_csv) {
  # Read the .csv file and save the data in an object with the same name as the file
  length <- gsub(".csv", "", file) # Get file name without .csv extension
  assign(length, read.csv(file)) # Save the data in an object with the same name as the file
}

niter <- length(freq_list)
soleLenFreq <- list()
soleFit <- list()

#start_time <- Sys.time()

for (i in 1:niter) {
 
  soleLenFreq[[i]] <- new("LB_lengths", LB_pars=solePars,file=paste0(i,".csv"), dataType="freq", header=TRUE)
  soleLenFreq[[i]]@L_units <- solePars@L_units
  soleFit[[i]] <- LBSPRfit(solePars, soleLenFreq[[i]]) ##Fitting the  model
  #print(i)
}

#end_time <- Sys.time()
#end_time - start_time

#2.56 hours

#save(soleFit, file = "input/soleFit_scn1.RData")
#save(soleFit, file = "input/soleFit_scn2.RData")
#save(soleFit, file = "input/soleFit_scn3.RData")
#save(soleFit, file = "input/soleFit_scn4.RData")

soleFit_list <- list()

### individual point estimates for each year for each iter

for (i in 1:niter) {
  soleFit_list[[i]] <- data.frame(rawFM=soleFit[[i]]@FM, rawSPR=soleFit[[i]]@SPR)
}


### ------------------------------------------------------------------------ ###
### Plots ####
### ------------------------------------------------------------------------ ###

SPR <- data.frame(iter1 = soleFit_list[[1]][,2])

for (i in 1:(niter)-1) {
  new_col <-  soleFit_list[[i+1]][,2]
  SPR[ , i+1] <- new_col
  colnames(SPR)[i+1] <- paste0("iter", i+1)
}

SPR <- cbind(years=c(1:100),SPR)
#save(SPR, file = "input/SPR_fmsy.RData")

### quantitative measures
medians_SPR  <- apply(SPR, 1, median) 
SPR_Q5 <- apply(SPR , 1, quantile, probs = 0.05)
SPR_Q95 <- apply(SPR , 1, quantile, probs = 0.95)



years <- 1:100
SPRgg <- data.frame(years, medians_SPR,SPR_Q5, SPR_Q95)

spr_plot = data.frame(Year=rep(SPRgg$years,3),
                      spr=c(medians_SPR,SPR_Q5,SPR_Q95),
                      lines = c(rep("median",100),
                                rep("quantile 5",100),
                                rep("quantile 95",100)))

p1 <- ggplot(spr_plot,aes(x=Year,y=spr, group = lines)) +
  geom_line(aes(linetype = lines, color = lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  xlab("Years")+ylab("SPR") + theme(legend.title =element_blank())



### F/M ratio

FM <- data.frame(iter1 = soleFit_list[[1]][,1])

for (i in 1:(niter-1)) {
  new_col <-  soleFit_list[[i+1]][,1]
  FM[ , i+1] <- new_col
  colnames(FM)[i+1] <- paste0("iter", i+1)
}

FM <- cbind(years=c(1:100),FM)
#save(FM, file = "input/FM_fmsy1000.RData")
medians_FM  <- apply(FM, 1, median) ## medians
FM_Q5 <- apply(FM , 1, quantile, probs = 0.05)
FM_Q95 <- apply(FM , 1, quantile, probs = 0.95)

years <- 1:100
FMgg <- data.frame(years, medians_FM,FM_Q5, FM_Q95)

FM_plot = data.frame(Year=rep(FMgg$years,3),
                     fm=c(medians_FM,FM_Q5,FM_Q95),
                     lines = c(rep("median",100),
                               rep("quantile 5",100),
                               rep("quantile 95",100)))


p2 <- ggplot(FM_plot,aes(x=Year,y=fm, group = lines)) +
  geom_line(aes(linetype = lines, color = lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  xlab("Years")+ylab("F/M") + theme(legend.title =element_blank())



#jpeg("LBSPR_0.5fmsy.jpeg", width = 2100, height = 1100, res = 300)
ggarrange(p1, p2, 
          ncol = 2, nrow = 1,
          common.legend = TRUE,
          legend = "bottom")
#dev.off()

