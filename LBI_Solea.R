### ------------------------------------------------------------------------ ###
### LBI ####
### ------------------------------------------------------------------------ ###
#knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_knit$set(root.dir = "D:/psampedro/Documents/DATA_LIMITED_STOCKS/LBI")

library(kableExtra)
library(tidyverse)
library(LBSPR) 
library(reshape2)
library(ggplot2) 
library(tidyr)
library(ReporteRs)
source("https://raw.githubusercontent.com/ices-tools-dev/LBIndicator_shiny/master/utilities.R") # includes the option of m_k
source("utilities_vpaz.R") # Historical series traffic light table function, modified lb_plot settings.
library(ggpubr)
library(cowplot)

### We load size composition data for each scenario

#load("~/TFM-IEO/input/freq_list_fmsy.RData")
#load("~/TFM-IEO/input/freq_list_0.5fmsy.RData")
#load("~/TFM-IEO/input/freq_list_fcrash.RData")
load("~/TFM-IEO/input/freq_list_scn4.RData")

### We check that they have been imported correctly
head(freq_list)

### weights
#load("~/TFM-IEO/input/wei_list_fmsy.RData")
#load("~/TFM-IEO/input/wei_list_0.5fmsy.RData")
#load("~/TFM-IEO/input/wei_list_fcrash.RData")
load("~/TFM-IEO/input/wei_list_scn4.RData")

### We check that they have been imported correctly
head(wei_list)

### length distribution plot with bin_plot

### traffic light table
lb_tableSH(freq_list[[1]][, 1:101], 1, "cm", 48.9, 26, 1.41,wei_list[[1]][,1:101])
#lb_tableSH(freq_list[[1]][, c(1,11:21)], 1, "cm", 48.9, 26, 1.41,wei_list[[1]][, c(1,11:21)])

### Estimates of the defined indicators 
### one iter

#lb_ind(freq[,1:11], 1, 48.9,26,1.41,wei[,1:11])

### all iters
niter <- length(freq_list)
ind <- list()
for (i in 1:niter) {
  ind[[i]] <- lb_ind(freq_list[[i]], 1, 48.9,26,1.41,wei_list[[i]])
}

#save(ind, file = "input/ind_list_fmsy.RData")
#save(ind, file = "input/ind_list_crash.RData")
#save(ind, file = "input/ind_list_0.5fmsy.RData")
#save(ind, file = "input/ind_list_mix.RData")

### Data for each ind 
### Lc_Lmat

Lc_Lmat <- data.frame(iter1 = ind[[1]][,16])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,16]
  Lc_Lmat[ , i+1] <- new_col
  colnames(Lc_Lmat)[i+1] <- paste0("iter", i+1)
}

Lc_Lmat <- cbind(year=ind[[1]][,1],Lc_Lmat)
#save(Lc_Lmat, file = "input/Lc_Lmat_fmsy.RData")

### quantitative measures
medians_Lc_Lmat <- apply(Lc_Lmat[,-1], 1, median) ## medians
Lc_LmatQ5 <- apply(Lc_Lmat[,-1], 1, quantile, probs = 0.05)
Lc_LmatQ95 <- apply(Lc_Lmat[,-1], 1, quantile, probs = 0.95)


### L25_Lmat

L25_Lmat <- data.frame(iter1 = ind[[1]][,17])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,17]
  L25_Lmat[ , i+1] <- new_col
  colnames(L25_Lmat)[i+1] <- paste0("iter", i+1)
}

L25_Lmat <- cbind(year=ind[[1]][,1],L25_Lmat)
#save(L25_Lmat, file = "input/L25_Lmat_fmsy.RData")

### quantitative measures
medians_L25_Lmat <- apply(L25_Lmat[,-1], 1, median) ## medians
L25_LmatQ5 <- apply(L25_Lmat[,-1], 1, quantile, probs = 0.05) ## Q5
L25_LmatQ95 <- apply(L25_Lmat[,-1], 1, quantile, probs = 0.95) ## Q95


### Lmax5_Linf

Lmax5_Linf <- data.frame(iter1 = ind[[1]][,22])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,22]
  Lmax5_Linf[ , i+1] <- new_col
  colnames(Lmax5_Linf)[i+1] <- paste0("iter", i+1)
}

Lmax5_Linf <- cbind(year=ind[[1]][,1],Lmax5_Linf)
#save(Lmax5_Linf, file = "input/Lmax5_Linf_fmsy.RData")

### quantitative measures
medians_Lmax5_Linf <- apply(Lmax5_Linf[,-1], 1, median) ## medians
Lmax5_LinfQ5 <- apply(Lmax5_Linf[,-1], 1, quantile, probs = 0.05)
Lmax5_LinfQ95 <- apply(Lmax5_Linf[,-1], 1, quantile, probs = 0.95)


### Pmega

Pmega <- data.frame(iter1 = ind[[1]][,23])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,23]
  Pmega[ , i+1] <- new_col
  colnames(Pmega)[i+1] <- paste0("iter", i+1)
}

Pmega <- cbind(year=ind[[1]][,1],Pmega)
#save(Pmega, file = "input/Pmega_fmsy.RData")

### quantitative measures
medians_Pmega  <- apply(Pmega[,-1], 1, median) ## medians
PmegaQ5 <- apply(Pmega[,-1] , 1, quantile, probs = 0.05)
PmegaQ95 <- apply(Pmega[,-1] , 1, quantile, probs = 0.95)

### Lmean_Lopt

Lmean_Lopt <- data.frame(iter1 = ind[[1]][,19])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,19]
  Lmean_Lopt[ , i+1] <- new_col
  colnames(Lmean_Lopt)[i+1] <- paste0("iter", i+1)
}

Lmean_Lopt <- cbind(year=ind[[1]][,1],Lmean_Lopt)
#save(Lmean_Lopt, file = "input/Lmean_Lopt_fmsy.RData")

### quantitative measures
medians_Lmean_Lopt  <- apply(Lmean_Lopt[,-1], 1, median) ## medians
Lmean_LoptQ5 <- apply(Lmean_Lopt[,-1] , 1, quantile, probs = 0.05)
Lmean_LoptQ95 <- apply(Lmean_Lopt[,-1] , 1, quantile, probs = 0.95)


### Lmean_L_FeM

Lmean_L_FeM <- data.frame(iter1 = ind[[1]][,15])

for (i in 1:(niter-1)) {
  new_col <-  ind[[i+1]][,15]
  Lmean_L_FeM[ , i+1] <- new_col
  colnames(Lmean_L_FeM)[i+1] <- paste0("iter", i+1)
}

Lmean_L_FeM <- cbind(year=ind[[1]][,1],Lmean_L_FeM)
#save(Lmean_L_FeM, file = "input/Lmean_L_FeM_fmsy.RData")

### quantitative measures
medians_Lmean_L_FeM  <- apply(Lmean_L_FeM[,-1], 1, median) ## medians
Lmean_L_FeMQ5 <- apply(Lmean_L_FeM[,-1] , 1, quantile, probs = 0.05)
Lmean_L_FeMQ95 <- apply(Lmean_L_FeM[,-1] , 1, quantile, probs = 0.95)


### ------------------------------------------------------------------------ ###
### Plots ####
### ------------------------------------------------------------------------ ###

### years of simulation
years <- 1:100

### bibliography references
ref_Lc_Lmat <- rep(1,100)
ref_L25_Lmat <- rep(1,100)
ref_Lmax5_Linf <- rep(0.8,100)
ref_Pmega <- rep(0.3,100)
ref_Lmean_Lopt <- rep(0.9,100)
ref_Lmean_L_FeM <- rep(1,100)

### plots for scenario 1

### Lc_Lmat

Lc_Lmatgg <- data.frame(years, medians_Lc_Lmat, Lc_LmatQ5, Lc_LmatQ95,ref_Lc_Lmat)

df_plot=data.frame(Year=rep(Lc_Lmatgg$years,4),
                   Lc=c(medians_Lc_Lmat,Lc_LmatQ5,Lc_LmatQ95,ref_Lc_Lmat),
                   lines=c(rep("median",100),
                           rep("quantile 5",100),
                           rep("quantile 95",100),
                           rep("reference",100)))


p1 <-ggplot(df_plot,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0.78, max(df_plot$Lc)+0.2)) +
  xlab("Years")+ylab("Lc/Lmat") + theme(legend.title =element_blank())


### L25_Lmat

L25_Lmatgg <- data.frame(years, medians_L25_Lmat, L25_LmatQ5, L25_LmatQ95,ref_L25_Lmat)

df_plot2=data.frame(Year=rep(L25_Lmatgg$years,4),
                    Lc=c(medians_L25_Lmat,L25_LmatQ5,L25_LmatQ95,ref_L25_Lmat),
                    lines = c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100)))

p2 <- ggplot(df_plot2,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0.78, max(df_plot$Lc)+0.2)) +
  xlab("Years")+ylab("L25/Lmat") + theme(legend.title =element_blank())

### Lmax5_Linf

Lmax5_Linfgg <- data.frame(years, medians_Lmax5_Linf, Lmax5_LinfQ5, Lmax5_LinfQ95,ref_Lmax5_Linf)
df_plot3 = data.frame(Year=rep(Lmax5_Linfgg$years,4),
                      Lc=c(medians_Lmax5_Linf,Lmax5_LinfQ5,Lmax5_LinfQ95,ref_Lmax5_Linf),
                      lines=c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100)))

p3 <- ggplot(df_plot3,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0.78, max(df_plot$Lc))) +
  xlab("Years")+ylab("Lmax5/Linf") + theme(legend.title =element_blank())

### Pmega

Pmegagg <- data.frame(years, medians_Pmega, PmegaQ5, PmegaQ95,ref_Pmega)

df_plot4 = data.frame(Year=rep(Pmegagg$years,4),
                      Lc=c(medians_Pmega,PmegaQ5,PmegaQ95,ref_Pmega),
                      lines=c(rep("median",100),
                              rep("quantil 5",100),
                              rep("quantil 95",100),
                              rep("reference",100)))
p4 <- ggplot(df_plot4,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0, max(df_plot4$Lc))) +
  xlab("Years")+ylab("Pmega") + theme(legend.title =element_blank())


### Lmean_Lopt

Lmean_Loptgg <- data.frame(years, medians_Lmean_Lopt,Lmean_LoptQ5, Lmean_LoptQ95,ref_Lmean_Lopt)

df_plot5 = data.frame(Year=rep(Lmean_Loptgg$years,4),
                      Lc=c(medians_Lmean_Lopt,Lmean_LoptQ5,Lmean_LoptQ95,ref_Lmean_Lopt),
                      lines=c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100)))

p5 <- ggplot(df_plot5,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0.78, max(df_plot$Lc)+0.2)) +
  xlab("Years")+ylab("Lmean/Lopt") + theme(legend.title =element_blank())


### Lmean_L_FeM 

Lmean_L_FeMgg <- data.frame(years, medians_Lmean_L_FeM,Lmean_L_FeMQ5, Lmean_L_FeMQ95,ref_Lmean_L_FeM )

df_plot6 = data.frame(Year=rep(Lmean_L_FeMgg$years,4),
                      Lc=c(medians_Lmean_L_FeM,Lmean_L_FeMQ5,Lmean_L_FeMQ95,ref_Lmean_L_FeM ),
                      lines = c(rep("median",100),
                                rep("quantile 5",100),
                                rep("quantile 95",100),
                                rep("reference",100)))

p6 <-ggplot(df_plot6,aes(x=Year, y = Lc, group=lines)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red"))+
  scale_y_continuous(limits = c(0.78, max(df_plot$Lc)+0.2)) +
  xlab("Years")+ylab("Lmean/LF=M") + theme(legend.title =element_blank())



#jpeg("plots/ind_LBI_scn1.jpeg", width = 2900, height = 2000, res = 300)
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3,
          common.legend = TRUE,
          legend = "bottom")
#dev.off()

### new references for scnarios 2,3 y 4

ref_fmsy_Lc_Lmat <- rep(0.94,100)
ref_fmsy_L25_Lmat <- rep(1.02,100)
ref_fmsy_Lmax5_Linf <- rep(0.9,100)
ref_fmsy_Pmega <- rep(0.15,100)
ref_fmsy_Lmean_Lopt <- rep(0.96,100)
ref_fmsy_Lmean_L_FeM <- rep(1.03,100)

### plots for scenarios 2,3 y 4

### Lc_Lmat

Lc_Lmatgg <- data.frame(years, medians_Lc_Lmat, Lc_LmatQ5, Lc_LmatQ95,ref_Lc_Lmat,ref_fmsy_Lc_Lmat)

df_plot=data.frame(Year=rep(Lc_Lmatgg$years,5),
                   Lc=c(medians_Lc_Lmat,Lc_LmatQ5,Lc_LmatQ95,ref_Lc_Lmat,ref_fmsy_Lc_Lmat),
                   lines=c(rep("median",100),
                           rep("quantile 5",100),
                           rep("quantile 95",100),
                           rep("reference",100),
                           rep("reference FMSY",100)))


p1 <-ggplot(df_plot,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("Lc/Lmat") + theme(legend.title =element_blank())



### L25_Lmat


L25_Lmatgg <- data.frame(years, medians_L25_Lmat, L25_LmatQ5, L25_LmatQ95,ref_L25_Lmat,ref_fmsy_L25_Lmat)

df_plot2=data.frame(Year=rep(L25_Lmatgg$years,5),
                    Lc=c(medians_L25_Lmat,L25_LmatQ5,L25_LmatQ95,ref_L25_Lmat,ref_fmsy_L25_Lmat),
                    lines = c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100),
                              rep("reference FMSY",100)))

p2 <- ggplot(df_plot2,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+ scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("L25/Lmat") + theme(legend.title =element_blank())

### Lmax5_Linf


Lmax5_Linfgg <- data.frame(years, medians_Lmax5_Linf, Lmax5_LinfQ5, Lmax5_LinfQ95,ref_Lmax5_Linf,ref_fmsy_Lmax5_Linf)
df_plot3 = data.frame(Year=rep(Lmax5_Linfgg$years,5),
                      Lc=c(medians_Lmax5_Linf,Lmax5_LinfQ5,Lmax5_LinfQ95,ref_Lmax5_Linf,ref_fmsy_Lmax5_Linf),
                      lines=c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100),
                              rep("reference FMSY",100)))

p3 <- ggplot(df_plot3,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("Lmax5/Linf") + theme(legend.title =element_blank())

### Pmega

Pmegagg <- data.frame(years, medians_Pmega, PmegaQ5, PmegaQ95,ref_Pmega,ref_fmsy_Pmega)

df_plot4 = data.frame(Year=rep(Pmegagg$years,5),
                      Lc=c(medians_Pmega,PmegaQ5,PmegaQ95,ref_Pmega,ref_fmsy_Pmega),
                      lines=c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100),
                              rep("reference FMSY",100)))
p4 <- ggplot(df_plot4,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("Pmega") + theme(legend.title =element_blank())

### Lmean_Lopt

Lmean_Loptgg <- data.frame(years, medians_Lmean_Lopt,Lmean_LoptQ5, Lmean_LoptQ95,ref_Lmean_Lopt,ref_fmsy_Lmean_Lopt)

df_plot5 = data.frame(Year=rep(Lmean_Loptgg$years,5),
                      Lc=c(medians_Lmean_Lopt,Lmean_LoptQ5,Lmean_LoptQ95,ref_Lmean_Lopt,ref_fmsy_Lmean_Lopt),
                      lines=c(rep("median",100),
                              rep("quantile 5",100),
                              rep("quantile 95",100),
                              rep("reference",100),
                              rep("reference FMSY",100)))

p5 <- ggplot(df_plot5,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("Lmean/Lopt") + theme(legend.title =element_blank())


### Lmean_L_FeM

Lmean_L_FeMgg <- data.frame(years, medians_Lmean_L_FeM,Lmean_L_FeMQ5, Lmean_L_FeMQ95,ref_Lmean_L_FeM,ref_fmsy_Lmean_L_FeM)

df_plot6 = data.frame(Year=rep(Lmean_L_FeMgg$years,5),
                      Lc=c(medians_Lmean_L_FeM,Lmean_L_FeMQ5,Lmean_L_FeMQ95,ref_Lmean_L_FeM,ref_fmsy_Lmean_L_FeM),
                      lines = c(rep("median",100),
                                rep("quantile 5",100),
                                rep("quantile 95",100),
                                rep("reference",100),
                                rep("reference FMSY",100)))

p6 <-ggplot(df_plot6,aes(x=Year, y = Lc)) +
  geom_line(aes(linetype=lines,color=lines))+
  scale_color_manual(values = c(rep("black",3),"red","blue"))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed"))+
  xlab("Years")+ylab("Lmean/LF=M") + theme(legend.title =element_blank())


#jpeg("plots/ind_LBI_scn4.jpeg", width = 2900, height = 2000, res = 300)
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3,
          common.legend = TRUE,
          legend = "bottom")
#dev.off()




