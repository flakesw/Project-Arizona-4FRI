library(raster) 
 
##### no harvest  ###############   
tec.nh <- vector() 
tec.df.nh.IPSL  <- data.frame(seq(1,90,1))
for (j in 1:10){ 
for (i in 1:90) {
tec.raster <- raster(paste("E:/Landis/NoHarvestIPSLCM5ALR/replicate", j, "/century/total_C-",i,".img", sep=""))
tec.df <- as.data.frame(tec.raster)
tec.df.pos <- tec.df[tec.df[,1]>0,]
tec.yr.avg <- mean(tec.df.pos)
tec.nh <- append(tec.nh, tec.yr.avg)
}
tec.df.nh.IPSL  <- data.frame(tec.df.nh.IPSL, tec.nh)
tec.nh <- vector()
}


###average replicates
tec.allreps.nh.IPSL  <- as.vector(apply(tec.df.nh.IPSL[,2:6], 1, FUN="mean"))
tec.allreps.nh.ci.IPSL  <- as.vector(apply(tec.df.nh.IPSL[,2:6], 1, function(x){2.132*sd(x)/sqrt(length(x))}))
tec.allreps.nh.df.IPSL <- data.frame(tec.df.nh.IPSL[,1], tec.allreps.nh.IPSL, tec.allreps.nh.ci.IPSL)
names(tec.allreps.nh.df.IPSL) <- c("Year", "avg.nh", "ci.nh")


##### status quo  ###############   
tec.sq <- vector() 
tec.df.sq.IPSL  <- data.frame(seq(1,90,1))
for (j in 1:10){ 
for (i in 1:90) {
tec.raster <- raster(paste("E:/Landis/Harvest_statusquoIPSLCM5ALR/replicate", j, "/century/total_C-",i,".img", sep=""))
tec.df <- as.data.frame(tec.raster)
tec.df.pos <- tec.df[tec.df[,1]>0,]
tec.yr.avg <- mean(tec.df.pos)
tec.sq <- append(tec.sq, tec.yr.avg)
}
tec.df.sq.IPSL  <- data.frame(tec.df.sq.IPSL, tec.sq)
tec.sq <- vector()
}

###average replicates
tec.allreps.sq.IPSL  <- as.vector(apply(tec.df.sq.IPSL[,2:6], 1, FUN="mean"))
tec.allreps.sq.ci.IPSL <- as.vector(apply(tec.df.sq.IPSL[,2:6], 1, function(x){1.833*sd(x)/sqrt(length(x))}))
tec.allreps.sq.df.IPSL <- data.frame(tec.df.sq.IPSL[,1], tec.allreps.sq.IPSL , tec.allreps.sq.ci.IPSL )
names(tec.allreps.sq.df.IPSL) <- c("Year", "avg.sq", "ci.sq")


##### moderate  ###############   
tec.mod <- vector() 
tec.df.mod.IPSL  <- data.frame(seq(1,90,1))
for (j in 1:10){ 
for (i in 1:90) {
tec.raster <- raster(paste("E:/Landis/Harvest_moderateIPSLCM5ALR/replicate", j, "/century/total_C-",i,".img", sep=""))
tec.df <- as.data.frame(tec.raster)
tec.df.pos <- tec.df[tec.df[,1]>0,]
tec.yr.avg <- mean(tec.df.pos)
tec.mod <- append(tec.mod, tec.yr.avg)
}
tec.df.mod.IPSL  <- data.frame(tec.df.mod.IPSL, tec.mod)
tec.mod <- vector()
}

###average replicates
tec.allreps.mod.IPSL  <- as.vector(apply(tec.df.mod.IPSL[,2:6], 1, FUN="mean"))
tec.allreps.mod.ci.IPSL  <- as.vector(apply(tec.df.mod.IPSL[,2:6], 1, function(x){1.833*sd(x)/sqrt(length(x))}))
tec.allreps.mod.df.IPSL <- data.frame(tec.df.mod.IPSL[,1], tec.allreps.mod.IPSL , tec.allreps.mod.ci.IPSL )
names(tec.allreps.mod.df.IPSL) <- c("Year", "avg.mod", "ci.mod")


##### fast  ###############   
tec.fast <- vector() 
tec.df.fast.IPSL  <- data.frame(seq(1,90,1))
for (j in 1:10){ 
for (i in 1:90) {
tec.raster <- raster(paste("E:/Landis/Harvest_fastIPSLCM5ALR/replicate", j, "/century/total_C-",i,".img", sep=""))
tec.df <- as.data.frame(tec.raster)
tec.df.pos <- tec.df[tec.df[,1]>0,]
tec.yr.avg <- mean(tec.df.pos)
tec.fast <- append(tec.fast, tec.yr.avg)
}
tec.df.fast.IPSL  <- data.frame(tec.df.fast.IPSL, tec.fast)
tec.fast <- vector()
}


###average replicates
tec.allreps.fast.IPSL  <- as.vector(apply(tec.df.fast.IPSL[,2:6], 1, FUN="mean"))
tec.allreps.fast.ci.IPSL  <- as.vector(apply(tec.df.fast.IPSL[,2:6], 1, function(x){1.833*sd(x)/sqrt(length(x))}))
tec.allreps.fast.df.IPSL <- data.frame(tec.df.fast.IPSL[,1], tec.allreps.fast.IPSL , tec.allreps.fast.ci.IPSL )
names(tec.allreps.fast.df.IPSL) <- c("Year", "avg.fast", "ci.fast")

############## plot  ###################
rgb(0,0,255, max=255,alpha=75, names = "blue30" )  ##  "#0000FF4B"  
rgb(255,0,0, max=255,alpha=75, names = "red30" )  ##  "#FF00004B"
rgb(0,100,0, max=255,alpha=75, names = "green30" )  ##  "#0064004B"  
rgb(0,0,0, max=255,alpha=75, names = "black30" )  ##  "#0000004B" 


#windows(pointsize=18)
#par(mgp=c(2.5,1,0), mfrow = c(2,2))

plot(tec.allreps.nh.df.IPSL$Year, tec.allreps.nh.df.IPSL$avg.nh, type="l", lwd=2, ylim= c(18000,23200), xlab = "Year", ylab = bquote("Total Ecosystem Carbon (g C/m"^"  2"*")"), axes = FALSE)
axis(1, at=c(-5,95,seq(0,90,10)), labels = NA)
axis(1, at=seq(0,90,10), labels=seq(2010,2100,10))   
axis(2, at=c(16000,25000, seq(18000,23000,1000)))
polygon(c(tec.allreps.nh.df.IPSL$Year, rev(tec.allreps.nh.df.IPSL$Year)), c(tec.allreps.nh.df.IPSL$avg.nh-tec.allreps.nh.df.IPSL$ci.nh, rev(tec.allreps.nh.df.IPSL$avg.nh+tec.allreps.nh.df.IPSL$ci.nh)),col = "#0064004B", border = NA)
lines(tec.allreps.nh.df.IPSL$Year, tec.allreps.nh.df.IPSL$avg.nh, lwd=2, col="darkgreen")

polygon(c(tec.allreps.sq.df.IPSL$Year, rev(tec.allreps.sq.df.IPSL$Year)), c(tec.allreps.sq.df.IPSL$avg.sq-tec.allreps.sq.df.IPSL$ci.sq, rev(tec.allreps.sq.df.IPSL$avg.sq+tec.allreps.sq.df.IPSL$ci.sq)),col = "#0000004B", border = NA)
lines(tec.allreps.sq.df.IPSL$Year, tec.allreps.sq.df.IPSL$avg.sq, lty=1, lwd=2)

polygon(c(tec.allreps.mod.df.IPSL$Year, rev(tec.allreps.mod.df.IPSL$Year)), c(tec.allreps.mod.df.IPSL$avg.mod-tec.allreps.mod.df.IPSL$ci.mod, rev(tec.allreps.mod.df.IPSL$avg.mod+tec.allreps.mod.df.IPSL$ci.mod)),col = "#FF00004B", border = NA)     
lines(tec.allreps.mod.df.IPSL$Year, tec.allreps.mod.df.IPSL$avg.mod, lwd=2, col="red")

polygon(c(tec.allreps.fast.df.IPSL$Year, rev(tec.allreps.fast.df.IPSL$Year)), c(tec.allreps.fast.df.IPSL$avg.fast-tec.allreps.fast.df.IPSL$ci.fast, rev(tec.allreps.fast.df.IPSL$avg.fast+tec.allreps.fast.df.IPSL$ci.fast)),col = "#0000FF4B", border = NA)
lines(tec.allreps.fast.df.IPSL$Year, tec.allreps.fast.df.IPSL$avg.fast, lwd=2, col="blue")

#legend("topleft", legend =c("No Harvest","Status Quo", "Moderate 4FRI", "Fast 4FRI"), col = c("darkgreen","black","red", "blue"), lty=1, lwd=4, bty="n")

text(2.7, 23040, "Hot/Drier", cex=1.5, adj = c(0,0))
text(1.7, 22700, "-114 mm annual precip", cex=1.15, adj = c(0,0))
text(1, 22400,"+8° C summer max temp", cex=1.15, adj = c(0,0))
text(1, 18000, "c", cex=1.1)
#### export data to csv

all.data.tec <- data.frame(tec.allreps.nh.df.IPSL$avg.nh, tec.allreps.sq.df.IPSL$avg.sq, tec.allreps.mod.df.IPSL$avg.mod, tec.allreps.fast.df.IPSL$avg.fast)

write.csv(all.data.tec, "E:/Landis/Results/tec_maps_IPSL.csv")













