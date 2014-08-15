# heatmap for Heiko

main.data<- read.csv("Data for Heatmapcluster.csv",header = TRUE,sep = ";")

prots <- unique(main.data[["Accession"]])
#tumors <- unique(main.data[["Tumor"]])
#to.remove <- c("OvCa 10", "OvCa 16", "OvCa 23", "OvCa 69")
CD3.high <- c("OvCa 13","OvCa 28","OvCa 45","OvCa 53","OvCa 64","OvCa 65","OvCa 66","OvCa 68","OvCa 70","OvCa 74","OvCa 81")
CD3.low <- c("OvCa 9","OvCa 12","OvCa 15","OvCa 39","OvCa 41","OvCa 43","OvCa 48","OvCa 54","OvCa 57","OvCa 58","OvCa 59","OvCa 60","OvCa 72","OvCa 73","OvCa 79","OvCa 80")
tumors <- c(CD3.high,CD3.low)


bin.df <- data.frame(matrix(nrow=length(prots), ncol=length(tumors), rep(0, length(prots)*length(tumors))))
rownames(bin.df) <- prots
colnames(bin.df) <- tumors
#bin.df <- bin.df[,!(names(bin.df) %in% to.remove)]

for(row in row.names(bin.df)){
  pos <- which(row == main.data[["Accession"]])
  for(p in pos){
    if(main.data[["Tumor"]][p] %in% names(bin.df)){
      bin.df[row,as.character(main.data[["Tumor"]][p])] =1
    }
  }
}
# remove all 1 
bin.df.cut <- bin.df[!apply(bin.df,1,sum)==1,]










bin.matrix <- as.matrix(bin.df)
library(manipulate)
heatmap(x = bin.matrix[1:10,], Rowv = NA )

library(gplots)
heatmap.2(x=bin.matrix)

library(iplots)
ihist(var = melt(bin.df))

library(ggplot2)
library(reshape2)
qplot(x=colnames(bin.df),y=row.names(bin.df),data = melt(bin.df), geom="tile")



(p <- ggplot(data = melt(bin.df)) + geom_tile(aes(fill = rescale),
colour = "white") + scale_fill_gradient(low = "white", high = "black"))

