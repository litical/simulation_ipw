setwd("C:/Users/naijt/Documents/R code/From old pc")
h.strata <- read.csv("High_strata.csv");h.strata <- h.strata[,-1:-25]
l.strata <- read.csv("L.csv");l.strata <- l.strata[,-1:-25]
trata <- l.strata[,-1:-25];l.strata <- l.strata[-101:-1000,];

###function to create bias col
bias.gen <- function(o.data)
{
  ##getting cols
  #actual b1 cols
  b1.cols <- which(grepl("b",colnames(o.data)) & !grepl("mod",colnames(o.data))) 
  b1.cols <- colnames(o.data)[b1.cols]
  #cols for models
  mod.cols <- which(grepl("mod",colnames(o.data))) 
  mod.cols <- colnames(o.data)[mod.cols]
  ##iterating for every b1
  for(bval in 1:length(b1.cols))
  {
    #making var for current b1 var
    cur.b <- b1.cols[bval]
    #var of modb1 for current b1
    mod.b <- which(grepl(cur.b,mod.cols));mod.b <- mod.cols[mod.b]
    #creating new column for each mode ba
    for(cval in 1:length(mod.b))
    {
      bias.name <- paste0("mod",cval,cur.b,"_bias")
      diff <- (o.data[,cur.b] - o.data[,mod.b[cval]])
      o.data[,bias.name] <- ((diff*diff)^(1/2))/o.data[,cur.b]
    }
  }
  return (o.data)
}
h.strata <- bias.gen(h.strata)
l.strata <- bias.gen(l.strata)

#matrix for low strata
l.mat <- matrix(nrow = 4, ncol = 5)
colnames(l.mat) <-c("Mod1","Mod2","Mod3","Mod4","Mod5")
rownames(l.mat) <-c("b1","b2","b3","b4")
#putting values in matrix
for(mod in 1:5)
{
  bias.col <- which(grepl("bias",colnames(l.strata)) & grepl(paste0("mod",mod),colnames(l.strata)))
  bias.col <- colnames(l.strata)[bias.col]
  for (c.row in 1:4) 
  {
    l.mat[c.row,mod] <-var(l.strata[,bias.col[c.row]])
  }
}
#matrix for High strata
h.mat <- matrix(nrow = 4, ncol = 5)
colnames(h.mat) <-c("Mod1","Mod2","Mod3","Mod4","Mod5")
rownames(h.mat) <-c("b1","b2","b3","b4")
#putting values in matrix
for(mod in 1:5)
{
  bias.col <- which(grepl("bias",colnames(h.strata)) & grepl(paste0("mod",mod),colnames(h.strata)))
  bias.col <- colnames(h.strata)[bias.col]
  for (c.row in 1:4) 
  {
    h.mat[c.row,mod] <- var(h.strata[,bias.col[c.row]])
  }
}
mean(l.mat[1:3,4])
mean(l.mat[4,4])
