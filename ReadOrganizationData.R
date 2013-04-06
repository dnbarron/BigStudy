n.org <- length(org.fns)
org.nets <- list()     
org.names <- data.frame(ID=1:52)
for (i in 1:n.org){
  org.nets[[i]] <- read.csv(org.fns[i],skip=6,header=FALSE,nrows=52)[,1:4]
  ego.org <- read.csv(org.fns[i],skip=1,header=FALSE,nrows=1,stringsAsFactors=FALSE)[,1]
  org.nets[[i]]$ego <- ego.org
  colnames(org.nets[[i]]) <- c('Alter','Patients','Staff','Other','Ego')
  org.names <- cbind(org.names,org.nets[[i]][,1])
}

#for (i in 3:(n.org+1)){
#  print(i)
#  print(all.equal(org.names[,2],org.names[,i]))
#}

## Test that ego and alter organizational names are the same
ego.orgs <- vector()
for (i in 1:n.org){
  ego.orgs <- c(ego.orgs,read.csv(org.fns[i],skip=1,header=FALSE,nrows=1,stringsAsFactors=FALSE)[,1] )
}
all(ego.orgs %in% org.names[,2])

### create data frame with all organizational network data
orgs <- data.frame()
for (i in 1:n.org){
  orgs <- rbind(orgs,org.nets[[i]])
}

# Remove data when alter == ego, other NAs should be 0s

ix <- orgs$Ego == orgs$Alter
orgs <- orgs[!ix,]
orgs[is.na(orgs$Patients),2] <- 0
orgs[is.na(orgs$Staff),3] <- 0
orgs[is.na(orgs$Other),4] <- 0

nms <- orgs$Ego
nms <- str_replace(nms,fixed("?"),"'")
orgs$Ego <- nms
nmsA <- orgs$Alter
nmsA <- str_replace(nmsA,fixed("?"),"'")
orgs$Alter <- nmsA
orgs <- orgs[,c('Ego','Alter','Patients','Staff','Other')]


orgs.nonzero <- subset(orgs, (Patients > 0 | Staff > 0 | Other > 0))


g.org <- graph.data.frame(orgs.nonzero)
summary(g.org)

Patients <- delete.edges(g.org, E(g.org)[get.edge.attribute(g.org,name = "Patients")==0])
summary(Patients)

x11(width=12,height=8)
plot(Patients, layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.size=5)
#dev.off()
