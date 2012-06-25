library(igraph)
library(stringr)

fns <- dir(pattern='*.csv')
n <- length(fns)
nets <- list()
for (i in 1:n){
    nets[[i]] <- read.csv(fns[i],skip=5,header=FALSE,nrows=77)
}

     
el <- data.frame()
#i <- 1
for (i in 1:n){
  tmp <- nets[[i]][,c(1,2,3,6,7,8)]
  ego <- str_sub(fns[i],1,-5)
#   sp <- str_locate(ego,'[a-z][A-Z]')
#   ego <- paste(substr(ego,1,sp[1]) ,  substring(ego,sp[2]))
  sp2 <- str_locate(tmp[,1],' [A-Z]')
  alter <- paste(substr(tmp[,1],1,sp2[,1]-1),substring(tmp[,1],sp2[,2]),sep="")
#  ix <- tmp[,2] > 0
#  tmp <- tmp[ix,]

alter <- ifelse(alter=='RachaelWiliams','RachaelWilliams',alter)  
alter <- ifelse(alter=='SuEdwards','SueEdwards',alter)

  # Get rid of -1 s in advice variable
  tmp[,4] <- ifelse(tmp[,4]<=0, 0, 1)
  el <- rbind(el,data.frame(ego,alter,Knows=tmp[,2],Strength=tmp[,3],
                advice=tmp[,4],leadership=tmp[,5],influence=tmp[,6]))
}

# Reduce to non-zero edges and build a graph object
el.nonzero.edges <- subset(el, (Knows > 0 | advice > 0))


g1 <- graph.data.frame(el)
summary(g1)


alters <- V(g1)
el$alter %in% el$ego

attribs <- read.table('Attributes.txt',header=TRUE,stringsAsFactors=FALSE)
#attribs[,1] <- attribs[,1]-1
#names(attribs)[1] <- "Vertex"
# Set vertex attributes
for (i in V(g1)) {
  for (j in names(attribs)) {
    ix <- match(V(g1)$name[i],attribs[,1])
    if (is.na(ix)) stop("Invalid name")
    g1 <- set.vertex.attribute(g1, j, index=i, attribs[ix,j])
  }
}
summary(g1)

int_vertex_colors <- get.vertex.attribute(g1,"Interviewed")
colors <- c('Lightblue', 'Red')
int_vertex_colors[int_vertex_colors == 'No'] <- colors[1]
int_vertex_colors[int_vertex_colors == 'Yes'] <- colors[2]

knows <- delete.edges(g1, E(g1)[get.edge.attribute(g1,name = "Knows")==0])


summary(knows)
deg.know <- degree(knows,mode='in')

knows.noiso <- delete.vertices(knows,V(knows)[degree(knows)==0])

x11(width=12,height=8)
plot(knows.noiso, layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.size=5,
     vertex.label=NA,vertex.color=int_vertex_colors)
#dev.off()

advice <- delete.edges(g1, E(g1)[get.edge.attribute(g1,name = "advice")==0])
summary(advice)


advice.noiso <- delete.vertices(advice,V(advice)[degree(advice)==0])
deg.adv <- degree(advice,mode='in')
plot(degree.distribution(advice.noiso))
#pdf('NetworkPlot1.pdf')
x11(width=12,height=8)
plot(advice.noiso, layout=layout.fruchterman.reingold.grid,edge.arrow.size=.3,
     vertex.label=NA,vertex.color=int_vertex_colors,vertex.size=3)
dev.off()

tkplot(advice.noiso, layout=layout.fruchterman.reingold.grid,edge.arrow.size=.3,
       vertex.label=NA,vertex.color=int_vertex_colors,vertex.size=deg.adv*2)
plot.coords <- tkplot.getcoords(3)
tkplot.export.postscript(3)
# Write Pajek file
write.graph(g1,'all.net',format='pajek')

deg <- degree(g1, mode='in')

plot(degree.distribution(g1))
     
bet.adv <- betweenness(advice.noiso)

x11(width=10,height=6)
plot(g1,layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.label=NA,vertex.size=deg)
dev.off()


leader <- delete.edges(g1, E(g1)[get.edge.attribute(g1,name = "leadership")!=1])
deg.lead <- degree(leader,mode='in')
influence <- delete.edges(g1, E(g1)[get.edge.attribute(g1,name = "influence")!=1])


leader.noiso <- delete.vertices(leader,V(leader)[degree(leader)==0])
deg.lead.no <- degree(leader.noiso, mode='in')

x11(width=10,height=6)
tkplot(leader.noiso,layout=layout.kamada.kawai,edge.arrow.size=1,vertex.label=deg.lead.no,
     vertex.size=deg.lead.no,vertex.color=int_vertex_colors)
dev.off()

deg.inf <- degree(influence, mode='in')
infl.noiso <- delete.vertices(influence,V(influence)[degree(influence)==0])
deg.inf.no <- degree(infl.noiso, mode='in')

x11(width=10,height=6)
tkplot(infl.noiso,layout=layout.kamada.kawai,edge.arrow.size=1,vertex.label=deg.inf.no,
       vertex.size=deg.inf.no*2,vertex.color=int_vertex_colors)
dev.off()

library(xtable)
xtable(data.frame(alters$name,deg.know,deg.adv,deg.lead,deg.inf),digits=0)


xtable(data.frame(alters$name,deg.adv),digits=0)
#tk1 <- tkplot(g1,layout=layout.kamada.kawai,vertex.label=NA,vertex.size=5)

##### Focus on know network
### Lood at strength of ties
edge.colors <- c('gray','springgreen','lightskyblue','navy','red')
int_edge_colors <- get.edge.attribute(knows,"Strength")
int_edge_colors[int_edge_colors == 1] <- edge.colors[1]
int_edge_colors[int_edge_colors == 2] <- edge.colors[1]
int_edge_colors[int_edge_colors == 3] <- edge.colors[3]
int_edge_colors[int_edge_colors == 4] <- edge.colors[5]
int_edge_colors[int_edge_colors == 5] <- edge.colors[5]

# Weak ties grey
# Medium ties blue
# Strong ties red

x11(width=12,height=10)
plot(knows, layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.size=5,
     vertex.label=deg.know,vertex.color=int_vertex_colors,edge.color=int_edge_colors)
dev.off()

# Look at advice network

tkplot(advice.noiso, layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.size=5,
     vertex.label=deg.know,vertex.color=int_vertex_colors)

bet.adv <- betweenness(advice,directed=FALSE)
wtrp.adv <- walktrap.community(advice.noiso)
memb.adv <- community.to.membership(advice.noiso,wtrp.adv$merges,steps=12)
modularity(advice.noiso, memb.adv$membership)

vertex.colors <- c('springgreen','lightskyblue','navy','red','grey')

com.vertex.colors <- memb.adv$membership
com.vertex.colors[com.vertex.colors>3] <- vertex.colors[5]
com.vertex.colors[com.vertex.colors==0] <- vertex.colors[1]
com.vertex.colors[com.vertex.colors==1] <- vertex.colors[2]
com.vertex.colors[com.vertex.colors==2] <- vertex.colors[3]
com.vertex.colors[com.vertex.colors==3] <- vertex.colors[4]

brdg.colors <- rep('grey',199)
brdg.color[]

tkplot(advice.noiso, layout=layout.kamada.kawai,edge.arrow.size=.3,vertex.size=5,
       vertex.label=NA,vertex.color=com.vertex.colors)

ix <- order(memb.adv$membership)
com.df <- data.frame(Name=get.vertex.attribute(advice.noiso,"name"),Community=memb.adv$membership)[ix,]
com.df[1:16,]
rem <- as.numeric(rownames(com.df[17:62,])) -1
advice.com <- delete.vertices(advice.noiso,rem)
com.vertex.colors.sub <- c(1,3,2,1,0,2,2,3,2,3,2,2,2,0,0,1)
com.vertex.colors.sub[com.vertex.colors.sub==0] <- vertex.colors[1]
com.vertex.colors.sub[com.vertex.colors.sub==1] <- vertex.colors[2]
com.vertex.colors.sub[com.vertex.colors.sub==2] <- vertex.colors[3]
com.vertex.colors.sub[com.vertex.colors.sub==3] <- vertex.colors[4]

plot(advice.com, layout=layout.kamada.kawai,edge.arrow.size=1,vertex.size=5,
       vertex.label=NA,vertex.color=com.vertex.colors.sub)

## fastgreedy community structure
fg.adv <- fastgreedy.community(as.undirected(advice.noiso))
fg.adv <- community.to.membership(advice.noiso,fg.adv$merges,steps=12)
modularity(advice.noiso, fg.adv$membership)
fg.adv$membership
wtrp.adv$membership

### edge betweenness community
eb.adv <- edge.betweenness.community(advice.noiso,directed=FALSE)
eb.adv.mem <- community.to.membership(advice.noiso, eb.adv$merges, steps=9)
modularity(advice.noiso, eb.adv.mem$membership)
table(eb.adv.mem$membership)
eb.adv$bridges
eb.adv$removed.edges    
eb.adv$edge.betweenness
brdg.colors <- rep('grey',length(eb.adv$removed.edges))
brdg.colors[eb.adv$removed.edges[1:5]] <- 'red'
brdg.colors[eb.adv$removed.edges[1:5]] <- 'red'
brdg.wd <- rep(1,length(eb.adv$removed.edges))
brdg.wd[eb.adv$removed.edges[1:5]] <- 5

tkplot(advice.noiso, layout=layout.kamada.kawai,edge.arrow.size=1,vertex.size=5,
     edge.color=brdg.colors,
     edge.width=brdg.wd)

##### Network of people interviewed only

V.notint <- V(knows)[V(knows)$Interviewed=='Yes']
net.interview <- delete.vertices(knows,V(knows)[V(knows)$Interviewed=='No'])
summary(net.interview)
dens <- function(g){
  n.edge <- ecount(g)
  n.vert <- vcount(g)
  e.max <- n.vert^2 - n.vert
  if (is.directed(g)){
    n.edge/e.max
  }
  else {
    n.edge/(e.max/2)
  }
}

dens(net.interview)