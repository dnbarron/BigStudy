clique.number(g1)  # largest clique has 8 members

cls <- closeness(g1, mode='in')

is.connected(g1)  # Graph is connected
clusters(g1,mode="strong")
no.clusters(g1) # 1 cluster!

cbs <- cohesive.blocks(g1)
x11(width=10,height=6)
plot.bgraph(cbs)

ebc <- edge.betweenness.community(g1)
community.to.membership(g1,ebc$merges, steps=70)

power.law.fit(deg)  # 1.44

recpr <- reciprocity(g1)  # 0.069  Very low, but prob because 

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
brdg.colors <- rep('grey',199)
brdg.colors[eb.adv$removed.edges[1:5]] <- 'red'
brdg.colors[eb.adv$removed.edges[1:5]] <- 'red'
brdg.wd <- rep(1,199)
brdg.wd[eb.adv$removed.edges[1:5]] <- 5

tkplot(advice.noiso, layout=layout.kamada.kawai,edge.arrow.size=1,vertex.size=5,
       edge.color=brdg.colors,
       edge.width=brdg.wd)

##### Network of people interviewed only

V.notint <- V(knows)[V(knows)$Interviewed==1]
net.interview <- delete.vertices(knows,10:(vcount(knows)-1))
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


know.dy <- dyad.census(knows.int)
adv.dy <- dyad.census(advice.int)
lead.dy <- dyad.census(leader.int)
inf.dy <- dyad.census(infl.int)
dy.op <- matrix(cbind(know.dy$mut,know.dy$asym,know.dy$null,ecount(knows.int),
                      adv.dy$mut,adv.dy$asym,adv.dy$null,ecount(advice.int),
                      lead.dy$mut,lead.dy$asym,lead.dy$null,ecount(leader.int),
                      inf.dy$mut,inf.dy$asym,inf.dy$null,ecount(infl.int)),ncol=4,byrow=TRUE)
row.names(dy.op) <- c('Knows','Advice','Leader','Influence')
colnames(dy.op) <- c('Mutual','Asymmetric','Null','Total')
dy.op