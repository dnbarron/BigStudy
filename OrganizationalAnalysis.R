# Reduce to non-zero edges and build a graph object ------

el.org.nonzero.edges<- subset(el.org, (Knows|advice|leadership > 0))

g.org <- graph.data.frame(el.org)
summary(g.org)

g.org.nonzero <- graph.data.frame(el.org.nonzero.edges)

#attribs[,1] <- attribs[,1]-1
#names(attribs)[1] <- "Vertex"
# Set vertex attributes

ego <- V(g.org)$name

for (i in V(g.org)) {
    ix <- match(ego[i],atts[,3])
    if (is.na(ix)) stop(cat(i," Invalid name"))
    g.org <- set.vertex.attribute(g.org, 'Interviewed', index=i, atts[ix,2])
}

summary(g.org)
g.org.simp <- simplify(g.org,edge.attr.comb='sum')
summary(g.org.simp)
          

int_vertex_colors <- get.vertex.attribute(g.org,"Interviewed")
colors <- c('Lightblue', 'Red')
int_vertex_colors[int_vertex_colors == 'No'] <- colors[1]
int_vertex_colors[int_vertex_colors == 'Yes'] <- colors[2]

is.loop(g.org)
has.multiple(g.org)

knows.org <- delete.edges(g.org, E(g.org)[Knows==0])
knows.org.simp <- simplify(knows.org,edge.attr.comb='sum')

summary(knows.org.simp)
deg.knows.org <- degree(knows.org.simp,mode='in')

knows.org.noiso <- delete.vertices(knows.org.simp,V(knows.org.simp)[degree(knows.org.simp,mode='in')==0])

mypal <- brewer.pal(9,"Reds")
mypal <- c(mypal[1],mypal)
my.edge.colours <- E(knows.org.noiso)$Knows

for (i in 1:length(my.edge.colours)){
  tmp <- as.numeric(my.edge.colours[i])
  my.edge.colours[i] <- mypal[tmp]
}

x11(width=12,height=8)
plot(knows.org.noiso, layout=layout.kamada.kawai,edge.arrow.size=.4,vertex.size=5,vertex.label=NA,edge.color=my.edge.colours)
dev.off()


advice <- delete.edges(g.org, E(g.org)[get.edge.attribute(g.org,name = "advice")==0])
advice.simp <- simplify(advice, edge.attr.comb='sum')
summary(advice.simp)
adv.simp.adj <- get.adjacency(advice.simp,sparse=FALSE)
graph.density(advice.simp)

#hierarchy(adv.simp.adj)
#hierarchy(adv.simp.adj,measure='k')

advice.noiso <- delete.vertices(advice.simp,V(advice.simp)[degree(advice.simp)==0])
deg.adv <- degree(advice.simp,mode='in')

mypal <- brewer.pal(7,"Reds")
mypal <- c(mypal[1],mypal)
my.edge.colours.adv <- E(advice.noiso)$advice

for (i in 1:length(my.edge.colours.adv)){
  tmp <- as.numeric(my.edge.colours.adv[i])
  my.edge.colours.adv[i] <- mypal[tmp]
}

plot(degree.distribution(advice.simp))
#pdf('NetworkPlot1.pdf')
x11(width=12,height=8)
plot(advice.noiso, layout=layout.fruchterman.reingold.grid,edge.arrow.size=.3,
  #   vertex.label=abbreviate(V(advice.noiso)$name),
     vertex.label=NA,
     vertex.color=int_vertex_colors,vertex.size=5,
#     edge.color = my.edge.colours.adv,
     edge.width = E(advice.noiso)$advice/2
     )
dev.off()

library(intergraph)
intergraph:::as.network.igraph(advice.simp,directed=TRUE)
