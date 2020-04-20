library(igraph); 
g1 = read.graph("C:\\Users\\cobysoss\\Desktop\\networks\\celegansneural.gml", "gml")
area = vcount(g1)^2
between = betweenness(g1)
nodes <- 1:297

df = data.frame(nodes, between)
df <- df[order(df$between),]
numBlue = (nrow(df)/3) 
j = 0
for(i in 1:nrow(df)) 
{
   node = data.frame(df[i, 1])
   if(j < numBlue)
   {
      V(g1)[unlist(node)]$color <- "blue"
   }
   else
   {
      V(g1)[unlist(node)]$color <- "red"
   }
   j <- j+1
}

summary(g1)
print(g1,
      graph.attributes = igraph_opt("print.graph.attributes"),
      vertex.attributes = igraph_opt("print.vertex.attributes"),
      edge.attributes = igraph_opt("print.edge.attributes"), names = TRUE,
      max.lines = igraph_opt("auto.print.lines"))

plot( g1, layout = layout.lgl(g1, area, repulserad = area * vcount(g1)) * 1.5,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 3,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)


