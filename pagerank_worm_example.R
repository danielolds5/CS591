library(igraph); 
g1 = read.graph("C:\\Users\\cobysoss\\Desktop\\networks\\celegansneural.gml", "gml")
area = vcount(g1)^2
page_ranks = page.rank(g1)
i = 1
for(idx in page_ranks$vector)
{
   V(g1)[i]$color <- ifelse(idx < .001, "blue", "red")
   i <- i + 1
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


