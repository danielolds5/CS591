library(igraph); 
g1 = read.graph("C:\\Users\\cobysoss\\Desktop\\networks\\celegansneural.gml", "gml")
between = betweenness(g1)
nodes <- 1:297

df = data.frame(nodes, between)
df <- df[order(df$between, decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
   node = data.frame(df[i, 1])
   if(j < 18)
   {
      V(g1)[unlist(node)]$color <- "red"
   }
   else
   {
      V(g1)[unlist(node)]$color <- "blue"
   }
   j <- j+1
}

plot( g1, layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)


