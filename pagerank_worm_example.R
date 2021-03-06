library(igraph); 
g1 = read.graph("C:\\Users\\cobysoss\\Desktop\\networks\\celegansneural.gml", "gml")
page_ranks = page.rank(g1)
nodes <- 1:297
df = data.frame(nodes, page_ranks$vector)
df <- df[order(df$page_ranks.vector,decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
   pageRank <- data.frame(df[i,2])
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


