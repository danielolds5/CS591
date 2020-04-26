library(igraph)
library(ggraph)
library(tidyverse)

NN = read.graph("celegansneural.gml", format = c("gml"))

nodes <- 1: 297
degree_NN <- degree(NN)
df = data.frame(nodes, degree_NN)
df <- df[order(df$degree_NN, decreasing = TRUE),]

Z <- degree_NN  
Z = 1/2*sum(degree_NN)

Z_best = 297*297
K_best = 0 

for(i in 1:nrow(df)) 
{   
  print(unlist(df[i,2]))
  node = data.frame(df[i, 1])                                                                                                         
  Z = Z + i - 1 - unlist(df[i,2]) 
  if(Z < Z_best)
  { 
    Z_best = Z
    k_best = i
  }
}

for(i in 1:nrow(df)) 
{
  node = data.frame(df[i, 1])

  if(i <= k_best)
  {
    V(NN)[unlist(node)]$color <- "red"
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
}

plot( NN, layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 3,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)




############################################
Z <- function(graph1)
  
  

% 

graph1 = read.graph("celegansneural.gml", format = c("gml"))

degree_list <- list(degree(graph1, v = V(graph1), mode = "all"))

degree_array <- array(as.numeric(unlist(degree_list)))


% S is all the nodes in the graph 1-n 

% Sort Degree in Descending order
degree_sort <- sort(degree_array, decreasing = TRUE)

degree_sort

sum(degree_sort)

n <- length(degree_sort)
z <- 1:n
X <- degree_sort  
for(k in z){
  for(i in 1:k){
    X = (1/2)*sum(i)
  }
}


Z <- graph_degree  
    Z = 1/2*sum(graph_degree)

Z_best = 297*297
K_best = 0 

for(i in 1:nrow(df)) 
  {   
    print(unlist(df[i,2]))
    node = data.frame(df[i, 1])                                                                                                         
    Z = Z + i - 1 - unlist(df[i,2]) 
    if(Z < Z_best)
    { 
      Z_best = Z
      k_best = i
    }
  } 


g1 = read.graph("C:\\Users\\cobysoss\\Desktop\\networks\\celegansneural.gml", "gml")
area = vcount(g1)^2
page_ranks = page.rank(g1)
nodes <- 1:297
df = data.frame(nodes, page_ranks$vector)
df <- df[order(df$page_ranks.vector),]
numBlue = (nrow(df)/3) 
j = 0
for(i in 1:nrow(df)) 
{
  pageRank <- data.frame(df[i,2])
  node = data.frame(df[i, 1])
  if(i < numBlue)
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

plot( g1, layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 3,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)





for(i in 1:nrow(df)) 
{
  pageRank <- data.frame(df[i,2])
  node = data.frame(df[i, 1])
  if(i < numBlue)
  {
    V(g1)[unlist(node)]$color <- "blue"
  }
  else
  {
    V(g1)[unlist(node)]$color <- "red"
  }
  j <- j+1
}