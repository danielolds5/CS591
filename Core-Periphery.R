library(igraph)

par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 1
NN = read.graph("celegansneural.gml", format = c("gml"))
nodes <- 1: 297
degree_NN <- degree(NN)
length_degree = length(degree_NN)
for(j in 1:length_degree)
{
  degree_NN[j] = .5 * degree_NN[j]  
}

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
core_nodes = c()
for(i in 1:nrow(df)) 
{
  node = data.frame(df[i, 1])

  if(i <= k_best)
  {
    new_element = unlist(node)
    core_nodes = c(core_nodes, new_element)
    V(NN)[unlist(node)]$color <- "red"
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
}

plot( NN,layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)

###############begin page rank comparison####################
core_and_pr_intersect = c()
page_ranks = page.rank(NN)
df = data.frame(nodes, page_ranks$vector)
df <- df[order(df$page_ranks.vector,decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
  pageRank <- data.frame(df[i,2])
  node = data.frame(df[i, 1])
  if(j < 18)
  {
    if(node %in% core_nodes)
    {
      V(NN)[unlist(node)]$color <- "red"
      core_and_pr_intersect = c(core_and_pr_intersect, unlist(node))
    }
    else
    {
      V(NN)[unlist(node)]$color <- "yellow"
    }
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
  j <- j+1
}

plot( NN, layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)


############Closeness comparison#################
closenessCen = closeness(NN)
nodes <- 1:297

df = data.frame(nodes, closenessCen)
df <- df[order(df$closenessCen, decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
  node = data.frame(df[i, 1])
  if(j < 18)
  {
    if(node %in% core_nodes)
    {
      V(NN)[unlist(node)]$color <- "red"  
    }
    else
    {
      V(NN)[unlist(node)]$color <- "yellow"
    }
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
  j <- j+1
}

plot( NN, layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)

####################Betweeness centrality##################################
between = betweenness(NN)
nodes <- 1:297
core_and_pr_and_betweenness = c()

df = data.frame(nodes, between)
df <- df[order(df$between, decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
  node = data.frame(df[i, 1])
  if(j < 18)
  {
    if(node %in% core_nodes)
    {
      if(node %in% core_and_pr_intersect)
      {
        core_and_pr_and_betweenness = c(core_and_pr_and_betweenness, unlist(node))
      }
      V(NN)[unlist(node)]$color <- "red"  
    }
    else
    {
      V(NN)[unlist(node)]$color <- "yellow"
    }
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
  j <- j+1
}

plot( NN, 
      layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)

####################Eccentricity centrality##################################
ecc = eccentricity(NN)
nodes <- 1:297

df = data.frame(nodes, ecc)
df <- df[order(df$ecc, decreasing = TRUE),]
j = 0
for(i in 1:nrow(df)) 
{
  node = data.frame(df[i, 1])
  if(j < 18)
  {
    if(node %in% core_nodes)
    {
      V(NN)[unlist(node)]$color <- "red"  
    }
    else
    {
      V(NN)[unlist(node)]$color <- "yellow"
    }
  }
  else
  {
    V(NN)[unlist(node)]$color <- "blue"
  }
  j <- j+1
}

plot( NN, 
      layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)


##########Hyper central nodes####################################

nodes = 1:297
for(i in nodes)
{
  if(i %in% core_and_pr_and_betweenness)
  {
    V(NN)[i]$color <- "red"
  }
  else
  {
    V(NN)[i]$color <- "blue"
  }
}

plot( NN, 
      layout = layout_with_kk,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 6,
      edge.arrow.size = 0.01,
      vertex.size2 = 3,
      vertex.label = NA,
      asp = 1,
      margin = 0)