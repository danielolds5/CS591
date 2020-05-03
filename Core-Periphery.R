library(igraph)
library(qs)

# Read in Neural Network Data
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

matrix = as_adjacency_matrix(NN)
maxDegreeG = max(degree(NN))
degreeOfAllG = degree(NN)
P <- function(i, U, G){  
  #matrix = as_adjacency_matrix(G)
  for(j in U) 
    P = sum(matrix[i,j]) + degreeOfAllG[i] / maxDegreeG
  return(P)
}

getNeighbors <- function(U){ 
  totalNeighbors = c()
  for(u in U)
  {
    u = unlist(u)
    Neighbors = neighbors(NN, V(NN)[u], mode = "all")
    totalNeighbors = union(totalNeighbors, Neighbors)
  }
  return(totalNeighbors)
}

fakeReRank <- function(G){
  return(qs::qread(file = "rank.ser"))
}

reRank <- function(G) {
  
  degree_G <- degree(G)
  df = data.frame(nodes, degree_G)
  df <- df[order(df$degree_G, decreasing = TRUE),]
  
  U <- c() 
  V <- df$nodes
  V1 <- V    
  
  for(i in 1:nrow(df)) 
  {   
    u = data.frame(df[i, 1])
    V1 <- V1[-1]
    break
  }
  U <- c(u)
  while (length(V1) != 0)
  {
    scoreVector = array(data = 0, dim = 297, dimname = NULL)
    neibors = getNeighbors(U)
    for(i in neibors) 
    {
      if(i %in% V1)
      {
        scoreVector[i] <- P(i, U, G)  
      }
      
    }
    nextu = which(scoreVector == max(scoreVector))
    l = length(nextu)
    if(length(nextu) > 1)
    {
      nextu = nextu[1]
    }
    U = union(U, nextu)
    V1 <- V1[-which(V1 == nextu)]
    len = length(V1)
    print(len)
  }
  return(U)
} 

RDFake <- function(G, U, i, a)
{
  print(i)
  RDArray = qread("RD.ser")
  return(RDArray[i])
}

RD <- function(G, U, i, a)
{
  #print(i)
  if(i <= a)
  {
    m = 0
    for(j in 1:i)
    {
      edges = E(G)[from(U[j])]
      if(length(edges) > 0)
      {
        for(k in 1:length(E(G)[from(U[j])]))
        {
          node_endpoints = ends(G, edges[k])
          endpoint_not_uj = node_endpoints[2]
          if(endpoint_not_uj %in% U[1:i])
          {
            m = m + 1;
          }
        }  
      }
    }
    totalLinks = i * (i - 1)
    if(totalLinks == 0)
    {
      return(0)
    }
    return(m/totalLinks)
  }
  else
  {
    m = 0
    for(j in (i-a+1):i)
    {
      edges = E(G)[from(U[j])]
      if(length(edges) > 0)
      {
        for(k in 1:length(E(G)[from(U[j])]))
        {
          node_endpoints = ends(G, edges[k])
          endpoint_not_uj = node_endpoints[2]
          if(endpoint_not_uj %in% U[(i-a+1):i])
          {
            m = m + 1;
          }
        }  
      }
    }
    n = length((i-a+1):i)
    totalLinks = n *(n - 1)
    if(totalLinks == 0)
    {
      return(0)
    }  
    return(m/totalLinks)
  }
}

AvgDegreeFloor <- function(G) {
  return(floor(mean(degree(G))))  
}

USubsetToList <- function(USubset)
{
  list = c()
  for(e in USubset)
  {
    list = union(e, list)
  }
  return(list)
}

FindCoreSet <- function(RDScores, U, a, B) 
{
  NumC = c(1)
  CoreSet = vector("list", 1000)
  
  for(i in a:length(U))
  {
    if(RDScores[i] >= B)
    {
      CoreSet[[NumC + 1]] = union(CoreSet[[NumC + 1]], USubsetToList(U[(i-a+1):i]))
      #Variable = U[(i-a+1):i]
      print(CoreSet[2])
    }
    else if((RDScores[i-1] >= B) & (i > a))
    {
      NumC = NumC + 1
    }
  }
  if(RDScores[i] < B)
  {
    NumC = NumC - 1
  }
  
  CoreSet[1] = NumC
  return(CoreSet)
}

GeneralFrame <- function(G, B){
  U = fakeReRank(G)
  a = AvgDegreeFloor(G)
  RDScores = array(data = 0, dim = length(U), dimname = NULL)
  for(i in 1:length(U))
  {
    RDScores[i] = RDFake(G, U, i, a)
  }
  
  CSet = FindCoreSet(RDScores, U, a, B)
  
  
  return(0)
}





GeneralFrame(NN, .2)


