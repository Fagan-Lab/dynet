# voter.R
#
# Implementation of voter model dynamics on a network in R.
# author: Luke Perry
# Submitted as part of research assistance for Dr. William Fagan and Anshuman Swain
#
# Simulate voter-model-style dynamics on a network.
# Nodes are randomly assigned a state in :math:`\{-1, 1\}`; at each
# time step all nodes asynchronously update by choosing their new
# state uniformly from their neighbors. Generates an :math:`N \times
# L` time series.
# The results dictionary also stores the ground truth network as
# `'ground_truth'`.
#
# Parameters
# ----------
# G (matrix)
#     the input (ground-truth) graph with `N` nodes.
# L (int)
#     the length of the desired time series.
# noise (float, str or None)
#     if noise is present, with this probability a node's state will
#     be randomly redrawn from :math:`\{-1, 1\}` independent of its
#     neighbors' states. If 'automatic', set noise to :math:`1/N`.
#
# Returns
# -------
# TS (matrix)
#     an :math:`N \times L` array of synthetic time series data.


# dependency import
library(igraph)

# get dimensions helper function
DIM <- function( ... ){
  args <- list(...)
  lapply( args , function(x) { if( is.null( dim(x) ) )
    return( length(x) )
    dim(x) } )
}

voter <- function(G, L, noise=NULL) {
  # get adj matrix
  G = graph_from_adjacency_matrix(x, weighted = TRUE)
  # get num of nodes in igraph obj
  N = unlist(DIM(V(G))) 
  
  # noise input validation
  if (is.null(noise)) {
    noise <- 0
  } else if (identical(noise, "automatic" | noise, "auto")) {
    noise <- 1 / N
  } else if (!is.numeric(noise)|!is.null(noise)) {
    stop("noise must be a number, 'automatic', or NULL")
  }

  # igraph stores weights and edges separately, 
  # to get around this I did not use the transition matrix directly. 
  # I used it as reference/temp to calculate weights then later refer to weights with G[i]
  #
  # get adj mat
  transitions = get.adjacency(G)  
  # sum cols of adjmat for n columns
  colSums = unname(tapply(E(G)$weight, (seq_along(E(G)$weight)-1) %/% ncol(transitions), sum))
  # divide each vertex's weight by sum of edge weights for that vertex
  colSumIter = 1
  currColSumIter = 1
  for (i in 1:length(E(G)$weight)) {
    E(G)$weight[i] <- E(G)$weight[i] / colSums[colSumIter]
    if (currColSumIter == ncol(transitions)) {
      currColSumIter = 0
      colSumIter = colSumIter + 1
    }
  }
  
  # generate time series
  TS = matrix(rep(0, N * L), nrow = N)
  randNum = runif(N) # random uniform distribution
  for (i in 1:nrow(TS)) {
    if (randNum[i] < .5){
      TS[i,1] = 1
    } else {
      TS[i,1] = -1
    }
  }
  
  # indices to help with random sampling
  indices = 1:N
  
  # iterate through time series data and use transition graph weights as sum
  for (t in 2:L) {

    TS[,t] = TS[,t-1]
    for (i in indices) {
      TS[i,t] <- sample(c(TS[,t]), 1, replace = FALSE, prob = c(G[i]))
       if (!is.null(noise)) {
         if (runif(1) < noise) {
           if (runif(1) < .5) {
             TS[i, t] = 1
           } else {
             TS[i, t] = -1
           }
         }
       }
    }
    
  }

  return(TS)
}

# R code and test data:
#
# L = 6
# Noise = .5
#
#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   -1    1    1    1    1    1
#[2,]   -1   -1    1    1   -1    1
#[3,]    1    1    1   -1    1    1
#
#
#
# Python code and test data:
#
# import networkx as nx
# import numpy as np
# G = nx.Graph()
# G.add_edge("a", "a", weight=1)
# G.add_edge("a", "b", weight=2)
# G.add_edge("a", "c", weight=3)
# N = G.number_of_nodes()
# L = 6
# Noise = .5
# transitions = nx.to_numpy_array(G)
# transitions = transitions / np.sum(transitions, axis=0)
# noise = .5
# TS = np.zeros((N, L))
# TS[:, 0] = [1 if x < 0.5 else -1 for x in np.random.rand(N)]
# indices = np.arange(N)
# for t in range(1, L):
#     np.random.shuffle(indices)
#     TS[:, t] = TS[:, t - 1]
#     for i in indices:
#         TS[i, t] = np.random.choice(TS[:, t], p=transitions[:, i])
#         if np.random.rand() < noise:
#             TS[i, t] = 1 if np.random.rand() < 0.5 else -1
# print(TS)
# >>> [[ 1.  1. -1. -1.  1. -1.]
#      [-1.  1.  1.  1. -1.  1.]
#      [-1.  1. -1. -1. -1.  1.]]



