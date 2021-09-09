#' Dijkstra algorithm implementation ...
#' @export dijkstra
#' @param graph A graph
#' @param init_node a specific node in graph
#' @return Vector containing smallest distances to every node in graph
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm 


dijkstra <- function(graph, init_node){
  stopifnot(is.data.frame(graph), identical(names(graph), c("v1", "v2", "w")))
  stopifnot(is.numeric(init_node), init_node %in% graph[ ,1])
  
  total_nodes <- length(unique(graph[["v1"]]))
  unvisited_list <- as.vector(unique(graph[["v1"]]))
  visited_list <- list()
  
  scores = rep(Inf, total_nodes)
  names(scores) <- c(1:length(scores))
  scores[init_node] = 0
  
  current_node = init_node
  
  while(length(unvisited_list) > 0){
    unvisited_list <- unvisited_list[unvisited_list != current_node]
    visited_list <- c(visited_list, current_node)
    
    df1 <- graph[graph$v1 == current_node, ]
    s1 <- df1[["v2"]]
    weights <- df1[["w"]]
    
    for(i in 1:length(s1)){
      if(s1[i] %in% unvisited_list){
        sum <- (scores[current_node] + weights[i])  
        if(sum < scores[s1[i]]){
          scores[s1[i]] <- sum
        }
      }
    }
    
    if(length(visited_list) == total_nodes){
      break
    }
    
    copy <- scores
    for(node in visited_list){
      copy <- copy[-which(names(copy) == node)]
    }
    minval <- copy[which.min(copy)]
    current_node <- names(which(copy == minval))
  }
  
  return(as.vector(scores))
}

