name <- "Johan Marbinah"
liuid <-  "nnnnn000"


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



wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)

