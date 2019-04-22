LGD = 0.65


set.seed(3052343)
library(igraph)

adj_mtx <-  read.table("mtx.csv", header = T, sep = ";")
node_w <-   read.table("puf.csv", header = T, sep = ";")
node_s <- read.table("sizes.csv", header = T, sep = ";")

adj_mtx <- as.matrix(adj_mtx)
adj_mtx[is.na(adj_mtx)] = 0

G <- graph.adjacency((adj_mtx ), weighted = T)
V(G)$default <- 0
V(G)$capital <- as.numeric(as.character(node_w[,2]))
V(G)$size <- as.numeric(as.character(node_s[,2]))

#plot(G, layout = layout.kamada.kawai(G), edge.arrow.size=0.3, vertex.size = 10, vertex.label.cex = .75)

sim <- function(G, starting_node, l = 0.85, drawimage = T){
node_color <- function(n,m) c(rgb(0,0.7,0),rainbow(m))[n+1]

stop_ <- FALSE
j <- 1
default <- list(starting_node)

while(!stop_){
V(G)$default[default[[j]]] <- j
j <- j + 1; stop_ <- TRUE
for( i in default[[j-1]]){V(G)$capital <- V(G)$capital - l*G[,i]}
default[[j]] = setdiff((1:33)[V(G)$capital < 0], unlist(default)); 
if( length( default[[j]] ) > 0) stop_ <- FALSE
             }

#print(default)

if(drawimage) plot(G, layout = layout.kamada.kawai(G), edge.arrow.size=0.3, vertex.size = 12.5, vertex.color = node_color(V(G)$default, 4*length(default)), vertex.label.cex = .75)
sum(V(G)$size[unlist(default)])/sum(V(G)$size)

}

result <- sapply(1:33, function(j) sim(G,j,LGD, FALSE))

