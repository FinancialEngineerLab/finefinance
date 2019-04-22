library(igraph)

adj_mtx <-  read.table("mtx.csv", header = T, sep = ";")
adj_mtx <- as.matrix(adj_mtx)
adj_mtx[is.na(adj_mtx)] <- 0
adj_mtx[adj_mtx != 0] <- 1

G <- graph.adjacency(adj_mtx, mode = "undirected")


CORE <- largest.cliques(G)

for (i in 1:length(CORE)){

core <- CORE[[i]]
periphery <- setdiff(1:33, core)

V(G)$color[periphery] <- rgb(0,1,0)
V(G)$color[core] <- rgb(1,0,0)

print(i)
print(core)
print(periphery)

H <- induced.subgraph(G, periphery)
d <- mean(degree(H))


windows()
plot(G, vertex.color = V(G)$color, main = paste("Avg periphery degree:", round(d,2) ) )}

