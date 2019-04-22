#### 1-9 financial Networks ####

library(igraph)

#### Represnetaion, simulatiopn, and visulaization of financial networks ####
### Erdos-Renyi network : uniformley random ###
set.seed(7)

e <- erdos.renyi.game(100, 0.1) #node, edge
plot(e)
graph.density(e)
transitivity(e)
average.path.length (e)

### Small world Networks ###

set.seed(592)
w <- watts.strogatz.game(1,100, 5,0.05)
plot(w) #random scale-free networks(watts strogatz)
graph.density(w)
transitivity(w) #higher
average.path.length(w) # longer

we <- get.edgelist(w)
we
head(we,5)

#### Analysis of networks' structure and detection of topology changes ####

data <- read.csv2("C:/Users/Shinhyunjin/Dropbox/data/introquantfinance/networktable.csv")
data
str(data)

size <- read.csv2("C:/Users/Shinhyunjin/Dropbox/data/introquantfinance/vertices.csv")

bignetwork <- graph.data.frame(data, vertices  = size)
bignetwork
is.connected(bignetwork)
table(is.multiple(bignetwork))
str(is.loop(bignetwork))

snetwork <- simplify(bignetwork, edge.attr.comb = "sum")
plot(snetwork, edge.arrow.size = 0.4)


#

communities

#
data
monthlynetwork <- subset(data, (Year == 2008) & (Month == 9))
monthlynetwork

mAmount <- with(data, aggregate(Amount, by = list(Month = Month, Year = Year), FUN = sum))
plot(ts(mAmount$x, start = c(2007,1), frequency=12), ylab = "Amount")

#

ds <- sapply(2007:2010, function(year){
  sapply(1:12, function(month){
    mdata <- subset(data, (Year == year) & (Month == month))
    graph.density(graph.data.frame(mdata))
  })
})
plot(ts(as.vector(ds), start = c(2007,1), frequency=12))
abline(v=2008+259/366, col = 'red') #Lehman-fall abline

#### Contribution to systemtic risk - identification of SIFIs ####
# Sysmetically important financial institutions(SIFIs) #

g <- graph.data.frame(data)
degree <- degree(g, normalized = TRUE)
between <- betweenness(g, normalized = TRUE)
closeness <- closeness(g, normalized = TRUE)
eigenv <- evcent(g, directed =TRUE)$vector

# normalize

norm <- function(x) x / mean(x)
index <- (norm(degree) + norm(between)+ norm(closeness) + norm(eigenv))/4
index
hist(index)
