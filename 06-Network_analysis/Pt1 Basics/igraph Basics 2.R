### iGraph Basics, Pt. 2 / Prof. Quandt

library(igraph)

#--------------------------------------
# 4. Layouts - making things look nice
#--------------------------------------

vertices=read.csv("/Users/thorstenquandt/Desktop/Network data/igraph Basics 2/Friends_nodes_large.csv", header=T, as.is=T)
edges=read.csv("/Users/thorstenquandt/Desktop/Network data/igraph Basics 2/Friends_edges_large.csv", header=T, as.is=T)
net <- graph_from_data_frame(d=edges, vertices=vertices, directed=T) 

# For clarity, I rename one attribute. 
V(net)$Personen_Name <- V(net)$Names 
net <- delete_vertex_attr(net, "Names") 

plot(net) # Not good.
plot(net, 
     edge.arrow.size=.3, 
     edge.curved=.1, 
     vertex.label.color="black", 
     vertex.label.dist=1.5, 	
     vertex.color=c("pink","skyblue")[1+(V(net)$Geschlecht=="m")] 
) # Slightly better.

# Use the "nice layout. :-) (alternatively, try KK and others)
layout <- layout_nicely(net)
## layout <- layout_with_kk(net)

plot(net, layout=layout, 
     edge.arrow.size=.03, 
     edge.curved=.1, 
     edge.width=1,
     vertex.label=V(net)$Personen_Name,
     vertex.label.color="black", 
     vertex.label.dist=1.0, 
     vertex.color=c("pink","skyblue")[1+(V(net)$Geschlecht=="m")],
     vertex.size=4,  # Adjust this as needed
     vertex.label.cex=0.7  # Adjust label size as needed
)


## More general: Colors on the basis of education
colrs <- c("blue", "green", "red")  
# Convert Bildung to a factor with levels (i.e. a numeric variable)
V(net)$Bildung <- as.factor(V(net)$Bildung)
# Map the factor levels to colors
V(net)$color <- colrs[as.numeric(V(net)$Bildung)]
# Plot the graph
plot(net, layout=layout, 
     edge.arrow.size=.1, 
     edge.curved=.1, 
     vertex.label=V(net)$Personen_Name,
     vertex.label.color="black", 
     vertex.label.dist=1.0, 
     vertex.color=V(net)$color,
     vertex.size=4,  # Adjust this as needed
     vertex.label.cex=0.7  # Adjust label size as needed
)


## Node size one the basis of spendings, requires a normalization due to the big range 

min_size <- 1
max_size <- 30
normalized_sizes <- min_size + (max_size - min_size) * (V(net)$Ausgaben - min(V(net)$Ausgaben)) / (max(V(net)$Ausgaben) - min(V(net)$Ausgaben))

plot(net, layout=layout, 
     edge.arrow.size=.1, 
     edge.curved=.1, 
     vertex.label=V(net)$Personen_Name,
     vertex.label.color="black", 
     vertex.label.dist=1.0, 
     vertex.color=c("pink","skyblue")[1+(V(net)$Geschlecht=="m")],
     vertex.size=normalized_sizes,  # Vertex size based on noramlized "Ausgaben"
     vertex.label.cex=0.7  # Adjust label size as needed
)



## Exercise: Make it even nicer (?)  

## There is a strength parameter for the friendship edges we didn't use.
## Remember that you can also set various edge parameters that might be useful for this.
## You can alos switch off labels to make it clearer. 



## Partial networks on the basis of attributes

# Identify vertices matching the criteria
male_vertices <- which(V(net)$Geschlecht == "m")
# Create the subgraph
male_net <- induced_subgraph(net, vids = male_vertices)
plot(male_net)

criteria_vertices <- which(V(net)$Geschlecht == "m" & V(net)$Bildung == "Realschule")
criteria_net <- induced_subgraph(net, vids = criteria_vertices)
plot(criteria_net)
plot(criteria_net, layout=layout_with_fr, 
     edge.arrow.size=0.4, 
     edge.curved=0.2, 
     edge.widht=2,
     vertex.label=V(criteria_net)$Personen_Name,
     vertex.label.color="black", 
     vertex.label.dist=1.0, 
     vertex.color=V(criteria_net)$color,
     vertex.size=7,  # Adjust this as needed
     vertex.label.cex=1  # Adjust label size as needed
)


## How to add a legend in principle -> hard to fit 
#V(net)$Geschlecht <- as.factor(V(net)$Geschlecht)
#legend("topright",                    
#      legend=levels(V(net)$Geschlecht),    
#       fill=c("skyblue","pink")[1+(V(net)$Geschlecht=="m")],  
#       title="Geschlecht",                  
#       cex=1,           
#       pt.cex=1,   
#       title.cex=1,    
#)


#----------------------------------------------------------------
# 5. Calculation of essential measures - the easiest part!
#----------------------------------------------------------------

# Density 
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) #directed

# Degree
deg <- degree(net, mode="all") 
plot(net, edge.arrow.size=0.1, vertex.size=deg)
hist(deg, breaks=0.5:vcount(net)-1, main="Histogram Degree")
centr_degree(net, mode="in", normalized=T) #Gesamtnetzwerk

# Degree distribution 
deg.dist <- degree_distribution(net, cumulative=T, mode="all") # mode sets in/out degree - here it's both
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")

# Closeness
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) #Closeness centrality 

# Betweenness
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T) #Closeness centrality

# diamter = longest geodesic (longest shortest path)
diameter(net, directed=F, weights=NA)

# Distanz
mean_distance(net, directed=F)

#--------------------
# 6. Substructures
#--------------------

# Newman-Girvan
ceb <- cluster_edge_betweenness(net)
dendPlot(ceb, mode="hclust")
length(ceb)     # number of communities
membership(ceb) # community membership for each node
plot(ceb, net)
plot(ceb, net, layout=layout, 
     edge.arrow.size=.03, 
     edge.curved=.1, 
     edge.width=1,
     vertex.label=V(net)$Personen_Name,
     vertex.label.color="black", 
     vertex.label.dist=1.0, 
     vertex.color=c("pink","skyblue")[1+(V(net)$Geschlecht=="m")],
     vertex.size=4,  # Adjust this as needed
     vertex.label.cex=0.7  # Adjust label size as needed
)
# If you don't like the solution, you can manually select communities.
#communities <- cut_at(ceb, no=20)  # Get 20 communities
#V(net)$community <- communities
#plot(net, vertex.color=rainbow(20)[communities], vertex.label.cex=0.8, vertex.size=6, edge.arrow.size=0)

# An alternative: The "greedy" optimiziation of modularity
cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net), vertex.size=6,  vertex.label.cex=0.5)

# Cliques
# Find all cliques
all_cliques = cliques(net)
print(all_cliques)
# Find cliques of size 4
cliques_of_size_4 <- cliques(net, min=4, max=4)
print(cliques_of_size_4)
# Largest cliques
largest_cliques = largest_cliques(net)
print(largest_cliques)

# Coloring according to clique membership
# colors <- rainbow(length(cliques_of_size_4))
# node_colors <- rep(NA, vcount(net))
# for (i in 1:length(cliques_of_size_4)) {
#   clique_members <- cliques_of_size_4[[i]]
#   node_colors[clique_members] <- colors[i]
# }
# plot(net, 
#      edge.arrow.size=.03, 
#      edge.curved=.1, 
#      edge.width=1,
#      vertex.label=V(net)$Personen_Name,
#      vertex.label.color="black", 
#      vertex.label.dist=1.0, 
#      vertex.color=node_colors, # colors according to clique
#      vertex.size=4,  
#      vertex.label.cex=0.7  
# )
## However, this has some issues... which ones?


# K-Cores
kc <- coreness(net, mode="all")
num_cores <- max(kc)  # Get the maximum coreness
plot(net, vertex.size=kc,
     vertex.label=kc, vertex.color=rainbow(num_cores)[kc], edge.arrow.size=0)
# This plots according to maximum coreness, not group membership!


# Addendum: Custom plotting function, as an example, in case you are bored with these...
# plot_network_standard <- function(net) {
#   
#   plot(net, 
#        edge.arrow.size=.03, 
#        edge.curved=.1, 
#        edge.width=1,
#        vertex.label=V(net)$Personen_Name,
#        vertex.label.color="black", 
#        vertex.label.dist=1.0, 
#        vertex.color="white",  # Default color. Adjust if needed.
#        vertex.size=4,
#        vertex.label.cex=0.7
#   )
# }
#plot_network_standard(net)

