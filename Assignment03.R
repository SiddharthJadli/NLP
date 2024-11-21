# Set working directory to desktop
setwd("/Users/siddharthjadli/Study/Monash/2024Sem1/FIT3152/Ass03")

# Cleaning up the environment before starting
rm(list = ls()) 

set.seed(32014252) # Setting seed for reproducability

# LIBRARIES
library(tm)
library(slam)       # Matrices and arrays
library(SnowballC)  # Stemming
library(proxy)      # Cosine distances
library(igraph)     # Graph analysis
library(igraphdata) # Graph analysis

# Get file path to folder "test" where the documents are located
cname = file.path(".", "Corpus")
docs = Corpus(DirSource((cname)))
print(summary(docs))

# Tokenise
# Hyphen to space, ref Williams
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))

# Filter Words by removing stop words and white space
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

# Stem
docs <- tm_map(docs, stemDocument, language = "english")

# Create document term matrix
dtm <- DocumentTermMatrix(docs)

# Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.4)  # Remove columns with 40% empty (0) cells
dtms = as.matrix(dtms)
write.csv(dtms, "dtms.csv")

# Clustering
set.seed(32014252) # keep clustering consistent

distmatrix = proxy::dist(dtms, method = "cosine")
fit = hclust(distmatrix, method = "ward.D")
plot(fit, xlab = "")

# Vector of topic labels in same order as corpus
topics = c("Lit","Lit","Lit", "Lit", "Lit", "Lit",
            "Sport", "Sport", "Music", "Music", "Sport", "Sport", "Music", "Sport",
            "Sport")

groups = cutree(fit, k = 3) # vector of cluster numbers

# create matrix and reorder columns
TA = as.data.frame.matrix(table(GroupNames = topics, Clusters = groups))
TA


# Single-mode network showing the connections between the documents

# Convert original DTM to binary matrix
dtmsx = as.matrix(dtms)
dtmsx = as.matrix((dtmsx > 0) + 0)

# multiply binary matrix by its transpose
ByAbsMatrix = dtmsx %*% t(dtmsx)

# make leading diagonal zero
diag(ByAbsMatrix) = 0

# create graph object
ByAbs = graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)

set.seed(32014252) # keep clustering consistent
plot(ByAbs)

# calculate network centrality measures and combine in a dataframe
d = as.table(degree(ByAbs))
b = as.table(betweenness(ByAbs))
c = as.table(closeness(ByAbs))
e = as.table(evcent(ByAbs)$vector)
stats = as.data.frame(rbind(d,b,c,e))
stats = as.data.frame(t(stats))
colnames(stats) = c("degree", "betweenness", "closeness", "eigenvector")

averagePath = average.path.length(ByAbs)
diameter = diameter(ByAbs)

cat("Average Path Length: ", averagePath)
cat("\nDiameter: ", diameter, "\n\n")
print(stats, digits = 3)

# sort and explore key nodes
head(stats)
stats[order(-stats$betweenness),]
stats[order(-stats$closeness),]
stats[order(-stats$eigenvector),]

set.seed(32014252) # keep community detection consistent
#identify community groups/clusters using cluster_fast_greedy()
cfg = cluster_fast_greedy(as.undirected(ByAbs))
g_cfg = plot(cfg,
             as.undirected(ByAbs),vertex.label=V(ByAbs)$role,main="Fast
Greedy")


# TOKENS MATRIX
# original dtm
dtmsx = as.matrix(dtms)

# convert to binary matrix
dtmsx = as.matrix((dtmsx > 0) + 0)

# multiply transpose binary matrix by binary matrix
ByTokenMatrix = t(dtmsx) %*% dtmsx

# make leading diagonal zero
diag(ByTokenMatrix) = 0

# create graph object
ByToken = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)

set.seed(32014252) # keep clustering consistent
plot(ByToken) # plot

# calculate network centrality measures and combine in a dataframe
d = as.table(degree(ByToken))
b = as.table(betweenness(ByToken))
c = as.table(closeness(ByToken))
e = as.table(evcent(ByToken)$vector)

stats = as.data.frame(rbind(d,b,c,e))
stats = as.data.frame(t(stats))
colnames(stats) = c("degree", "betweenness", "closeness", "eigenvector")

#sort and explore key nodes
head(stats)
stats[order(-stats$betweenness),]
stats[order(-stats$closeness),]
stats[order(-stats$eigenvector),]
stats[order(-stats$degree),]

set.seed(32014252) # keep clustering consistent
#identify community groups/clusters using cluster_fast_greedy()
cfg = cluster_fast_greedy(as.undirected(ByToken))

g_cfg = plot(cfg,
             as.undirected(ByToken),vertex.label=V(ByToken)$role,main="Fast
Greedy")

# BIPARTITE NETWORK

# formatting data for bipartite graph
dtmsa = as.data.frame(dtms) # clone dtms
dtmsa$ABS = rownames(dtmsa) # add row names
dtmsb = data.frame()

for (i in 1:nrow(dtmsa)){
    for (j in 1:(ncol(dtmsa)-1)){
        touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)], colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb, touse ) # close loops
    } 
  } 

colnames(dtmsb) = c("weight", "abs", "token")
dtmsc = dtmsb[dtmsb$weight != 0,] # delete 0 weights

dtmsc = dtmsc[,c(2,3,1)]  # put colunms in order: abs, token, weight


set.seed(32014252) # keep plot consistent
# plotting bipartite graph
g <- graph.data.frame(dtmsc, directed=FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightgreen", "pink")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
plot(g)

