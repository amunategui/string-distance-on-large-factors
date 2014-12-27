# get the Hadley Wickham's vehicles data set
library(RCurl)
urlfile <-'https://raw.githubusercontent.com/hadley/fueleconomy/master/data-raw/vehicles.csv'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
vehicles <- read.csv(textConnection(x))

# size the data
nrow(vehicles)
length(unique(vehicles$model))

# get a small sample for starters
vehicles_small <- vehicles[1:100,]
length(unique(vehicles_small$model))
head(unique(as.character(vehicles_small$model)))

# call the stringdistmatrix function and request 20 groups
library(stringdist)
uniquemodels <- unique(as.character(vehicles_small$model))
distancemodels <- stringdistmatrix(uniquemodels,uniquemodels,method = "jw")
rownames(distancemodels) <- uniquemodels
hc <- hclust(as.dist(distancemodels))

# visualize the dendrogram
plot(hc)
rect.hclust(hc,k=20)

# get a bigger sample
vehicles_small <- vehicles[1:2000,]
length(unique(vehicles_small$model))

# run the stringdistmatrix function and request 200 groups
uniquemodels <- unique(as.character(vehicles_small$model))
distancemodels <- stringdistmatrix(uniquemodels,uniquemodels,method = "jw")
rownames(distancemodels) <- uniquemodels
hc <- hclust(as.dist(distancemodels))
dfClust <- data.frame(uniquemodels, cutree(hc, k=200))
names(dfClust) <- c('modelname','cluster')

# visualize the groupings
plot(table(dfClust$cluster))
print(paste('Average number of models per cluster:', mean(table(dfClust$cluster))))

# lets look at the top groups and see what the algorithm did:
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head (dfClust[c('cluster','modelname')],50)

# try combining fields together
vehicles_small$modelAndTrany <- paste0(as.character(vehicles_small$model)," ",as.character(vehicles_small$trany))
print(length(unique(vehicles_small$modelAndTrany)))

uniquemodels <- unique(as.character(vehicles_small$modelAndTrany))
distancemodels <- stringdistmatrix(uniquemodels,uniquemodels,method = "jw")
rownames(distancemodels) <- uniquemodels
hc <- hclust(as.dist(distancemodels))
dfClust <- data.frame(uniquemodels, cutree(hc, k=500))
names(dfClust) <- c('modelname','cluster')
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head (dfClust[c('cluster','modelname')],50)

# build a convenient function to do all of the above
GroupFactorsTogether <- function(objData, variableName, clustersize=200, method='jw') {
        #      osa: Optimal string aligment, (restricted Damerau-Levenshtein distance).
        #      lv: Levenshtein distance (as in R's native adist).
        #      dl: Full Damerau-Levenshtein distance.
        #      hamming: Hamming distance (a and b must have same nr of characters).
        #      lcs: Longest common substring distance.
        #      qgram: q-gram distance.
        #      cosine: cosine distance between q-gram profiles
        #      jaccard: Jaccard distance between q-gram profiles
        #      jw: Jaro, or Jaro-Winker distance.
        #      soundex: Distance based on soundex encoding
        
        #       stringdistmatrix(a, b, method = c("osa", "lv", "dl", "hamming", "lcs",
        #               "qgram", "cosine", "jaccard", "jw", useBytes = FALSE,
        #               weight = c(d = 1, i = 1, s = 1, t = 1), maxDist = Inf, q = 1, p = 0,
        #               useNames = FALSE, ncores = 1, cluster = NULL)
        #               require(stringdist)
        
        str <- unique(as.character(objData[,variableName]))
        print(paste('Uniques:',length(str)))
        
        d <- stringdistmatrix(str,str,method = c(method))
        
        rownames(d) <- str
        hc <- hclust(as.dist(d))
        
        dfClust <- data.frame(str, cutree(hc, k=clustersize))
        
        plot(table(dfClust$'cutree.hc..k...k.'))
        
        most_populated_clusters <- dfClust[dfClust$'cutree.hc..k...k.' > 5,]
        names(most_populated_clusters) <- c('entry','cluster')
        
        # sort by most frequent
        t <- table(most_populated_clusters$cluster)
        t <- cbind(t,t/length(most_populated_clusters$cluster))
        t <- t[order(t[,2], decreasing=TRUE),]
        p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
        most_populated_clusters <- merge(x=most_populated_clusters, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
        most_populated_clusters <- most_populated_clusters[rev(order(most_populated_clusters$binCount)),]
        names(most_populated_clusters) <-  c('cluster','entry')
        return (most_populated_clusters[c('cluster','entry')])
}