# Data from: https://archive.ics.uci.edu/ml/datasets/wine+quality

# The main purpose of this script is to build a kMeans model to classify wine
# into red or white wine.

df1 <- read.csv('CSV files for ML Projects/winequality-red.csv', sep = ';')
df2 <- read.csv('CSV files for ML Projects/winequality-white.csv', sep = ';')

# ADD LABEL FOR RED AND WHITE WINES
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

wine <- rbind(df1, df2)

# EDA

library(ggplot2)

# HISTOGRAM OF RESIDUAL SUGAR
pl <- ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

# HISTOGRAM OF CITRIC ACID
pl <- ggplot(wine,aes(x=citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

# HISTOGRAM OF ALCOHOL CONCENTRATION

pl <- ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

# SCATTER PLOT OF RESIDUAL SUGAR VS CITRIC ACID

pl <- ggplot(wine,aes(x=citric.acid,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

# SCATTER PLOT OF RESIDUAL SUGAR VS VOLATILE ACIDITY

pl <- ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

# CLUSTERING THE DATAA
clus.data <- wine[,1:12]

# KMEANS MODEL
wine.cluster <- kmeans(clus.data,2)

print(wine.cluster$centers)

print(table(wine$label,wine.cluster$cluster))

# END