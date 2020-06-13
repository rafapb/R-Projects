# The main objective of this script is to recreate a plot from The Economist using ggplot. 
# The article and the plot can be found here: https://www.economist.com/graphic-detail/2011/12/02/corrosive-corruption

library(ggplot2)
library(data.table)
library(ggthemes)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

df <- fread('Economist_Data.csv', drop=1)
pl <- ggplot(df, aes(x=CPI, y=HDI, color=Region)) + geom_point(size=4,shape=1)
pl2 <- pl + geom_smooth(aes(group=1),method='lm',formula=y~log(x),se=F,color='red')
pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)
pl4 <- pl3 + scale_x_continuous(limits = c(0.9,10.5),breaks=1:10)

print(pl4 + theme_economist_white())

# END