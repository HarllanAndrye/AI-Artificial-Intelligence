
values <- read.csv("results_graph_dfs_astar.csv", header = TRUE, sep = ",")

dfs <- values[values$technic == "DFS",]
astar <- values[values$technic == "AStar",]

options(scipen=999) #retirar os numeros 0e+00,...
par(mfrow = c(1,2))

plot(dfs$qtdNode ~ dfs$qtdCities, type = "l", xlab = "Qtd cities", ylab = "", main = "Technic DFS", las = 1, xlim = c(4,10))
plot(dfs$time_ms ~ dfs$qtdCities, type = "l", xlab = "Qtd cities", ylab = "", main = "Technic DFS", las = 1, xlim = c(4,10))

plot(astar$qtdNode ~ astar$qtdCities, type = "l", xlab = "Qtd cities", ylab = "", main = "Technic A*", las = 1, xlim = c(4,15))
plot(astar$time_ms ~ astar$qtdCities, type = "l", xlab = "Qtd cities", ylab = "", main = "Technic A*", las = 1, xlim = c(4,15))

install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

dfsplot1 <- qplot(dfs$qtdCities, dfs$qtdNode, geom = "path", xlab = "Qtd cities", ylab = "Qtd node", main = "Technic DFS", las = 1)
dfsplot2 <- qplot(dfs$qtdCities, dfs$time_ms, geom = "path", xlab = "Qtd cities", ylab = "Time (ms)", main = "Technic DFS", las = 1)

astarplot1 <- qplot(astar$qtdCities, astar$qtdNode, geom = "path", xlab = "Qtd cities", ylab = "Qtd node", main = "Technic A*", las = 1)
astarplot2 <- qplot(astar$qtdCities, astar$time_ms, geom = "path", xlab = "Qtd cities", ylab = "Time (ms)", main = "Technic A*", las = 1)

grid.arrange(dfsplot1, dfsplot2, ncol=2, nrow=1)
grid.arrange(astarplot1, astarplot2, ncol=2, nrow=1)



