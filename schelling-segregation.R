# ================ Schelling's Segregation Model ================
# Sarah Li
# http://heysarah.li/
# https://github.com/serifluous
#=================
# Setup
#=================
rm(list=ls()) 
library(ggplot2)
library(knitr)



# ============== Machinery for the Schelling Model ==============
# 
# ===============================================================
# Distance : vector matrix -> vector
# Calculates distances between a given point and list of points
# ================
Distance <- function(point, all.points)
{
  number.of.points = dim(all.points)[1]
  distances = rep(NA,number.of.points)
  for (i in 1:number.of.points) {
    x.distance = (point[1] - all.points[i,1])^2
    y.distance = (point[2] - all.points[i,2])^2
    distances[i] = sqrt(x.distance + y.distance)
  }
  return(distances)
}


# ================
# RandomizePopulation: num num num -> (data frame)
# Given population totals of red (r = 1), green (g = 2), and blue (b = 3),
# RandomizePopulation takes the population of each type and
# produces a data frame with each individual and his/her random house allocation
# ================
RandomizePopulation <- function(r.population = 0, g.population = 0, b.population = 0)
{
  total.population = r.population + g.population + b.population
  # Produce data points for given populations
  total.population.vector = c(rep(1, r.population),
                              rep(2, g.population),
                              rep(3, b.population))
  # Randomize order of points in vector
  randomized.population = sample(total.population.vector, total.population)
  # Chooses random house allocation on x-y plane bounded by (0,1) on both sides
  first.allocation = cbind(randomized.population,
                           runif(total.population,0,1),
                           runif(total.population,0,1),
                           rep(0, total.population))
  return(as.data.frame(first.allocation))
}


# ================
# PickANewHouse : num num (data frame) (data frame) -> (data frame)
# Generates a new house allocation for the given individual j if the ratio of neighbors is unsatisfactory.
# If the individual is satisfied, Column 4 'satisfied?' = true
# ================
# Distances data frame:
# Column 1 = individual type (i.e., color)
# Column 2 = distance from the given individual's house
# ================
# Population data frame:
# Column 1 = type
# Column 2 = x
# Column 3 = y
# Column 4 = satisfied?
# ================
PickANewHouse <- function(j, type, distances.df, population.df)
{ 
  # *.ratio, *.neighbors.considered are global variables.
  # See function Schelling below.
  # Selects ratio of neighbors of same type deemed satisfactory
  # and number of neighbors considered in ratio
  # depending on type of individual j
  if (type == 1) {
    selected.ratio = r.ratio
    neighbors.considered = distances.df[1:r.neighbors.considered,]
  } else if (type == 2) {
    selected.ratio = g.ratio
    neighbors.considered = distances.df[1:g.neighbors.considered,]
  } else {
    selected.ratio = b.ratio
    neighbors.considered = distances.df[1:b.neighbors.considered,]
  }
  
  # Calculates actual ratio
  same.type = subset(neighbors.considered, neighbors.considered[,1] == type)
  j.ratio = dim(same.type)[1] / dim(neighbors.considered)[1]
  # Randomizes housing allocation if ratio is unsatisfactory.
  # Otherwise, 'satisfactory?' = true
  if (j.ratio < selected.ratio) {
    population.df[j,2] = runif(1,0,1)
    population.df[j,3] = runif(1,0,1)
  } else {
    population.df[j,4] = 1
  }
  return(population.df)
}


# ================
# NewAllocation: (data frame) num num num num num num -> (data frame)
# If there still remain unsatisfied individuals,
# NewAllocation generates new housing allocations for those who are unsatisfied
# ================
NewAllocation <- function(population)
{
  population.df <<- population
  total.population = dim(population.df)[1]
  
  for (j in 1:total.population) {
    distances = Distance(c(population.df[j,2], population.df[j,3]),
                         cbind(population.df[,2], population.df[,3]))
    distances.with.type = cbind(population.df[,1], distances)
    distances.df <<- as.data.frame(distances.with.type)
    distances.df <<- distances.df[order(distances.df$distances),]
    distances.df <<- distances.df[-1,]
    
    population.df <<- PickANewHouse(j, population.df[j,1], distances.df, population.df)
  }
  
  return(population.df)
}


# ================
# Ternary operator function, by Stack Overflow user kohske
# http://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
# ================
`?` <- function(x, y)
{
  eval(
    sapply(
      strsplit(
        deparse(substitute(y)), 
        ":"
      ), 
      function(e) parse(text = e)
    )[[2 - as.logical(x)]])
}



# ================ Schelling's Segregation Model ================
# 
# ===============================================================
# Schelling : num num num num num num num num num -> plot (data frame)
# Simulates Schelling's Segregation model
# ================
Schelling <- function(r.population = 0,
                      g.population = 0,
                      b.population = 0,
                      r.same.type.desired = 0, r.neighbors.considered = 0,
                      g.same.type.desired = 0, g.neighbors.considered = 0,
                      b.same.type.desired = 0, b.neighbors.considered = 0)
{
  r.same.type.desired <<- r.same.type.desired
  r.neighbors.considered <<- r.neighbors.considered
  g.same.type.desired <<- g.same.type.desired
  g.neighbors.considered <<- g.neighbors.considered
  b.same.type.desired <<- b.same.type.desired
  b.neighbors.considered <<- b.neighbors.considered
  r.ratio <<- ((r.neighbors.considered != 0) ? (r.same.type.desired / r.neighbors.considered) : 0)
  g.ratio <<- ((g.neighbors.considered != 0) ? (g.same.type.desired / g.neighbors.considered) : 0)
  b.ratio <<- ((b.neighbors.considered != 0) ? (b.same.type.desired / b.neighbors.considered) : 0)
  
  total.population = r.population + g.population + b.population
  allocation <<- RandomizePopulation(r.population, g.population, b.population)
  allocated.plot <- ggplot(allocation, aes(x=V2, y=V3)) +
    geom_point(colour=(allocation$randomized.population + 1), size = 4, alpha = 0.6) +
    scale_fill_brewer() + 
    theme(plot.title = element_text(size=20, face="bold", vjust = 2), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) +
    labs(title = "Schelling's Segregation Model\nIteration 0\nHappiness: ???? %", x = "X", y = "Y")
  print(allocated.plot)
  
  i = 0
  while (1) {
    number.satisfied = dim(subset(allocation, allocation[,4] == 1))[1]
    
    if ((number.satisfied/total.population) != 1) {
      allocation[,4] <<- rep(0,total.population)
      allocation <<- NewAllocation(allocation)
      i = i+1
      number.satisfied = dim(subset(allocation, allocation[,4] == 1))[1]
      print(paste("Percent Happy:", 100 * (number.satisfied/total.population),
                  "(iteration", i, ")" ))
      allocated.plot <- ggplot(allocation, aes(x=V2, y=V3)) +
        geom_point(colour=(allocation$randomized.population + 1), size = 4, alpha = 0.6) +
        scale_fill_brewer() + 
        theme(plot.title = element_text(size=20, face="bold", vjust = 2), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) +
        labs(title = paste("Schelling's Segregation Model\nIteration", i,"\nHappiness:", round(100 * number.satisfied/total.population,2), "%"), x = "X", y = "Y")
      print(allocated.plot)
    } else {
      print(paste("Iterations: ", i))
      schelling.iterations <<- i
      return(allocation)
    }
  }
}


# ================
# Similar Neighbors Index
# ================
# SimilarNeighbors : (dataframe) -> num
# Calculates mean of proportion of neighbors similar to each individual
# ================
SimilarNeighbors <- function(allocation)
{
  total.population = dim(allocation)[1]
  neighbors <- rep(NA,total.population)
  
  for (j in 1:total.population) {
    distances = Distance(c(population.df[j,2], population.df[j,3]),
                         cbind(population.df[,2], population.df[,3]))
    distances.with.type = cbind(population.df[,1], distances)
    distances.df <<- as.data.frame(distances.with.type)
    distances.df <<- distances.df[order(distances.df$distances),]
    distances.df <<- distances.df[-1,]
    
    type = allocation[j,1]
    if (type == 1) {
      neighbors.considered = distances.df[1:r.neighbors.considered,]
    } else if (type == 2) {
      neighbors.considered = distances.df[1:g.neighbors.considered,]
    } else {
      neighbors.considered = distances.df[1:b.neighbors.considered,]
    }
    
    same.type = subset(neighbors.considered, neighbors.considered[,1] == type)
    j.ratio = dim(same.type)[1] / dim(neighbors.considered)[1]
    neighbors[j] = j.ratio
  }
  
  return(mean(neighbors))
}



# ================ Sample Output ================
# 
# ================
# Sample 1
# Each population has 150 individuals
# and are satisfied if at least 3/8 of their neighbors are similar
# ================
sample.schelling.1 = Schelling(r.population = 150,
                               g.population = 150,
                               b.population = 150,
                               r.same.type.desired = 3, r.neighbors.considered = 8,
                               g.same.type.desired = 3, g.neighbors.considered = 8,
                               b.same.type.desired = 3, b.neighbors.considered = 8)

# Similar Neighbor Index for Sample 1
SimilarNeighbors(sample.schelling.1)


# ================
# Sample 2
# The first population has 500 individuals and satisfied if 9/12 neighbors are similar
# The second and third population have 100 individuals each
# and are satisfied if 2/12 neighbors are similar
# ================
sample.schelling.2 = Schelling(r.population = 500,
                               g.population = 100,
                               b.population = 100,
                               r.same.type.desired = 9, r.neighbors.considered = 12,
                               g.same.type.desired = 2, g.neighbors.considered = 12,
                               b.same.type.desired = 2, b.neighbors.considered = 12)

# Similar Neighbor Index for Sample 2
SimilarNeighbors(sample.schelling.2)
