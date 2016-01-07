# Les 4
# job de Pater
# 150106

library(raster)



# Example 1 ---------------------------------------------------------------

r <- s <- raster(ncol=50, nrow=50)
# fill the raster with values
r[] <- 1:ncell(r)
s[] <- 2 * (1:ncell(s))
s[200:400] <- 150
s[50:150] <- 151
# perform the replacement
r[s %in% c(150, 151)] <- NA
# Visualize the result
plot(r)


# Ex 2 --------------------------------------------------------------------

data("cars")
plot(cars)

library(sp)
data("meuse"); class(meuse)
coordinates(meuse) <- c("x", "y")
class(meuse)
bubble(meuse, "cadmium", maxsize = 2.5,
       main = "zic concentrations (ppm)", key.entries = 2^(-1:4))

# Load meuse.riv dataset
data(meuse.riv)
# Create an object of class spatialPolygons from meuse.riv
meuse.sr <- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
# Load the meuse.grid dataset
data(meuse.grid)
# Assign coordinates to the dataset and make it a grid
coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) = TRUE
# Plot all variables of the meuse.grid dataset in a multiple window spplot
spplot(meuse.grid, col.regions=bpy.colors(), main = "meuse.grid",
       sp.layout=list(
         list("sp.polygons", meuse.sr),
         list("sp.points", meuse, pch="+", col="black")
       )
)



# Ex 3 --------------------------------------------------------------------

library(raster)
# Function to substract 2 rasterLayers
minusRaster <- function(x, y, plot=FALSE) { 
  z <- x - y
  if (plot) {
    plot(z)
  }
  return(z)
}

# Let's generate 2 rasters 
# that first one is the R logo raster
# converted to the raster package file format.
r <- raster(system.file("external/rlogo.grd", package="raster")) 
# The second RasterLayer is derived from the initial RasterLayer in order
# to avoid issues of non matching extent or resolution, etc
r2 <- r 
# Filling the rasterLayer with new values.
# The /10 simply makes the result more spectacular
r2[] <- (1:ncell(r2)) / 10
# simply performs the calculation
r3 <- minusRaster(r, r2, 1) 



# Ex 4 --------------------------------------------------------------------

library(raster)

# Create a raster layer and fill it with "randomly" generated integer values
a <- raster(nrow=50, ncol=50)
a[] <- floor(rnorm(n=ncell(a)))

# The freq() function returns the frequency of a certain value in a RasterLayer
# We want to know how many times the value -2 is present in the RasterLayer
freq(a, value=-2)

# Let's imagine that you want to run this function over a whole list of RasterLayer
# but some elements of the list are impredictibly corrupted
# so the list looks as follows
b <- a
c <- NA
list <- c(a,b,c)
# In that case, b and a are raster layers, c is ''corrupted''

# Running freq(c) would return an error and crash the whole process
out <- list()
for(i in 1:length(list)) {
  out[i] <- freq(list[[i]], value=-2)
}


# Therefore by building a function that includes a try()
# we are able to catch the error
# allowing the process to continue despite missing/corrupted data.
fun <- function(x, value) {
  tr <- try(freq(x=x, value=value), silent=TRUE)
  if (class(tr) == 'try-error') {
    return('This object returned an error')
  } else {
    return(tr)
  }
}

# Let's try to run the loop again
out <- list()
for(i in 1:length(list)) {
  out[i] <- fun(list[[i]], value=-2)
}
out


# Note that using a function of the apply family would be a more
# elegant/shorter way to obtain the same result
(out <- sapply(X=list, FUN=fun, value=-2))



# Ex 5 debugging 2 --------------------------------------------------------

foo <- function(x) {
  x <- x + 2
  print(x)
  bar(2) 
}

bar <- function(x) { 
  x <- x + a.variable.which.does.not.exist 
  print(x)
}

foo(2) 
## gives an error

traceback()
## 2: bar(2) at #1
## 1: foo(2)
# Ah, bar() is the problem
  