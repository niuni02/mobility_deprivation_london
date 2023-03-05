# Data analysis for quantitative report

# Load packages
library(ggplot2)
library(dplyr)
library(readr)

# Import the data
raw_data <- read.csv("raw_data/deprivation_and_mobility.csv")
View(raw_data)

# using the mutate() function to calculate percentages from raw data
london <- mutate(raw_data, percentage_not_deprived = (Households_not_deprived/Deprivation*100),
                 percentage_deprived = (100 - percentage_not_deprived),
                 percentage_home = (Work_from_home/Total_methods_travel)*100,
                 percentage_public = (Public_transport/Total_methods_travel)*100,
                 percentage_car = (Car_or_van/Total_methods_travel)*100,
                 percentage_other = (Other_travel/Total_methods_travel)*100)
View(london)

# using the select() function to keep only relevant data
london <- select(london, "GEO_CODE", "GEO_LABEL", "percentage_deprived", "percentage_home", "percentage_public", "percentage_car", "percentage_other")
View(london)

# SUMMARY STATISTICS
summary(london)

# CORRELATION ANALYSIS
# Building a ggplot
ggplot(london, aes(percentage_deprived, percentage_public)) + 
  geom_point(colour = "Dark Green") + 
  geom_smooth() +
  labs(title = "Scatter plot comparing deprived households and public transport use",
       x = "% deprived households", y = "% travel via public transport")
  # positive correlation between deprived households and public transport.

ggplot(london, aes(percentage_deprived, percentage_home)) + 
  geom_point(colour = "Blue") + 
  geom_smooth() +
  labs(title = "Scatter plot comparing deprived households and working from home",
       x = "% deprived households", y = "% working from home")

ggplot(london, aes(percentage_deprived, percentage_car)) + 
  geom_point(colour = "Red") + 
  geom_smooth() +
  labs(title = "Scatter plot comparing deprived households and car/van use",
       x = "% deprived households", y = "% travel via car/van")

ggplot(london, aes(percentage_deprived, percentage_other)) + 
  geom_point(colour = "Orange") + 
  geom_smooth() +
  labs(title = "Scatter plot comparing deprived households and other methods of travel",
       x = "% deprived households", y = "% other methods of travel")

# Correlation matrix and plots
correlation_tests <- select(london, percentage_deprived, percentage_home, percentage_public, percentage_car, percentage_other) 
cor(correlation_tests)
# using the corrplot package can represent the correlation matrices graphically
library(corrplot)
matrix <- cor(correlation_tests) # Produce a matrix holding correlations
corrplot(matrix, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)


# ANALYSIS OF SPATIAL AUTOCORRELATION (LISA MAPPING)
# Packages
library(rgdal)
library(rgeos)
library(spdep)
library(tmap)

# Importing the data and merging data with shapefile
MSOA_shapes <- st_read("raw_data/MSOA_2011_London_gen_MHW.shp")
View(MSOA_shapes)

# Joining attribute data to the polygon shapes
london_shapes <- merge(MSOA_shapes, london,  by.x="MSOA11CD", by.y="GEO_CODE")
View(london_shapes)

# Choropleth map
qtm(london_shapes, fill="percentage_deprived", border="gray", fill.style="quantile", fill.palette="Purples", 
    title = "Deprived households in London (2011)", fill.title= "% deprived households", frame = FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size = 6) +
  tm_scale_bar(position = c("LEFT", "BOTTOM"), size = 0.75)

qtm(london_shapes, fill="percentage_public", border="gray", fill.style="quantile", fill.palette="Greens", 
    title = "Public transport use in London", fill.title= "% public transport use", frame = FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size = 6) +
  tm_scale_bar(position = c("LEFT", "BOTTOM"), size = 0.75)

qtm(london_shapes, fill="percentage_home", border="gray", fill.style="quantile", fill.palette="Blues", 
    title = "Persons working from home in London", fill.title= "% work from home", frame = FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size = 6) +
  tm_scale_bar(position = c("LEFT", "BOTTOM"), size = 0.75)

# Spatial autocorrelation
# Moran's I
# Identifying neighouring polygons
london_neighbours_test <- poly2nb(london_shapes, queen=TRUE)
london_neighbours <- poly2nb(london_shapes, snap=1)
summary(london_neighbours)

# Weighting the links
weights_test <- nb2listw(london_neighbours_test)
weights <- nb2listw(london_neighbours)

# Moran plot
moran.plot(london_shapes$percentage_deprived, listw = weights)

# Calculating the Moran's I statistic
moran.test(london_shapes$percentage_deprived, weights)

moran.plot(london_shapes$percentage_public, listw = weights)
moran.test(london_shapes$percentage_public, weights)

moran.plot(london_shapes$percentage_home, listw = weights)
moran.test(london_shapes$percentage_home, weights)

# LOCAL INDICATORS OF SPATIAL ASSOCIATION (LISA)
# Calculating LISAs
LISA_D <- localmoran(x = london_shapes$percentage_deprived, listw = weights)
summary(LISA_D)

# MAPPING LISAs
# Binding LISA values
LISA_D_map <- cbind(london_shapes, LISA_D)

# Mapping LISAs
qtm(LISA_D_map, fill="Ii", border="gray", fill.style="quantile",fill.palette="Purples", title = "LISA of deprivation in London", fill.title="Local Moran statistic", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))

# Significance
qtm(LISA_D_map, fill="Pr.z....E.Ii..",  border="gray", fill.style="fixed", fill.breaks=c(0, 0.25, 0.5, 0.75, 1), fill.palette="Reds", 
    title = "P-values for LISA of deprivation in London", fill.title="P-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM", size=1))

# Putting it all together on one map
LISA_D_map <- mutate(LISA_D_map, significant = ifelse(Ii < 0 & Pr.z....E.Ii.. <0.05, "Negative, p <0.05", NA), significant = ifelse(Pr.z....E.Ii.. >=0.05, "Insignificant", significant),
                   significant = ifelse(Ii > 0 & Pr.z....E.Ii.. <0.05, "Positive, p <0.05", significant))

qtm(LISA_D_map, fill="significant", border="gray", fill.style="cat", fill.palette="Purples", title = " LISA of deprivation in London", fill.title="Local Moran and p-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"),size=6)+ 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))

# LISA for public transport
LISA_pt <- localmoran(x = london_shapes$percentage_public, listw = weights)

LISA_pt_map <- cbind(london_shapes, LISA_pt)

qtm(LISA_pt_map, fill="Ii", border="gray", fill.style="quantile",fill.palette="Greens", title = "LISA of public transport use in London", fill.title="Local Moran statistic", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))

qtm(LISA_pt_map, fill="Pr.z....E.Ii..",  border="gray", fill.style="fixed", fill.breaks=c(0, 0.25, 0.5, 0.75, 1), fill.palette="Greens", 
    title = "P-values for LISA of public transport use in London", fill.title="P-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM", size=1))

LISA_pt_map <- mutate(LISA_pt_map, significant = ifelse(Ii < 0 & Pr.z....E.Ii.. <0.05, "Negative, p <0.05", NA), significant = ifelse(Pr.z....E.Ii.. >=0.05, "Insignificant", significant),
                     significant = ifelse(Ii > 0 & Pr.z....E.Ii.. <0.05, "Positive, p <0.05", significant))

qtm(LISA_pt_map, fill="significant", border="gray", fill.style="cat", fill.palette="Greens", title = " LISA of public transport in London", fill.title="Local Moran and p-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6)+ 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))


# LISA for work from home
LISA_w <- localmoran(x = london_shapes$percentage_home, listw = weights)

LISA_w_map <- cbind(london_shapes, LISA_w)

qtm(LISA_w_map, fill="Ii", border="gray", fill.style="quantile",fill.palette="Blues", title = "LISA of working from home in London", fill.title="Local Moran statistic", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))

qtm(LISA_w_map, fill="Pr.z....E.Ii..",  border="gray", fill.style="fixed", fill.breaks=c(0, 0.25, 0.5, 0.75, 1), fill.palette="Blues", 
    title = "P-values for LISA of public transport use in London", fill.title="P-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6) + 
  tm_scale_bar(position = c("LEFT", "BOTTOM", size=1))

LISA_w_map <- mutate(LISA_w_map, significant = ifelse(Ii < 0 & Pr.z....E.Ii.. <0.05, "Negative, p <0.05", NA), significant = ifelse(Pr.z....E.Ii.. >=0.05, "Insignificant", significant),
                      significant = ifelse(Ii > 0 & Pr.z....E.Ii.. <0.05, "Positive, p <0.05", significant))

qtm(LISA_w_map, fill="significant", border="gray", fill.style="cat", fill.palette="Blues", title = "LISA of working from home in London", fill.title="Local Moran and p-value", frame=FALSE) + 
  tm_compass(position = c("RIGHT", "TOP"), size=6)+ 
  tm_scale_bar(position = c("LEFT", "BOTTOM"))


# Linear regression
multi1 <- lm(percentage_deprived ~ percentage_home + percentage_public, data = london)
summary(multi1)
plot(multi1)

# two-tailed t-test
t.test(london$percentage_deprived, london$percentage_home)

t.test(london$percentage_public, london$percentage_home)

t.test(london$percentage_deprived, london$percentage_public)

t.test(london$percentage_deprived, london$percentage_home)