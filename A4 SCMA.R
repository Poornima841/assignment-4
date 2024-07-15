# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv('C:/Users/anjel/Downloads/Survey.csv',header=TRUE) 
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)


#A)Do principal component analysis and factor analysis and identify the dimensions in the data. 

is.na(survey_df) 
sum(is.na(survey_df)) 
sur_int=survey_df[,20:46] 
str(sur_int) 
dim(sur_int) 
library(GPArotation) 
pca <- principal(sur_int,5,n.obs =162, rotate ="promax") 
pca 

om.h<-omega(sur_int,n.obs=162,sl=FALSE) 
op<-par(mfrow=c(1,1)) 
om<-omega(sur_int,n.obs=162) 
library(FactoMineR) 
pca<-PCA(sur_int,scale.unit = TRUE) 
summary(pca) 
biplot(pca, scale = 0) 
str(sur_int) 
dim(sur_int) 
show(sur_int) 

#2nd ObJECTIVE
# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

install_and_load(packages)
survey_df<-read.csv('C:/Users/anjel/Downloads/Survey.csv',header=TRUE)
sur_int=survey_df[,20:46] 


#B) Carry our cluster analysis and characterize the respondents based on their background variables. 
library(cluster) 
library(factoextra) 
show(sur_int) 
fviz_nbclust(sur_int,kmeans,method = "gap_stat") 
set.seed(123) 
km.res<-kmeans(sur_int,4,nstart = 25) 
fviz_cluster(km.res,data=sur_int,palette="jco", 
             ggtheme = theme_minimal()) 
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=4,palette = "jco") 
library(pheatmap) 
pheatmap(t(sur_int),cutree_cols = 4) 



# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv('C:/Users/anjel/Downloads/Survey.csv',header=TRUE) 
sur_int=survey_df[,20:46] 

#Factor Analysis 

factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 


#3rd objective

#C) Do multidimensional scaling and interpret the results. 

icecream_df<-read.csv('C:\\Users\\anjel\\Downloads\\icecream.csv',header=TRUE)
dim(icecream_df)

names(icecream_df) 

ice<-subset(icecream_df,select = -c(Brand)) 
distance_matrix<-dist(ice) 

mds_result<-cmdscale(distance_matrix,k=2) 

plot(mds_result[,1],mds_result[,2],pch=16,xlab="Dimension1",ylab="Dimension2",main="MDS plot") 






# Import required libraries
library(readr)
library(dplyr)
library(stats)
library(ggplot2)

# Read the CSV file
df <- read_csv('C:/Users/anjel/Downloads/pizza_data.csv')

# Fit the model
model <- lm(ranking ~ factor(brand) + factor(price) + factor(weight) + factor(crust) + 
              factor(cheese) + factor(size) + factor(toppings) + factor(spicy), data = df)

# Print model summary
print(summary(model))

conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')
level_name <- list()
part_worth <- list()
part_worth_range <- numeric()
important_levels <- list()
end <- 1  # Initialize index for coefficient in params

for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  level_name[[item]] <- unique(df[[item]])
  
  begin <- end
  end <- begin + nlevels - 1
  
  new_part_worth <- coef(model)[begin:end]
  new_part_worth <- c(new_part_worth, -sum(new_part_worth))
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth[[item]] <- new_part_worth
  print(item)
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}

cat("-------------------------------------------------------------\n")
cat("level name:\n")
print(level_name)
cat("npw with sum element:\n")
print(new_part_worth)
cat("imp level:\n")
print(important_levels)
cat("part worth:\n")
print(part_worth)
cat("part_worth_range:\n")
print(part_worth_range)
print(length(part_worth))
cat("important levels:\n")
print(important_levels)

attribute_importance <- round(100 * (part_worth_range / sum(part_worth_range)), 2)
print(attribute_importance)

part_worth_dict <- list()
attrib_level <- list()

for (i in seq_along(conjoint_attributes)) {
  item <- conjoint_attributes[i]
  cat("Attribute :", item, "\n")
  cat("    Relative importance of attribute ", attribute_importance[i], "\n")
  cat("    Level wise part worths: \n")
  for (j in seq_along(level_name[[item]])) {
    cat("          ", level_name[[item]][j], ":", part_worth[[item]][j], "\n")
    part_worth_dict[[level_name[[item]][j]]] <- part_worth[[item]][j]
  }
  attrib_level[[item]] <- level_name[[item]]
}

# Plot relative importance of attributes
ggplot(data = data.frame(attributes = conjoint_attributes, importance = attribute_importance),
       aes(x = attributes, y = importance)) +
  geom_bar(stat = "identity") +
  labs(title = 'Relative importance of attributes',
       x = 'Attributes',
       y = 'Importance') +
  theme_minimal()

# Calculate utility
utility <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  score <- sum(sapply(conjoint_attributes, function(attr) part_worth_dict[[df[[attr]][i]]]))
  utility[i] <- score
}

df$utility <- utility
print(utility)

for (i in seq_along(conjoint_attributes)) {
  item <- conjoint_attributes[i]
  cat("Preferred level in", item, "is ::", level_name[[item]][important_levels[[item]]], "\n")
}

