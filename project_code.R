
#Set or change the working directory
setwd("C:/Users/gozde/OneDrive/Desktop/stat467-project")
getwd()
options(scipen = 999)  # scientific notation off

# importing necessary library in R
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(bestNormalize)
library(stringr)
library(gridExtra)
library(tidyr)
library(nortest)
library(MVN)
library(MASS)
library(ICSNP)
library(car)
library(ggforce)
library(factoextra)
library(klaR)
library(psych)
library(devtools)
library(GGally)
library(mlbench)


data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")

#######
# exploratory data analysis
dim(data)

# Remove columns with more than 50% NA values since we have columns they are contains many NA values
threshold <- 0.5 * nrow(data)
data <- data[, colSums(is.na(data)) <= threshold]
data<-na.omit(data)


######
summary(data)

###### This summary provides information about variables and their means, quartiles, min and max values.
###### Artist, song, explicit, genre are character. Other ones are numeric.

str(data)

###### This dataset, consists of 2000 observations with 18 variables, including both categorical and
###### numerical data. We can see ausio, genre, and etc.


set.seed(123)
library(dplyr)
data<-data[sample(1:nrow(data), 2000), ]

# Making seperate data for numeric variables. This one will be used for correlation matrix calculation.
num_vars <- data %>% dplyr::select(where(is.numeric))
is.na(num_vars)
sum(is.na(num_vars))


lapply(num_vars, function(col) {
  sum(is.na(col))  # Count NA values
  sum(is.infinite(col))  # Count Inf values
})



###### Boxplot
num_vars2 <- num_vars[, !colnames(num_vars) %in% c("mode", "instrumentalness","year")]
scaled_data <- scale(num_vars2)

par(mar = c(5, 6, 4, 2) + 0.9)  
boxplot_data <- data.frame(scaled_data)  


boxplot(boxplot_data,
        main = "Boxplots of Scaled Variables with Outliers",
        col = "skyblue",
        las = 2,  
        ylab = "Values")

####### Key and valence don't have outliers. Loudness, accoustichness and speechiness have significant outliers.
### Taking out outliers

#Detect and remove outliers using IQR method for each column
# By using a slightly smaller threshold (1.25) instead of (1.5),retaining more data while still addressing extreme deviations.
num_vars <- num_vars %>%
  mutate(across(everything(), ~ {
    q1 <- quantile(., 0.25, na.rm = TRUE)
    q3 <- quantile(., 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.25 * iqr
    upper_bound <- q3 + 1.25 * iqr
    replace(., . < lower_bound | . > upper_bound, NA)
  }))

# Remove rows with any remaining NA values
num_vars <- na.omit(num_vars)



################################
####### Visualizing Categorical Data
################################

# Bar plot showing the count of explicit and non-explicit songs
# Majority of songs in the dataset are non-explicit songs.
ggplot(data, aes(x = explicit, fill = explicit)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Count of Explicit vs Non-Explicit Songs", x = "Explicit", y = "Count")

data <- data %>%
  mutate(genre = str_split(genre, ",") %>% sapply(function(x) trimws(x[1])))

ggplot(data, aes(x = genre, fill = genre)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Count of Genre", x = " ", y = "Count")
# most songs are pop and hip-hop type.

# Stacked bar plot showing the count of explicit and non-explicit songs by genre
ggplot(data, aes(x = genre, fill = explicit)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(title = "Explicit Content Distribution by Genre", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot to show danceability distribution for each genre
ggplot(data, aes(x = genre, y = danceability, fill = genre)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Danceability Distribution by Genre", x = "Genre", y = "Danceability") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels
# Rock has lower danceability. Metal has the lowest danceability score among variables.


##################################
##### Var and Cov (detecting any multicollinearity present or not)
##################################

# Take out zero covariance
zero_variance_vars <- sapply(num_vars, function(x) var(x) == 0)
num_vars <- num_vars[, !zero_variance_vars]

cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
top_vars <- names(sort(apply(cor_matrix, 1, function(x) max(abs(x[x != 1]))), decreasing = TRUE)[1:10])
top_matrix <- cor_matrix[top_vars, top_vars]

corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


abs_cor_matrix <- abs(cor_matrix)

# Set the threshold for high correlation
threshold <- 0.7
high_cor_pairs <- which(abs_cor_matrix > threshold & abs_cor_matrix != 1, arr.ind = TRUE)

#Create a dataframe for highly correlated pairs
high_cor_vars <- data.frame(
  Var1 = rownames(abs_cor_matrix)[high_cor_pairs[, 1]],
  Var2 = colnames(abs_cor_matrix)[high_cor_pairs[, 2]],
  Correlation = cor_matrix[high_cor_pairs]
)

vars_to_remove <- c()

for (i in 1:nrow(high_cor_vars)) {
  if (!(high_cor_vars$Var1[i] %in% vars_to_remove | high_cor_vars$Var2[i] %in% vars_to_remove)) {
    vars_to_remove <- c(vars_to_remove, high_cor_vars$Var2[i])  # Remove Var2 arbitrarily
  }
}

#Remove selected variables from the dataset
filtered_data <- num_vars[, !(colnames(num_vars) %in% vars_to_remove)]

# Step 7: Print the removed variables and the filtered dataset
cat("Removed variables:\n")
print(vars_to_remove)

cat("\nFiltered dataset:\n")
print(filtered_data)
dim(filtered_data)
colnames(filtered_data)
# There is no removed variables so we dont have any multicollinearity problem in our data.

#################
#####heat-map
################

heatmap(cor_matrix, 
        main = "Heatmap of Correlation Matrix",
        col = colorRampPalette(c("blue", "white", "red"))(50),  
        scale = "none",  
        margins = c(8, 8))  
legend("topright", 
       legend = c("Low (Blue)", "Neutral (White)", "High (Red)"), 
       fill = c("blue", "white", "red"), 
       border = "black", 
       bty = "n", 
       cex = 0.8)

# popularity and duration are uncorrelated variables. No multicollinarity problem.

#################
#################
######## star 
#################
#################

num_vars2_clean <- na.omit(num_vars2)
variable_colors <- rainbow(ncol(num_vars2_clean))  


stars(num_vars2_clean[1:20, ],
      main = "Star Plot of Variables",
      labels = rownames(num_vars2_clean)[1:20],  
      col.segments = variable_colors,  
      col.lines = "black",             
      draw.segments = TRUE,            
      key.loc = c(15, 1.5))            

legend("topleft", 
       inset = c(-0.02, 0.4),                   
       legend = colnames(num_vars2_clean),      
       col = variable_colors,                  
       pch = 19,                               
       title = "Variables", 
       cex = 0.8)                   

# For example for 415 orange is quite long , it indicates this song is really popular. Danceability is also high (green). 
# Speechines is short shown as blue.


##################
##################
##### scatter plot
##################
##################

if (is.numeric(data$explicit)) {
  data$explicit <- as.factor(data$explicit)
}

num_vars2_clean <- na.omit(num_vars2)
colnames(num_vars2_clean) <- make.names(colnames(num_vars2_clean), unique = TRUE)
num_vars_clean <- num_vars2_clean[, sapply(num_vars2_clean, is.numeric)]

# take out unneccessary variables
num_vars_clean <- num_vars_clean[, !(colnames(num_vars_clean) %in% c("popularity", "mode", "key"))]


variances <- apply(num_vars_clean, 2, var)
important_vars <- names(sort(variances, decreasing = TRUE)[1:7])

important_data <- num_vars_clean[, important_vars]


plots <- list()
for (var in colnames(important_data)) {
  if (var != "key") { 
    p <- ggplot(data, aes(x = .data[[var]], y = popularity, color = explicit)) +
      geom_point(alpha = 0.6) +
      facet_wrap(~ explicit) +
      labs(
        title = paste("Scatterplot of", var, "vs by Explicit Content"),
        x = var,
        y=""
      ) +
      theme_minimal()
    plots[[var]] <- p
  }
}


grid.arrange(grobs = plots, ncol = 2)

### It shows scatterplots of features versus explicit content. 
# For example, explicit and non-explicit songs show no clear trend on duration of the songs.
# For loudness, explicit songs are often louder than the non-explicit songs. Same as true for energy.
# For accousticness, non-explicit songs show higher accousticness on average.

################
### density plot
# Reshape data for plotting
num_vars_long <- as.data.frame(num_vars) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
num_vars_long_no_year <- num_vars_long %>% filter(Variable != "year")

# Plot density without 'year'
ggplot(num_vars_long_no_year, aes(x = Value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Density Plots of Variables (Excluding Year)", x = "Value", y = "Density")

################
### scatter plot

num_vars_scatter <- num_vars[, !(colnames(num_vars) %in% c("key", "mode", "year"))]
dim(num_vars_scatter)
num_vars_scatter1<-num_vars_scatter[,1:5]
num_vars_scatter2<-num_vars_scatter[,6:11]

# Step 2: Scatterplot Matrix (Pairwise Relationships)
pairs(num_vars_scatter1, 
      main = "Scatterplot Matrix", 
      pch = 20, 
      cex = 0.5, 
      col = "blue")

pairs(num_vars_scatter2, 
      main = "Scatterplot Matrix", 
      pch = 20, 
      cex = 0.5, 
      col = "blue")


###############################################################################
##### Checking if our data is normal or not?
###############################################################################
# Use Shapiro-Wilk test for deciding if data is normal or not 

shapiro_summary <- lapply(as.data.frame(filtered_data), shapiro.test)
str(shapiro_summary)

shapiro_results <- data.frame(
  Variable = names(shapiro_summary), 
  P_Value = sapply(shapiro_summary, function(x) x$p.value), 
  Normal_Distribution = ifelse(sapply(shapiro_summary, function(x) x$p.value) > 0.05, "Yes", "No") 
)

print(shapiro_results)

# so non-normal
# make normal

num_vars <- as.data.frame(
  apply(filtered_data, 2, function(x) ifelse(is.infinite(x), NA, x))
)


numeric_vars <- data[, sapply(data, is.numeric)]
normalized_data <- numeric_vars %>%
  mutate(across(everything(), ~ {
    norm_obj <- bestNormalize(.x, standardize = FALSE)
    norm_obj$x.t
  }))


normal_check <- sapply(normalized_data, function(x) {
  shapiro.test(x)$p.value
})


normal_variables <- names(normal_check[normal_check > 0.05])  
print(normal_variables)

normalized_data_normal <- normalized_data[, normal_variables]
valid_rows <- complete.cases(normalized_data_normal)  

filtered_data <- normalized_data_normal[valid_rows, ]
filtered_genre <- data$genre[valid_rows]


final_data <- filtered_data %>%
  mutate(genre = filtered_genre)

print(head(final_data))


# Install required libraries
if (!require("ICSNP")) install.packages("ICSNP") # For Hotelling's T^2 test
if (!require("energy")) install.packages("energy") # For non-parametric multivariate test

shapiro_summary1 <- data.frame(
  Variable = colnames(normalized_data),
  P_Value = apply(normalized_data, 2, function(x) shapiro.test(x)$p.value)
)
shapiro_summary1$Normal_Distribution <- ifelse(shapiro_summary1$P_Value > 0.05, "Yes", "No")
print(shapiro_summary1)
# Create subsets based on normality
data_normal <- normalized_data[, shapiro_summary1$Normal_Distribution == "Yes"]
data_non_normal <- normalized_data[, shapiro_summary1$Normal_Distribution == "No"]

# Check dimensions
cat("Normal Data Dimensions:", dim(data_normal), "\n")
cat("Non-Normal Data Dimensions:", dim(data_non_normal), "\n")
dim(normalized_data)
head(data_non_normal)
head(data_normal)

#################################################################################
##### making Hotelling T^2 test for normal data
ad_results <- lapply(normalized_data, ad.test)

data_normal <- data_normal[, !names(data_normal) %in% "loudness"]

summary(data_normal)


## check again if it is normal or not
result <- mvn(data = data_normal, mvnTest = "royston")
result$multivariateNormality

# create univariate Q-Q plots
result <- mvn(data = data_normal, mvnTest = "royston", univariatePlot = "qqplot")
# create univariate histograms
result <- mvn(data = data_normal, mvnTest = "royston", univariatePlot = "histogram", univariateTest = "SW" )


HotellingsT2(data_normal,mu=colMeans(data_normal))
# fail to reject null hyp.

###################
# Creating CI for normal data
###################

colnames(data_normal)
colnames(num_vars)

setdiff(colnames(data_non_normal), colnames(num_vars))
setdiff(colnames(num_vars), colnames(data_non_normal))
common_columns1 <- intersect(colnames(data_normal), colnames(num_vars))
dt1 <- num_vars[, common_columns1]
set.seed(123)  
str(dt1)
dt1 <- as.data.frame(lapply(dt1, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
}))
bootstrap_mean_ci <- function(data, n_bootstrap = 1000, alpha = 0.05) {
  if (!is.numeric(data)) {
    stop("Data must be numeric.")
  }
  data <- na.omit(data)  # Eksik de??erleri kald??r
  boot_means <- replicate(n_bootstrap, mean(sample(data, replace = TRUE)))
  lower <- quantile(boot_means, probs = alpha / 2, na.rm = TRUE)
  upper <- quantile(boot_means, probs = 1 - alpha / 2, na.rm = TRUE)
  list(mean = mean(data, na.rm = TRUE), lower_ci = lower, upper_ci = upper)
}



results <- lapply(as.data.frame(dt1), bootstrap_mean_ci)
print(results)


tidy_results <- do.call(rbind, lapply(names(results), function(var) {
  data.frame(
    Variable = var,
    Mean = results[[var]]$mean,
    Lower_CI = results[[var]]$lower_ci,
    Upper_CI = results[[var]]$upper_ci
  )
}))

print(tidy_results)
library(openxlsx)
# write.xlsx(tidy_results,"normal_bon11.xlsx")

tidy_results$CI_Range <- (tidy_results$Upper_CI - tidy_results$Lower_CI) / 2


par(mfrow = c(3, 3))
for (i in 1:nrow(tidy_results)) {
  variable <- tidy_results$Variable[i]
  if (variable %in% c("year", "instrumentalness")) {
    next
  }
  
  
  mean_val <- tidy_results$Mean[i]
  lower_ci <- tidy_results$Lower_CI[i]
  upper_ci <- tidy_results$Upper_CI[i]
  
  if (!is.na(mean_val) && !is.na(lower_ci) && !is.na(upper_ci) && lower_ci < upper_ci) {
    graph_lower <- lower_ci - 0.01  
    graph_upper <- upper_ci + 0.01 
    plot(
      x = 1, 
      y = mean_val, 
      pch = 19, 
      col = "darkred", 
      xlim = c(0.8, 1.2), 
      ylim = c(graph_lower, graph_upper),  
      xlab = "",
      ylab = "Value",
      main = paste("Confidence Interval for", variable),
      cex.main = 1.2, 
      cex.axis = 0.9  
    )
    segments(x0 = 1, y0 = lower_ci, x1 = 1, y1 = upper_ci, col = "steelblue", lwd = 2)
    text(x = 1.1, y = mean_val, labels = round(mean_val, 2), col = "darkred", cex = 0.9)
  } else {
    message(paste("Skipped:", variable, "due to invalid CI values."))
  }
}

par(mfrow = c(1, 1))


##########################################################
########### MANOVA
##########################################################

# RQ: Is there a relationship between Genre and other normal data variables?

manova_dependent_vars <- as.matrix(final_data[, !names(final_data) %in% "genre"])
manova_result <- manova(manova_dependent_vars ~ genre, data = final_data)
summary(manova_result)
### So there is a significant relationship between genre and other variables
### So deciding which ones create differences
### Use pairwise t test - Post-hoc analysis

posthoc_results <- summary.aov(manova_result)
print(posthoc_results)

### genre has an impact on duration,energy,speechiness, acousticness,tempo
### has not impact on  liveness and  valence

# RQ: Is there a relationship between explicit and other normal data variables

numeric_vars <- data[, sapply(data, is.numeric)]
normalized_data <- numeric_vars %>%
  mutate(across(everything(), ~ {
    norm_obj <- bestNormalize(.x, standardize = FALSE)
    norm_obj$x.t
  }))

normal_check <- sapply(normalized_data, function(x) {
  shapiro.test(x)$p.value
})

normal_variables <- names(normal_check[normal_check > 0.05])  
print(normal_variables)

normalized_data_normal <- normalized_data[, normal_variables]
valid_rows <- complete.cases(normalized_data_normal)  

filtered_data <- normalized_data_normal[valid_rows, ]
filtered_explicit <- data$explicit[valid_rows]
final_data <- filtered_data %>%
  mutate(explicit = filtered_explicit)
print(head(final_data))


manova_dependent_vars <- as.matrix(final_data[, !names(final_data) %in% "explicit"])

manova_result <- manova(manova_dependent_vars ~ explicit, data = final_data)

summary(manova_result)

## so there is a differences between explicit and other normal data variables
# to decide which ones creates differences look at post hoc test

posthoc_results <- summary.aov(manova_result)
print(posthoc_results)


### according to explicite; duration, energy, speecchiness,valence creates differences
### doesn't create any differences accousticness,liveness,tempo 



###############################################################################
################## Multiple Linear Regression
###############################################################################

# Y= genre and explicit
# X= significant for both genre and explicit


data$genre <- as.factor(data$genre)
data$explicit <- as.factor(data$explicit)
numeric_vars <- data[, sapply(data, is.numeric)]
normalized_data <- numeric_vars %>%
  mutate(across(everything(), ~ {
    norm_obj <- bestNormalize(.x, standardize = FALSE)
    norm_obj$x.t
  }))

normal_check <- sapply(normalized_data, function(x) {
  shapiro.test(x)$p.value
})

normal_variables <- names(normal_check[normal_check > 0.05]) 
print(normal_variables)



normalized_data_normal <- normalized_data[, normal_variables]
valid_rows <- complete.cases(normalized_data_normal)  

filtered_data <- normalized_data_normal[valid_rows, ]
filtered_genre <- data$genre[valid_rows]
filtered_explicit<-data$explicit[valid_rows]
final_data <- filtered_data %>%
  mutate(genre = filtered_genre , explicit=filtered_explicit)

final_data$genre <- sapply(strsplit(as.character(final_data$genre), ","), function(x) trimws(x[1]))
final_data$genre <- as.factor(final_data$genre)
table(final_data$genre)
print(head(final_data))
significant_numeric_vars <- dplyr::select(final_data, duration_ms, energy, speechiness, valence)
independent_vars <- dplyr::select(final_data, genre, explicit)


# Multivariate Linear Regression
model <- lm(as.matrix(significant_numeric_vars) ~ genre + explicit, data = final_data)
summary(model)

head(resid(model))
coef(model)
sigma(model)

#################################################################################
#################################################################################
#################################################################################
#################### PCA - PCA REGRESSION
#################################################################################
#################################################################################
#################################################################################

num1<-data %>% dplyr::select(where(is.numeric))
threshold <- 0.5 * nrow(num1)
num1 <- num1[, colSums(is.na(num1)) <= threshold]
cov(num1)
num1<-na.omit(num1)
my_data1<-num1
summary(num1)
zero_variance_vars <- sapply(num1, function(x) var(x) == 0)
num1 <- num1[, !zero_variance_vars]
cor_matrix <- cor(num1)
top_vars <- names(sort(apply(cor_matrix, 1, function(x) max(abs(x[x != 1]))), decreasing = TRUE)[1:13])
top_matrix <- cor_matrix[top_vars, top_vars]
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

num1 <- as.data.frame(num1)

num1 <- num1[, !(names(num1) %in% "year")]
num1<-scale(num1)
pca1 <- prcomp(num1)
summary(pca1)
names(pca1)
pca1$rotation
pca1$x
pca1$sdev
fviz_eig(pca1,addlabels=TRUE,ncp=13) #represent the proportion values
pca<-pca1$x[,1:13] # We obtain 13 pca, we took first 10 pca
head(pca)


res1 <- cor(pca, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust")
cor(my_data1,pca) # linear
biplot(pca1, col = c("gray", "black"))


fviz_pca_var(pca1, select.var = list(contrib = 13))

#### on pc1= acousticness, loudness,energy, valence
#### on pc2= speechiness,danceability,liveness,tempo has effect

#### Variables near the edge of the circle are strongly represented in the PCA dimensions.
# Variables near the center have a weaker contribution.

### Acousticness and loudness/energy appear to oppose each other 
# (arrows point in opposite directions), indicating that tracks with high
#acousticness tend to have lower loudness/energy


#PC2 seems to capture rhythmic and lyrical aspects, representing a spectrum
#from speech-heavy, danceable tracks (high speechiness and danceability)
#to tracks with less rhythmic intensity.

fviz_pca_var(pca1, col.var = "contrib") # darker one gives high contribution


fviz_pca_ind(pca1, col.ind = "#00AFBB")

#Most points are clustered near the center, indicating that many individuals do 
#not strongly differentiate along the first two principal components.

#Points further away from the center represent individuals that are more 
#distinct based on the PCA dimensions.
# Select top 100 individuals based on distance from the origin


p <- fviz_contrib(pca1, choice = "ind", axes = 1:2) + coord_flip()
p + theme(axis.text.y = element_blank(), # Y eksenindeki etiketleri kald??r
          axis.ticks.y = element_blank())  # Y eksenindeki i??aretleri kald??r


fviz_pca_ind(pca1, label="none", habillage=my_data1$mode,
             addEllipses=TRUE, ellipse.level=0.95)

##This plot highlights that only a small subset of individuals 
#(to the right of the red line) has a significant impact on the
#first two principal components, while the majority have minimal contributions. 


#################################################################################
#################### PCA REGRESSION
#################################################################################

data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")
# Remove columns with more than 50% NA values since we have columns they are contains many NA values
set.seed(123)
library(dplyr)
library(corrplot)
num1<-data %>% dplyr::select(where(is.numeric))
threshold <- 0.5 * nrow(num1)
num1 <- num1[, colSums(is.na(num1)) <= threshold]
cov(num1)
num1<-na.omit(num1)
my_data1<-num1
summary(num1)

num1 <- as.data.frame(num1)
num1 <- select(num1, -c( "year"))
num1<-scale(num1)
pca1 <- prcomp(num1)
summary(pca1)
names(pca1)
pca1$rotation
pca1$x
pca1$sdev
fviz_eig(pca1,addlabels=TRUE) #represent the proportion values
dependent_data <- cbind(data$duration_ms, data$energy,data$speechiness,data$valence)

not_columns <- c("duration_ms", "energy", "year", "key","speechiness","valence")


X_data <- data[, !(names(data) %in% not_columns)]
X_data <- X_data[, sapply(X_data, is.numeric)]

pca1 <- prcomp(X_data, scale. = TRUE)
summary(pca1)
pca1_data <- as.data.frame(pca1$x[, 1:7])  


ols_data <- cbind(dependent_data, pca1_data)
colnames(ols_data)[1:4] <- c("duration_ms","energy","speechiness","valence")  


ols_data <- na.omit(ols_data)
lmodel <- lm(cbind(duration_ms,energy,speechiness,valence) ~ ., data = ols_data)
summary(lmodel)


################################################################################
################################################################################
########################### FACTOR ANALYSIS AND FACTOR ROTATION
################################################################################
################################################################################


data <- read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")
fca_data <- data[complete.cases(data), ]
fca_data <- fca_data[, !(names(fca_data) %in% c("year", "key"))]

data_numeric <- fca_data[, sapply(fca_data, is.numeric)]
cm <- cor(data_numeric, method = "pearson")
# cor matrix
corrplot(cm, method = "number", order = "hclust")

# KMO test
library(psych)
kmo_result <- KMO(r = cm)
print(kmo_result)

# Bartlett test
bartlett_result <- cortest.bartlett(cm, nrow(data_numeric))
print(bartlett_result)

# Paralel analysis
parallel <- fa.parallel(data_numeric, fm = "minres", fa = "fa")

#Parallel analysis suggests that the number of factors =6
#This shows that after factor 6 the total variance accounts for smaller amounts.
#we will use factanal function and test the null hypothesis that 6 factors are sufficient. 
parallel <- fa.parallel(data_numeric, fm = "minres", fa = "fa")
print(parallel$fa.values)  
print(parallel$fa.sim)    

factanal(data_numeric, factors = 4,method="mle")$PVAL 
factanal(data_numeric, factors = 5,method="mle")$PVAL
factanal(data_numeric, factors = 6,method="mle")$PVAL
factanal(data_numeric, factors = 7,method="mle")$PVAL  

#since p value is greater than 0.05, we fail to reject null hypothesis 
#and decide on that 7 factor solution is adequate.

names(f$loadings[,1])[abs(f$loadings[,1])>0.4]
f1<-fca_data[,names(f$loadings[,1])[abs(f$loadings[,1])>0.4]]
summary(alpha(f1, check.keys=TRUE))


fca_data_numeric <- fca_data[, sapply(fca_data, is.numeric)]


scores <- factanal(fca_data_numeric, factors = 7, scores = "regression")$scores
fca_data<-fca_data_numeric

scores<-factanal(fca_data, factors = 7,scores="regression")$scores
head(scores)
cm1 <- cor(scores, method="pearson")
corrplot::corrplot(cm1, method= "number", order = "hclust")

f<-factanal(data_numeric, factors = 7) # explain 41% of variances
f

################################################################################
###################### Factor Rotation
################################################################################
fa_varimax <- factanal(data_numeric, factors = 7, rotation = "varimax")
print(fa_varimax)

fa_varimax <- factanal(data_numeric, factors = 7, rotation = "promax")
print(fa_varimax)


#we can see that the first 7 factors explain almost 42% of the variance. 
#We can effectively reduce dimensionality from 18 to 7.

load <- f$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(fca_data),cex=.7)

### alpha analysis

for (i in 1:7) { 
  factor_items <- names(f$loadings[, i])[abs(f$loadings[, i]) > 0.2]  
  if (length(factor_items) > 1) {  
    factor_data <- fca_data[, factor_items]  
    alpha_result <- alpha(factor_data, check.keys = TRUE)  
    print(paste("Factor", i, "Alpha Analysis:"))
    print(summary(alpha_result))  
  } else {
    print(paste("Factor", i, "has less than 2 items. Alpha analysis skipped."))
  }
}

################################################################################
################### Fisher Discriminant Analysis
################################################################################

# Linearity and multicollinearity assumption was cheecked above.
# Assumes population covariances are equal to continu the FDA.


# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))


library(ggord)
library(GGally)

data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")
# Remove columns with more than 50% NA values since we have columns they are contains many NA values
threshold <- 0.5 * nrow(data)
data <- data[, colSums(is.na(data)) <= threshold]
data<-na.omit(data)
data<-as.factor(data)


# Make genre usable 
data$genre <- sapply(strsplit(as.character(data$genre), ","), function(x) trimws(x[1]))
data$genre <- as.factor(data$genre)
table(data$genre)
data$explicit<-as.factor(data$explicit)
data <- data[, !(names(data) %in% c("artist", "song", "year", "key","explicit"))]
na.omit(data)


num_vars <- data %>% dplyr::select(where(is.numeric))
is.na(num_vars)
sum(is.na(num_vars))


lapply(num_vars, function(col) {
  sum(is.na(col))  # Count NA values
  sum(is.infinite(col))  # Count Inf values
})


GGally::ggpairs(data)
pairs.panels(data[,1:5],gap=0,pch=21)


data <- na.omit(data) 
data <- data[data$genre %in% names(which(table(data$genre) > 2)), ]


ggpairs(data[, 1:5],
        aes(color = data$genre, alpha = 0.7),
        title = "Pair Plot ") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, face = "bold"))

#make this example reproducible
set.seed(2)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train <- data[sample, ]
test <- data[!sample, ] 


model <- lda(genre~.,data = train)
model

plot(model)

model.values <- predict(model)
names(model.values)

# for multivariate
ggord(model, train$genre, 
      xlim = c(-8, 8),  # Adjust horizontal range
      ylim = c(-10, 10)  # Adjust vertical range
) +
  labs(title = "LDA Biplot: Genre Discrimination") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))



# train performance

train_predict<- predict(model,train)$class
table_train <- table(Predicted =train_predict, Actual = train$genre)
table_train
sum(diag(table_train))/sum(table_train)
# misclassification rate 1-0.64=0.36

# test performance
test_predict<- predict(model,test)$class
table_test<- table(Predicted =test_predict, Actual = test$genre)
table_test

sum(diag(table_test))/sum(table_test)
#1-0.6=0.3





################################################################################
# for explicit 
data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")



# Remove columns with more than 50% NA values since we have columns they are contains many NA values
threshold <- 0.5 * nrow(data)
data <- data[, colSums(is.na(data)) <= threshold]
data<-na.omit(data)
data<-as.factor(data)
data$explicit <- as.factor(data$explicit)
table(data$explicit)

data <- data[, !(names(data) %in% c("artist", "song", "year", "key","genre"))]
na.omit(data)
num_vars <- data %>% dplyr::select(where(is.numeric))
is.na(num_vars)
sum(is.na(num_vars))


lapply(num_vars, function(col) {
  sum(is.na(col))  # Count NA values
  sum(is.infinite(col))  # Count Inf values
})


GGally::ggpairs(data)
pairs.panels(data[,1:5],gap=0,pch=21)
data <- na.omit(data)  

#make this example reproducible
set.seed(2)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train <- data[sample, ]
test <- data[!sample, ] 


model <- lda(explicit~.,data = train)
model

plot(model)

model.values <- predict(model)
names(model.values)
# two variate since one ld exists
partimat(as.factor(explicit)~duration_ms+speechiness+energy+liveness,data=data,method="lda") 


# train performance

train_predict<- predict(model,train)$class
table_train <- table(Predicted =train_predict, Actual = train$explicit)
table_train
sum(diag(table_train))/sum(table_train)
# misclassification rate 1-0.80=0.2

# test performance
test_predict<- predict(model,test)$class
table_test<- table(Predicted =test_predict, Actual = test$explicit)
table_test

sum(diag(table_test))/sum(table_test)
#1-0.78=0.22


##########################################################
###########################################################
###########################################################
#################### cluster 
#########################################################
##########################################################
#########################################################

library(cluster)
library(factoextra)


data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")

# Remove columns with more than 50% NA values since we have columns they are contains many NA values
threshold <- 0.5 * nrow(data)
data <- data[, colSums(is.na(data)) <= threshold]
data<-na.omit(data)

num_vars <- data %>% dplyr::select(where(is.numeric))
is.na(num_vars)
sum(is.na(num_vars))


lapply(num_vars, function(col) {
  sum(is.na(col))  # Count NA values
  sum(is.infinite(col))  # Count Inf values
})

#Detect and remove outliers using IQR method for each column
num_vars <- num_vars %>%
  mutate(across(everything(), ~ {
    q1 <- quantile(., 0.25, na.rm = TRUE)
    q3 <- quantile(., 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.25 * iqr
    upper_bound <- q3 + 1.25 * iqr
    replace(., . < lower_bound | . > upper_bound, NA)
  }))

# Remove rows with any remaining NA values
num_vars <- na.omit(num_vars)
set.seed(4)
sample_data<-data[sample(1:nrow(data), 30), ]
dim(sample_data)
dm<-dist(sample_data[,c("duration_ms","speechiness","energy","valence")])
dm

par(mfrow=c(2,2),mar=c(1,2,1,2))
plot(cs <- hclust(dm, method = "single"),main = "Single Linkage")
plot(cc <- hclust(dm, method = "complete"),main = "Complete Linkage")
plot(ca <- hclust(dm, method = "average"),main = "Average Linkage")
plot(cw <- hclust(dm, method = "ward.D2"),main = "Ward Method") # use agglomeration method "ward"

# Single linkage <- It is not ideal for our data since it fails to create compact.
# Complete Linkage <- 684, 307 and 1952 merge together.
# Average Linkage <- 767 and 1711 are merge together.
# Ward's Method <- 303, 1827, 1086 and 1200 are merge together.

par(mfrow=c(1,1))


res.diana <- diana(sample_data[, c("duration_ms","speechiness","energy","valence")], stand = TRUE)

# Plot the dendrogram
fviz_dend(res.diana, cex = 0.5,
          k = 6, # Cut in four groups
          palette = "jco" # Color palette
)

### k - means clustering
data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")
data <- data[, !(names(data) %in% c("artist", "song", "explicit", "year", "key", "mode", "genre"))]

sapply(data, var)
# variances are very different we need to make standardization

standard_data<-sapply(data,function(x) diff(range(x)))
standard_data_1<-sweep(data,2,standard_data,FUN="/")
sapply(standard_data_1,var)
# know they are close to each other


# to get idea about number of clusters we need to look at
# below plot

n <- nrow(standard_data_1)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(sapply(standard_data_1, var))
for (i in 2:6){
  wss[i] <- sum(kmeans(standard_data_1,centers = i)$withinss)
}
plot(1:6, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")


# elbow accours for 3 cluesters
kmeans(standard_data_1, centers = 3)$centers * standard_data
kmeans(standard_data_1, centers = 3)$cluster
library(ggplot2)

aggregate(standard_data_1, by = list(cluster = kmeans(standard_data_1, centers = 3)$cluster), mean)




# Visualize clusters
fviz_cluster(kmeans(standard_data_1, centers = 3), data = standard_data_1, geom = "point", stand = FALSE) +
  theme_minimal()

# cluster 1: It is seperated from other significantly so this cluster has distinct characteristics.

######################################################
###### canonical correlation analysis
#####################################################

# addume multivariate normal distribution.

data<-read.csv("C:/Users/gozde/OneDrive/Desktop/stat467-project/songs_normalize.csv")

# Remove columns with more than 50% NA values since we have columns they are contains many NA values
threshold <- 0.5 * nrow(data)
data <- data[, colSums(is.na(data)) <= threshold]
data<-na.omit(data)

num_vars <- data %>% dplyr::select(where(is.numeric))
is.na(num_vars)
sum(is.na(num_vars))


lapply(num_vars, function(col) {
  sum(is.na(col))  # Count NA values
  sum(is.infinite(col))  # Count Inf values
})

#Detect and remove outliers using IQR method for each column
num_vars <- num_vars %>%
  mutate(across(everything(), ~ {
    q1 <- quantile(., 0.25, na.rm = TRUE)
    q3 <- quantile(., 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.25 * iqr
    upper_bound <- q3 + 1.25 * iqr
    replace(., . < lower_bound | . > upper_bound, NA)
  }))

# Remove rows with any remaining NA values
num_vars <- na.omit(num_vars)
summary(data)
summary(num_vars)

# two group 
# first one significant variables for both genre and explicit
# first group: "duration_ms","speechiness","energy","valence"
# second group: danceability, tempo, popularity,accousticness


# Install and load the necessary package
library(CCA)
library(reshape2)

# Subset the two groups of variables
group1 <- num_vars[, c("duration_ms", "speechiness", "energy", "valence")]
group2 <- num_vars[, c("danceability", "tempo", "popularity", "acousticness")]

# Perform Canonical Correlation Analysis
cca_result <- cancor(group1, group2)

# Display canonical correlations
print(cca_result$cor)

# Display canonical coefficients
print(cca_result$xcoef)  # Coefficients for Group 1

#For Canonical Variable 1:
#Speechiness (-0.2016) and energy (0.1820) contribute the most to the first canonical variable.
#Speechiness contributes negatively, suggesting a negative relationship with the canonical variable.
#Energy contributes positively, indicating a positive relationship with the canonical variable.
#Duration_ms has a very small coefficient, indicating it has almost no influence.

#For Canonical Variable 2:
# Energy (-0.1806) is the most important variable, contributing negatively.
#Speechiness (-0.0465) has a smaller contribution but is still relevant.
#Other variables have negligible influence.

#For Canonical Variables 3 and 4:
#The coefficients are much smaller, indicating these canonical variables explain weak relationships (as the corresponding canonical correlations are also weak: 0.0165 and 0.061).
# Except speechiness.

print(cca_result$ycoef)  # Coefficients for Group 2
#For Canonical Variable 1:
#Danceability (-0.2407) contributes most, with a strong negative relationship.
#Acousticness (-0.0810) contributes negatively as well.
#Tempo and popularity have very small coefficients, suggesting negligible contributions.

#For Canonical Variable 2:
# Acousticness (0.2446) contributes most, with a strong positive relationship.
#Danceability (-0.0951) contributes negatively.
#Tempo and popularity remain minor contributors.

#For Canonical Variables 3 and 4:
#The coefficients are small for all variables, indicating weak contributions to these canonical variables.
# Except for acousticness

# Convert group1 to a numeric matrix
group1_matrix <- as.matrix(group1)

# Perform the matrix multiplication
canonical_group1 <- as.data.frame(group1_matrix %*% cca_result$xcoef)

# Convert group1 to a numeric matrix
group2_matrix <- as.matrix(group2)

# Perform the matrix multiplication
canonical_group2 <- as.data.frame(group2_matrix %*% cca_result$ycoef)



# Scatter plot of the first pair of canonical variables
# Strong positive cc between the 2 sets of variables. (but not perfect since there is some dispersion.)
plot(canonical_group1[,1], canonical_group2[,1], 
     xlab = "Canonical Variable 1 (Group 1)", 
     ylab = "Canonical Variable 1 (Group 2)", 
     main = "Canonical Correlation Analysis",
     col = "purple", pch = 19)
# it is moderate cca relation

pairs(canonical_group1, col = "blue", main = "Canonical Variables: Group 1")
pairs(canonical_group2, col = "red", main = "Canonical Variables: Group 2")

barplot(cca_result$xcoef[, 1], main = "Contributions to Canonical Variable 1 (Group 1)", col = "cyan")
barplot(cca_result$ycoef[, 1], main = "Contributions to Canonical Variable 1 (Group 2)", col = "pink")




cor_matrix <- cor(canonical_group1, canonical_group2)
ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Canonical Correlation Heatmap", x = "Group 1", y = "Group 2")


######################################################################
###############################################
# Rename columns for clarity
colnames(canonical_group1) <- "Canonical_Var_Group1"
colnames(canonical_group2) <- "Canonical_Var_Group2"

# Combine the data frames with unique column names
combined_data <- cbind(canonical_group1, canonical_group2)

# Scatterplot with density overlay
library(ggplot2)
ggplot(data = combined_data, aes(x = Canonical_Var_Group1, y = Canonical_Var_Group2)) +
  geom_point(color = "purple", size = 2, alpha = 0.6) +
  geom_density2d(color = "orange", size = 0.8) +
  labs(title = "Canonical Correlation: Scatterplot with Density Overlay",
       x = "Canonical Variable 1 (Group 1)",
       y = "Canonical Variable 1 (Group 2)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



