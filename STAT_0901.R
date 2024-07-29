######## INSTALL NEEDED PACKAGE AND LIBRARIES ########

install.packages("gplots")
install.packages("lmtest")

library(lmtest)
library(carData)
library(car)


######## IMPORT DATASET ######## 

# C:/Users/diogo/Desktop/Statistics Project/player_stats.csv
data <- read.csv(file = "C:/Users/Utilizador/Desktop/MASTER/STAT/project/player_stats.csv", 
                 header = TRUE, sep = ",")

dim(data)
names(data)
str(data)

######## DATA PREPOCCESSING ######## 

unique(data$marking)

# REMOVE THE GK VARIABLES
sort(unique(data$gk_positioning))
length(data$gk_positioning > 30)
data <- subset(data, gk_positioning <= 30)

# Remove variables that makes sense to don't influence the market value of a player
data <- subset(data,select=-c(player,country,club,marking,gk_positioning,gk_diving,gk_handling,gk_kicking,gk_reflexes))

# Remove dollar sign, extra spaces, and dots, then convert to integer
for (i in 1:length(data$value)) {
  last_three <- substring(data$value[i], nchar(data$value[i]) - 3)
  if (last_three == ".00 ") {
    data$value[i] <- sub(".00 ", "000", data$value[i])
  }
}
data$value <- as.integer(gsub("[\\$\\.]", "", gsub("\\s", "", substring(data$value, 2))))
head(data$value)

# Converting the market value to its logarithm
data$value <- log(data$value)

######## CORRELATION MATRIX ######## 

# Correlation matrix to see relationships between variables
correlation_matrix <- cor(data)
variables <- rownames(correlation_matrix)
num_vars <- length(variables)
for (i in 1:(num_vars - 1)) {
     for (j in (i + 1):num_vars) {
         if (correlation_matrix[i, j] > 0.8) {
             cat("Variables", variables[i], "and", variables[j], "have a correlation above 0.8:", abs(correlation_matrix[i, j]), "\n")
         }
     }
}
data <- subset(data,select=-c(dribbling,short_pass,slide_tackle,stand_tackle,att_position,acceleration,long_shots,volleys))

correlation_matrix <- cor(data)
# Correlation with the target variable
cor_with_target <- correlation_matrix["value", ]

abs_cor_with_target <- abs(cor_with_target)
sorted_correlations <- sort(abs_cor_with_target, decreasing = TRUE)

# Display top correlated variables
top_correlated_vars <- names(sorted_correlations[sorted_correlations > 0.3])
top_correlated_vars

data <- data[, top_correlated_vars]

######## REGRESSION ANALYSIS ######## 

# Let's start with all variables
# We are performing a t-test for all variables

reg <- lm(value~reactions+ball_control+composure+long_pass+vision+shot_power+curve+crossing+stamina+finishing+
                fk_acc+heading+penalties+aggression+agility, data=data)

summary(reg)

# If p_value > 0.01, we are rejecting H0 (H0: beta = 0 vs H1: beta != 0)
# If reject H0, we are initially assuming that the variable is important to explain our model


# Performing a test only with the non-rejected variables
reg2 <- lm(value~reactions+ball_control+composure+vision+crossing+stamina+
                  fk_acc+heading+penalties+agility, data=data)

summary(reg2)

# p_value ~ 0, so we are rejecting any variables that is important to estimate our model

######## BREUSCH-PAGAN TEST ######## 

bptest(reg2)

# p-value ~ 0
# WRTITE CONCLUSION

######## SPECIAL-WHITE TEST ######## 

bptest(resid(reg2)^2 ~ fitted(reg2) + I(fitted(reg2)^2) )

# p-value = 0.1927
# Reject H0 for all other usual significance levels
# There is statistical evidence of heteroskedasticity

######## ROBUST ESTIMATION ######## 

hccm <- vcov(reg2)
robust <- coeftest(reg2, vcov=hccm)
robust

# All p-values are lower then every usual level of significance
# So, we don't reject any variable
# Then we specified the restricted model is reg1

summary(reg2)


######## RESET TEST ######## 

RESETreg <- lm(value~reactions+ball_control+composure+vision+crossing+stamina+
                 fk_acc+heading+penalties+agility+
                 I(fitted(reg2)^2)+I(fitted(reg2)^3), data=data)
RESETreg

linearHypothesis(RESETreg, matchCoefs(RESETreg,"fitted"))

# p-value ~ 0 which means we reject the null hypotesis
# So, the previous model was misspecified

######## PLOTS ########

scatterplot(value ~ reactions, data = data)
scatterplot(value ~ ball_control, data = data)
scatterplot(value ~ composure, data = data)
scatterplot(value ~ vision, data = data)
scatterplot(value ~ crossing, data = data)
scatterplot(value ~ stamina, data = data)
scatterplot(value ~ fk_acc, data = data)
scatterplot(value ~ heading, data = data)
scatterplot(value ~ penalties, data = data)
scatterplot(value ~ agility, data = data) # este está fraco -> combinar com outra?

######## DEFINE THE FINAL MODEL ######## 

finalmodel_res <- lm(value~reactions+ball_control+composure+vision+crossing+stamina+
                       fk_acc+heading+penalties+agility+I(fitted(reg2)^2)+I(fitted(reg2)^3)
                     +I(crossing*ball_control)+I(agility*reactions)+I(crossing*vision), data=data)

linearHypothesis(finalmodel_res, matchCoefs(finalmodel_res,"fitted"))
#p-value=0.2985

summary(finalmodel_res)

######## DATA VISUALIZATION ######## 

### we are gonna plot 3 times because theres too much graphs
# the target variable has values too big compared to every feature so lets do 
#log-level for now

# Check for missing values in the dataset
missing_counts <- colSums(is.na(data))
missing_counts


# Set the outer margins and axis label margins
par(mar = c(3, 3, 1, 1) + 0.1)  # Outer margins
par(mgp = c(2, 1, 0))  # Axis label margins
# Set the overall size of the plotting device
options(repr.plot.width = 8, repr.plot.height = 15)

features <- setdiff(names(data), "value")

# Select only the first 15 features
selected_features <- features[1:15]
# Calculate the number of rows and columns for the plotting layout
num_cols <- 2  # Adjust the number of columns as needed
num_rows <- ceiling(length(selected_features) / num_cols)

# Create a multi-paneled plot
par(mfrow = c(num_rows, num_cols))

for (feature in selected_features) {
  # Check if the feature is numeric
  if (is.numeric(data[[feature]])) {
    # Set explicit limits for the x-axis
    xlim <- c(min(data[[feature]]), max(data[[feature]]))
    
    plot(data[[feature]], data$value,
         main = paste("Scatter Plot of", feature, "vs Target Variable"),
         xlab = feature,
         ylab = "Target Variable",
         xlim = xlim)
  }
}

#features that look relevant from 1 plot:
# - ball_control, dribbling, slide_tackle, stand_tackle
# - reactions, att_position, interceptions

# plot_2
selected_features <- features[16:26]
# Calculate the number of rows and columns for the plotting layout
num_cols <- 2  # Adjust the number of columns as needed
num_rows <- ceiling(length(selected_features) / num_cols)

# Create a multi-paneled plot
par(mfrow = c(num_rows, num_cols))

for (feature in selected_features) {
  # Check if the feature is numeric
  if (is.numeric(data[[feature]])) {
    # Set explicit limits for the x-axis
    xlim <- c(min(data[[feature]]), max(data[[feature]]))
    
    plot(data[[feature]], data$value,
         main = paste("Scatter Plot of", feature, "vs Target Variable"),
         xlab = feature,
         ylab = "Target Variable",
         xlim = xlim)
  }
}
#features that look the most relevant from 2 plot:
# - vision, composure, crossing, short_pass, long_pass
# 

#plot_3
selected_features <- features[27:length(features)]
# Calculate the number of rows and columns for the plotting layout
num_cols <- 2  # Adjust the number of columns as needed
num_rows <- ceiling(length(selected_features) / num_cols)

# Create a multi-paneled plot
par(mfrow = c(num_rows, num_cols))

for (feature in selected_features) {
  # Check if the feature is numeric
  if (is.numeric(data[[feature]])) {
    # Set explicit limits for the x-axis
    xlim <- c(min(data[[feature]]), max(data[[feature]]))
    
    plot(data[[feature]], data$value,
         main = paste("Scatter Plot of", feature, "vs Target Variable"),
         xlab = feature,
         ylab = "Target Variable",
         xlim = xlim)
  }
}
#features that look the most relevant from 3 plot:
# - heading , shot_power, finishing, long_shots,
# - curve, fk_acc, penalties, volleys, 

#for gk all seem reasonable

##lets do now the categorical ones
barplot(tapply(data$value, data$country, mean),
        main = "Bar Plot of Country vs Target Variable",
        xlab = "Country",
        ylab = "Value",
        col = "skyblue")

barplot(tapply(data$value, data$club, mean),
        main = "Bar Plot of Country vs Target Variable",
        xlab = "Club",
        ylab = "Value",
        col = "skyblue")

barplot(tapply(data$value, data$marking, mean),
        main = "Bar Plot of Country vs Target Variable",
        xlab = "marking",
        ylab = "Value",
        col = "skyblue")

#there is no importance on the cat feautes so lets drop them
data <- data[, !(names(data) %in% c('country', 'club', 'marking'))]
str(data)

# Reset the plotting layout
par(mfrow = c(1, 1))

# Print the result
print(missing_counts) # there is no missing values


######## BRINCAR #####

#install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(value~reactions+composure+short_pass+shot_power+vision+ball_control+long_pass+
                           dribbling+curve+long_shots, data=data, importance = TRUE)

importance <- importance(rf_model)
var_importance <- data.frame(Variables = rownames(importance), Importance = round(importance[ , "%IncMSE"], 2))
var_importance <- var_importance[order(-var_importance$Importance), ]
print(var_importance)

top_correlated_vars