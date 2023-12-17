#--------Loading the data-----------------
#load("/Users/Imthiyas/rehoming.RData")
#createsample(201771669)
#save(mysample, file = "mysample.RData")
#mysample
#summary(mysample)
install.packages("languageserver")

#-----------Introduction-------------------
#Our dataset can be downloaded from here: https://drive.google.com/file/d/11WetHwVuqTwPRiexe02sk8Nf_7queNqq/view?usp=share_link
load("/Users/Imthiyas/Library/CloudStorage/OneDrive-UniversityofLeeds/MSc Data Science & Analytics/Semester 1/Statistical Theory & Methods/Coursework/mysample.RData")
Data = get('mysample')
print(Data)
print(dim(Data)) #305 rows and 7 columns
Population_Mean = 27
Population_Variance = 74
Population_Standard_Deviation = sqrt(Population_Variance)

#------------Data Understanding-----------------
#Load the necessary libraries
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(BSDA)
library(nortest)
library(MASS)

#Finding Outliers: Missing Values or NA
print(summary(Data))
#We have 6 NA in Breed; Max in Rehomed column is 99999 (These are missing values)

MissingValues = colSums(is.na(Data))
print(MissingValues)
#We have 6 NA in Breed and 14 in Returned

copyData <- Data
copyData$count_na <- rowSums(is.na(Data))
print(sum(copyData$count_na)) #20 NA in total

unique_Rehomed <- unique(Data$Rehomed) # Get unique values in the Rehomed column
print(unique_Rehomed)
unique_counts_Rehomed <- table(Data$Rehomed) # Get the sum of each unique value in the Rehomed column
print(unique_counts_Rehomed) #99999: 9

unique_Returned <- unique(Data$Returned) # Get unique values in the Returned column
print(unique_Returned) #Yes, No, Unknown, NA
unique_counts_Returned <- table(Data$Returned) # Get the sum of each unique value in the Returned column
print(unique_counts_Returned) #Yes: 23, No: 264, Unknown: 4, NA: 14

# Calculate the number and percentage of rows with missing observations for rehoming time
missing_rehoming_time <- sum(Data$Rehomed == 99999)
missing_rehoming_time_percentage <- (missing_rehoming_time / nrow(Data)) * 100

# Calculate the number and percentage of rows with missing observations for breed
missing_breed <- sum(is.na(Data$Breed))
missing_breed_percentage <- (missing_breed / nrow(Data)) * 100

# Calculate the number and percentage of rows with NA observations for Returned
missing_NA_returned <- sum(is.na(Data$Returned))
missing_NA_returned_percentage <- (missing_NA_returned / nrow(Data)) * 100

# Calculate the number and percentage of rows with Unknown observations for Returned
missing_Unknown_returned <- sum(!is.na(Data$Returned) & Data$Returned == "Unknown")
missing_Unknown_returned_percentage <- (missing_Unknown_returned / nrow(Data)) * 100

# Print the results
print(paste("Number of rows with missing rehoming time: ", missing_rehoming_time))
print(paste("Percentage of rows with missing rehoming time: ", missing_rehoming_time_percentage, "%"))
print(paste("Number of rows with missing breed: ", missing_breed))
print(paste("Percentage of rows with missing breed: ", missing_breed_percentage, "%"))
print(paste("Number of rows with missing NA in Returned: ", missing_NA_returned))
print(paste("Percentage of rows with missing NA in Returned: ", missing_NA_returned_percentage, "%"))
print(paste("Number of rows with missing Unknown in Returned: ", missing_Unknown_returned))
print(paste("Percentage of rows with missing breed: ", missing_Unknown_returned_percentage, "%"))
total_missing_obj_percentage <- (missing_rehoming_time + missing_breed)/nrow(Data) * 100 #We are concerned about the missing values in Rehomed and Breed alone.
print(total_missing_obj_percentage)

#Checking if the the Breed NA and Rehomed 99999 are not in the same rows, so that we know how many percent of rows are we removing
minm <- filter(Data, Rehomed == 99999 | is.na(Breed))
print(minm) #No overlapping of NA and 99999 in a same row, thus we remove 15 rows (4.91% of total rows) from the dataset  (305-290)

#Data Cleaning & Flitering
# Remove these rows from the dataset
cleaned_data <- Data %>%
  filter(Rehomed != 99999, !is.na(Breed))

# Print the cleaned data
print(cleaned_data)
print(dim(cleaned_data)) # 290 rows and 7 columns

#------------Data Exploration-----------------
#Data Exploration:
# Split the data by breed
breeds <- unique(cleaned_data$Breed)
data_by_breed <- split(cleaned_data, cleaned_data$Breed)
print(breeds)
print(data_by_breed)
# Get the number of rows in each type of breed
breed_counts <- table(cleaned_data$Breed)
# Print the number of rows in each type of breed
print(breed_counts)
#Dobermann-21; Labrador Retriever-62; Staffordshire Bull Terrier-207 

#Getting Summary
# Create numerical summaries for each dataset
for(breed in breeds) {
  print(paste("Numerical summaries for breed: ", breed))
  print(summary(data_by_breed[[breed]]))
}#Visited Min has negative values. Maybe outliers

# Get unique values for each breed
for(breed in breeds) {
  print(paste("Unique rehoming times for breed: ", breed))
  print(unique(data_by_breed[[breed]]$Rehomed))
}#The value has Discrete data instead of continuous data

# Summarize the rehoming time for each breed
for(breed in breeds) {
  print(paste("Average rehoming time for breed: ", breed))
  print(mean(data_by_breed[[breed]]$Rehomed, na.rm = TRUE))
}

# Create a boxplot of rehoming time for each breed #Figure 1
ggplot(cleaned_data, aes(x = Breed, y = Rehomed,fill = Breed)) +
  geom_boxplot() +
  labs(title = "Boxplot of Rehoming Time by Breed", x = "Breed", y = "Rehoming Time") + 
  theme(plot.title = element_text(hjust = 0.5))

#-------------Data Modelling-------------------
#Use QQ plot to check if the data is normally distributed for each breed
# Set up a 3-panel plot
par(mfrow = c(1, 3))
par(pty='s')
# Create a QQ plot of rehoming time for each breed #Figure 2
for(breed in breeds) {
  qqnorm(data_by_breed[[breed]]$Rehomed, main = paste(breed), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 20, col = "blue")
  qqline(data_by_breed[[breed]]$Rehomed, col = 2, lwd = 2)
}#Right skewed; Light tailed;  Bimodal?

#Use ECDF to check if the data is normally distributed for each breed
# Set up a 3-panel plot
par(mfrow = c(3, 1))
# Create an ECDF of rehoming time for each breed
for(breed in breeds) {
  Fn <- ecdf(data_by_breed[[breed]]$Rehomed)
  plot(Fn, verticals=TRUE ,main = paste("ECDF of Rehoming Time for", breed), xlab = "Rehoming Time", ylab = "ECDF")
  # Add a curve
  curve(pnorm(x, mean = mean(data_by_breed[[breed]]$Rehomed, na.rm = TRUE), sd = sd(data_by_breed[[breed]]$Rehomed, na.rm = TRUE)), add = TRUE, col = "red", lwd = 2)
} #We won't use this, as this does not give perfect results for all breeds

#Use Histogram to check if the data is normally distributed for each breed #Figure 3
# Set up a 3-panel plot
par(mfrow = c(1, 3))
# Create a histogram of rehoming time for each breed
breed_colors <- c("skyblue", "lightpink", "lightgreen")  # Define colors for each breed
i <- 1  # Initialize counter
for(breed in breeds) {
  hist(data_by_breed[[breed]]$Rehomed, main = paste(breed), xlab = "Rehoming Time", ylab = "Count", freq = FALSE, col = breed_colors[i], xlim = c(0, 60), ylim = c(0, 0.06))
  lines(density(data_by_breed[[breed]]$Rehomed, na.rm = TRUE), col = "#4B0082")
  i <- i + 1  # Increment counter
}

#-------------Findings & Insights-------------------
#Checking Distribution for Normality:
mu_dobermann <- mean(data_by_breed[["Dobermann"]]$Rehomed, na.rm = TRUE)
sigma_dobermann <- sd(data_by_breed[["Dobermann"]]$Rehomed, na.rm = TRUE)
mu_labrador <- mean(data_by_breed[["Labrador Retriever"]]$Rehomed, na.rm = TRUE)
sigma_labrador <- sd(data_by_breed[["Labrador Retriever"]]$Rehomed, na.rm = TRUE)
mu_staffordshire <- mean(data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, na.rm = TRUE)
sigma_staffordshire <- sd(data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, na.rm = TRUE)
#KS Test for Normality
ks.test(x =  data_by_breed[["Dobermann"]]$Rehomed, y = "pnorm", mean = mu_dobermann, sd = sigma_dobermann)
ks.test(x =  data_by_breed[["Labrador Retriever"]]$Rehomed, y = "pnorm", mean = mu_labrador, sd = sigma_labrador)
ks.test(x =  data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, y = "pnorm", mean = mu_staffordshire, sd = sigma_staffordshire)

#Shapiro-Wilk Test for Normality
shapiro.test(data_by_breed[["Dobermann"]]$Rehomed)
shapiro.test(data_by_breed[["Labrador Retriever"]]$Rehomed)
shapiro.test(data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed)

#Chi-Square goodness of Fit Test for Normality
pearson.test(data_by_breed[["Dobermann"]]$Rehomed)
pearson.test(data_by_breed[["Labrador Retriever"]]$Rehomed)
pearson.test(data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed) #We will not use this, as our data is not categorical

#Average Rehoming Time:
# Calculate a confidence interval for the mean rehoming time for each breed
par(mfrow = c(1, 1))
#T test
ci_dobermann_t <- t.test(data_by_breed[["Dobermann"]]$Rehomed, mu = 27)$conf.int

#Z test
ci_labrador_z <- z.test(data_by_breed[["Labrador Retriever"]]$Rehomed, mu = 27, sigma.x = sqrt(74))$conf.int
ci_staffordshire_z <- z.test(data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, mu = 27, sigma.x = sqrt(74))$conf.int
# Create a data frame to summarize the results
ci_summary <- data.frame(
  Breed = c("Dobermann", "Labrador Retriever", "Staffordshire Bull Terrier"),
  Lower_Bound = c(ci_dobermann_t[1], ci_labrador_z[1], ci_staffordshire_z[1]),  #Since we choose t test for dobermann and z test for the other two.
  Upper_Bound = c(ci_dobermann_t[2], ci_labrador_z[2], ci_staffordshire_z[2])
)
# Print the summary
print(ci_summary)

# Plotting the confidence intervals
par(pty='m') 
# Analysis labels for the left side:
analysis = c("Dobermann",
"Labrador Retriever",
"Staffordshire Bull Terrier")
# Results of each test (estimated mean, upper CI limit, lower CI limit, p-value):
estimate = c(17.85714, 19.85484, 19.33816)
upper = c(22.65569, 21.99609, 20.51003)
lower = c(13.05859, 17.71359, 18.16630)
pval = c("7.46e-4","6.143e-11","2.2e-16")
# Note that the order of the results in each vector must match the order of the labels in the vector "analysis". Set the margin widths:
par(mar = c(5,6,0,8))
# Create an empty plot of a suitable size (considering the width of your confidence intervals):
plot(x = 0, # One point at (0,0).
xlim = c(-5, 30), ylim=c(0.75, 3), # Axis limits.
type = "n", xaxt = "n", yaxt="n", # No points, no axes drawn.
xlab = NULL, ylab= NULL, ann = FALSE, # No axis labels or numbers.
bty="n") # No box.
# Add a horizontal (side = 1) axis:
axis(side = 1, cex.axis = 1)
# Add an axis label 4 lines below the axis:
mtext("Rehoming Time",
side = 1, line = 2)
# Add some grid lines, preferably lined up with the numbers on the horizontal axis:
for(i in c(0, 10, 20)){
lines(c(i, i), c(0, 2.25), lty = 2, col = "gray53")
}
# Add labels for each analysis on the left (side = 2) at vertical heights of 1, 2, and 3:
verticalpos = seq(1, 2, length.out = length(analysis)) # Adjust the gap here
mtext(text = analysis, at = verticalpos,
side = 2, line = 5, outer = FALSE, las = 1, adj = 0)
# Try changing the "line" option to move these closer to or away from the plotted intervals.
# Plot the four point estimates (centres of the CIs for each analysis):
points(estimate, verticalpos, pch = 16)
# Plot the three interval estimates:
for(i in 1:3 ){
lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))}
# Now we add numerical results on the right (side = 4), but we need to put them into a nice form first. Note that paste() merges words and numbers, and formatC() allows us to control the number of decimal places.
est <- formatC(estimate, format='f', digits = 2)
P <- formatC(pval , format = 'f', digits = 7)
pval <- paste("p =", P) # Type pval to see what this does.
L <- formatC(lower, format = 'f', digits = 2)
U <- formatC(upper, format = 'f', digits = 2)
interval <- paste("(", L, ", ", U, "),", sep = "") # Type interval to check.
# Putting it all together:
results <- paste(est, interval, pval)
# Add to the plot:
mtext(text = results, at = verticalpos,
side = 4, line = 4, outer = FALSE, las = 1, adj = 1)
# Like a Christmas present, an R plot belongs in a box:
box("inner")
# Add a title to the plot
title("Confidence Intervals for Mean Rehoming Time",line=-10)


#Comparison of Breeds:
# Calculate a confidence interval for the difference in mean rehoming times between each pair of breeds
ci_dobermann_labrador <- t.test(data_by_breed[["Dobermann"]]$Rehomed, data_by_breed[["Labrador Retriever"]]$Rehomed, var.equal = TRUE)$conf.int
ci_dobermann_staffordshire <- t.test(data_by_breed[["Dobermann"]]$Rehomed, data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, var.equal = TRUE)$conf.int
ci_labrador_staffordshire <- t.test(data_by_breed[["Labrador Retriever"]]$Rehomed, data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, var.equal = TRUE)$conf.int
# Create a data frame to summarize the results
ci_diff_summary <- data.frame(
  Pair = c("Dobermann vs Labrador Retriever", "Dobermann vs Staffordshire Bull Terrier", "Labrador Retriever vs Staffordshire Bull Terrier"),
  Lower_Bound = c(ci_dobermann_labrador[1], ci_dobermann_staffordshire[1], ci_labrador_staffordshire[1]),
  Upper_Bound = c(ci_dobermann_labrador[2], ci_dobermann_staffordshire[2], ci_labrador_staffordshire[2])
)
# Print the summary
print(ci_diff_summary)

# Calculate the p-value for the difference in mean rehoming times between each pair of breeds
p_dobermann_labrador <- t.test(data_by_breed[["Dobermann"]]$Rehomed, data_by_breed[["Labrador Retriever"]]$Rehomed, var.equal = TRUE)$p.value
p_dobermann_staffordshire <- t.test(data_by_breed[["Dobermann"]]$Rehomed, data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, var.equal = TRUE)$p.value
p_labrador_staffordshire <- t.test(data_by_breed[["Labrador Retriever"]]$Rehomed, data_by_breed[["Staffordshire Bull Terrier"]]$Rehomed, var.equal = TRUE)$p.value
# Create a data frame to summarize the results
p_value_summary <- data.frame(
  Pair = c("Dobermann vs Labrador Retriever", "Dobermann vs Staffordshire Bull Terrier", "Labrador Retriever vs Staffordshire Bull Terrier"),
  P_Value = c(p_dobermann_labrador, p_dobermann_staffordshire, p_labrador_staffordshire)
)
# Print the summary
print(p_value_summary)

#----------------Discussion-----------------
#Transformation of Rehoming Time to Logarithmic Scale
# Create a new column for the log of rehoming time
cleaned_data$log_rehoming_time <- log(cleaned_data$Rehomed)
breeds <- unique(cleaned_data$Breed)
data_by_breed <- split(cleaned_data, cleaned_data$Breed)
print(breeds)
print(data_by_breed)
# Create a boxplot of the log of rehoming time for each breed #Figure 5
ggplot(cleaned_data, aes(x = Breed, y = log_rehoming_time,fill = Breed)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log Rehoming Time by Breed", x = "Breed", y = "Log Rehoming Time") + 
  theme(plot.title = element_text(hjust = 0.5))
#Use QQ plot to check if the data is normally distributed for each breed
# Set up a 3-panel plot
par(mfrow = c(1, 3))
par(pty='s')
# Create a QQ plot of rehoming time for each breed #Figure 6
for(breed in breeds) {
  qqnorm(data_by_breed[[breed]]$log_rehoming_time, main = paste(breed), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 20, col = "blue")
  qqline(data_by_breed[[breed]]$log_rehoming_time, col = 2, lwd = 2)
}

#Future Work using Weibull Distribution
library(MASS)
#Not Exponential, neither Poisson, It is skewed
par(pty='m',mfrow = c(1, 1))
fit_weibull <- fitdistr(data_by_breed[["Dobermann"]]$Rehomed,"weibull")
hist(data_by_breed[["Dobermann"]]$Rehomed, breaks = "Sturges", probability = TRUE,
     main = "Histogram of Rehoming Times with Fitted Weibull Curve",
     xlab = "Rehoming Times", ylab = "Density")
# Function to generate Weibull density values
weibull_density <- function(x, shape, scale) {
  dweibull(x, shape = shape, scale = scale)
}
# Extract the shape and scale parameters from the fit
shape_param <- fit_weibull$estimate["shape"]
scale_param <- fit_weibull$estimate["scale"]
# Add the curve using the shape and scale parameters obtained from the fit
curve(weibull_density(x, shape_param, scale_param), add = TRUE, col = "red", lwd = 2, from=0, to=max(data_by_breed[["Dobermann"]]$Rehomed))