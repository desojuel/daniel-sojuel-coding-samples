library(dplyr)

#outliers

Q <- quantile(df$suma_aa, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df$suma_aa)
up <-  Q[2]+1*iqr # Upper Range  
low <- Q[1]-1*iqr # Lower Range
df <- subset(df, df$suma_aa > low & df$suma_aa < up)

hist(df$suma_aa)
shapiro.test(df$suma_aa)

qqnorm(df$suma_aa)
qqline(df$suma_aa)
length(df$suma_aa)

#z normalization

df$suma_aa_z <- scale(df$suma_aa)
hist(df$suma_aa_z)
shapiro.test(df$suma_aa_z)

boxplot(df$suma_aa, main = "Box Plot of Z-Score Normalized Data")
boxplot(df$suma_aa_z, main = "Box Plot of Z-Score Normalized Data")

qqnorm(df$suma_aa_z)
qqline(df$suma_aa_z)

summary(df$suma_aa_z)
sd(df$suma_aa_z)


# min max normalization

df$suma_aa_minmax <- scale(df$suma_aa,
                           center = min(df$suma_aa),
                           scale = max(df$suma_aa) - min(df$suma_aa))
hist(df$suma_aa_minmax)
shapiro.test(df$suma_aa_minmax)

# range normalization

df$suma_aa_range <- (df$suma_aa - min(df$suma_aa)) / 
  (max(df$suma_aa) - min(df$suma_aa))
hist(df$suma_aa_range)
shapiro.test(df$suma_aa_range)


# Create histograms for original and normalized mpg variables
par(mfrow = c(2, 2))
hist(df$suma_aa, main = "Original suma_aa", xlab = "sumatoria")
hist(df$suma_aa_z, main = "Z-score", xlab = "Z-scores")
hist(df$suma_aa_minmax, main = "min max", xlab = "min max scores")
hist(df$suma_aa_range, main = "range", xlab = "range")

