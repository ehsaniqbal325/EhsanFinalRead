
# Anomaly detection

# install.packages("pacman")

# Install packages for Anomaly detection
pacman::p_load(ggplot2, grid, gridExtra, robustbase)

# Load Data ############################
# Read data wine
data = read.csv("geoMap-Fruits.csv")   # Load data

# Check the stracture
str(data)

# Transform variables to factors
data$Region = as.factor(data$Region)

# Univariate outliers
#L = Banana
#t = Mango
#gray_w = Apple
#h= Guava
#b = orange
# Boxplot for each variable separately
# Banana
u1 <- qplot(data = data, y = Banana, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Banana") +
  geom_text(aes(label = ifelse(Banana %in%
                                 boxplot.stats(Banana)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u1

# Mango 
u2 <- qplot(data = data, y = Mango, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Mango") +
  geom_text(aes(label = ifelse(Mango %in%
                                 boxplot.stats(Mango)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u2

# Apple
u3 <- qplot(data = data, y = Apple, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Apple") +
  geom_text(aes(label = ifelse(Apple %in%
                                 boxplot.stats(Apple)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u3

# Guava
u4 <- qplot(data = data, y = Guava, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "bretix") +
  geom_text(aes(label = ifelse(Guava %in%
                                 boxplot.stats(Guava)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u4

# orange
u5 <- qplot(data = data, y = orange, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "orange") +
  geom_text(aes(label = ifelse(orange %in%
                                 boxplot.stats(orange)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u5


# Plot boxplots together
grid.arrange(u1, u2, u3, u4, u5, nrow = 2,
             top = "Boxplots: Univariate outliers")


# Bivariate outliers
b1 <- qplot(data = data, x = Banana, y = Mango,
            main = "Banana vs. Mango") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Banana < 2.5 | Banana > 7.5 | Mango > 3.0),
                               as.character(state.name), "")), hjust = 1.5)
b1

b2 <- qplot(data = data, x = Guava, y = orange,
            main = "Guava vs. orange") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Guava > 8.9 | orange > 40),
                               as.character(state.name), "")), hjust = 1.5)
b2

b3 <- qplot(data = data, x = Apple, y = orange,
            main = "Apple vs. orange") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Apple > 7.5 | Apple < 1 | orange > 40),
                               as.character(state.name), "")), hjust = 1.5)
b3

# Plot together
grid.arrange(b1, b2, b3, nrow = 1, top = "Bivariate outliers")



# Clean up #####################
rm(list = ls())


