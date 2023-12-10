install.packages("ggcorrplot") 
library(ggcorrplot)

# open dataframe
data <- read.csv("C:\\Users\\user\\Desktop\\проект метрика 2023\\data0.csv", header = TRUE, sep = ';')
data
new_df <- na.omit(data)
new_df

# about dataframe
dim(new_df) # размерность 50х9
head(new_df) # 
summary(new_df) # Сводная статистика по каждому столбцу

new_df$ln_GDP <- log(new_df$GDP_2022)
new_df$ln_tour <- log(new_df$tourists_2022)


# outliers check
boxplot(new_df$ln_GDP, horizontal = TRUE, xlab = "ln_GDP", main = "Boxplot of ln_gdp")
stripchart(new_df$ln_GDP, method = "jitter", pch = 19, add = TRUE, col = "green")

boxplot(new_df$ln_tour, horizontal = TRUE, xlab = "ln_tour", main = "Boxplot of ln_tour")
stripchart(new_df$ln_tour, method = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(new_df$tree_height, horizontal = TRUE, xlab = "tree_height", main = "Boxplot of tree_height")
stripchart(new_df$tree_height, method = "jitter", pch = 19, add = TRUE, col = "green")

boxplot(new_df$CORRUPT_IND, horizontal = TRUE, xlab = "CORRUPT_IND", main = "Boxplot of CORRUPT_IND")
stripchart(new_df$CORRUPT_IND, method = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(new_df$christ_hlds, horizontal = TRUE, xlab = "christ_hlds", main = "Boxplot of christ_hlds")
stripchart(new_df$christ_hlds, method = "jitter", pch = 19, add = TRUE, col = "green")

boxplot(new_df$CORR_dev, horizontal = TRUE, xlab = "CORR_dev", main = "Boxplot of CORR_dev")
stripchart(new_df$CORR_dev, method = "jitter", pch = 19, add = TRUE, col = "red")

# outliers check
hist(new_df$GDP_2022, xlab = "GDP_2022", main = "Histogram of GDP_2022", 25)

new_df$ln_GDP <- log(new_df$GDP_2022)
hist(new_df$ln_GDP, xlab = "ln_GDP", main = "Histogram of ln_GDP")

new_df$ln_tour <- log(new_df$tourists_2022)
hist(new_df$ln_tour, xlab = "ln_tour", main = "Histogram of ln_tourists")

# new feature
new_df$CORR_dev <- new_df$CORRUPT_IND * new_df$dev_count_1

# correlation check
cor_data <- cor(new_df[, c('CORR_dev', 'tree_height', 'CORRUPT_IND','ln_tour', 'ln_GDP', 'гор_мил_1', 'christ_count_0', 'christ_hlds', 'dev_count_1')])
cor_data <- round(cor_data, 1)
ggcorrplot(cor_data, hc.order =TRUE, lab =TRUE) 


# got rid of outliers, new dataframe
data <- read.csv("C:\\Users\\user\\Desktop\\проект метрика 2023\\data1.csv", header = TRUE, sep = ';')
data
new_df1 <- subset(data, select = - c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12))
new_df1 <- na.omit(new_df1)
new_df1
new_df1$ln_GDP <- log(new_df1$GDP_2022)
new_df1$ln_tour <- log(new_df1$tourists_2022)
new_df1$CORR_dev <- new_df1$CORRUPT_IND * new_df1$dev_count

cor_data1 <- cor(new_df1[, c('CORR_dev', 'tree_height', 'CORRUPT_IND','ln_tour', 'ln_GDP', 'гор_мил_1', 'christ_count_0', 'christ_hlds', 'dev_count_1')])
cor_data1 <- round(cor_data1, 1)
ggcorrplot(cor_data1, hc.order =TRUE, lab =TRUE) 

hist(new_df1$tree_height, xlab = "tree_height", main = "Histogram of tree_height", 10)
hist(new_df1$tourists_2022, xlab = "tourists_2022", main = "Histogram of tourists_2022", 10)
hist(new_df1$ln_tour, xlab = "ln_tour", main = "Histogram of ln_tourists")
hist(new_df1$GDP_2022, xlab = "tourists_2022", main = "Histogram of tourists_2022", 10)
hist(new_df1$ln_GDP, xlab = "ln_tour", main = "Histogram of ln_tourists")

plot(new_df1$ln_GDP, new_df1$tree_height)
m <- lm(tree_height ~ ln_GDP, data = new_df1)
abline(m)

plot(new_df1$ln_tour, new_df1$tree_height)
m <- lm(tree_height ~ ln_tour, data = new_df1)
abline(m)

plot(new_df1$christ_count_0*new_df1$гор_мил_1, new_df1$tree_height)
m <- lm(tree_height ~ I(christ_count_0*гор_мил_1), data = new_df1)
abline(m)

plot(new_df1$CORRUPT_IND, new_df1$tree_height)
m <- lm(tree_height ~ CORRUPT_IND, data = new_df1)
abline(m)

plot(new_df1$CORRUPT_IND, new_df1$tree_height)
m <- lm(tree_height ~ CORRUPT_IND, data = new_df1)
abline(m)

plot(new_df1$christ_hlds, new_df1$tree_height)
m <- lm(tree_height ~ christ_hlds, data = new_df1)
abline(m)

plot(new_df1$ln_tour*new_df1$гор_мил_1, new_df1$tree_height)
m <- lm(tree_height ~ I(гор_мил_1*ln_tour), data = new_df1)
abline(m)


#modelsLong
m_8 <- lm(tree_height ~
            I((christ_count_0-1)*ln_GDP) 
          + I(гор_мил_1*ln_tour) 
          + I(гор_мил_1*ln_GDP)
          + I(log(CORR_dev + 1)*гор_мил_1)
          + гор_мил_1
          + I(christ_count_0*гор_мил_1)
          + CORR_dev + christ_count_0 + ln_tour + ln_GDP
          , data = new_df1)
summary(m_8)


m_5 <- lm(tree_height ~ I(CORRUPT_IND*christ_hlds)  
          + I((christ_count_0-1)*ln_GDP) 
          + I(гор_мил_1*ln_tour) 
          + I(гор_мил_1*ln_GDP)
          + I(log(CORR_dev + 1)*гор_мил_1)
          + гор_мил_1
          + I((christ_count_0+1)*christ_hlds*гор_мил_1)
          + I(christ_count_0*гор_мил_1)
          + CORR_dev + christ_count_0 + ln_tour + ln_GDP + christ_hlds
          , data = new_df1)
summary(m_5)


m_4 <- lm(tree_height ~ I(CORRUPT_IND*christ_hlds)  
          + I((christ_count_0-1)*ln_GDP) 
          + I(гор_мил_1*ln_tour) 
          + I(гор_мил_1*ln_GDP)
          + I(log(CORR_dev + 1)*гор_мил_1)
          + гор_мил_1
          + I((christ_count_0+1)*christ_hlds*гор_мил_1)
          + I(christ_count_0*гор_мил_1) +
            + CORR_dev  + ln_tour + ln_GDP + christ_hlds
          , data = new_df1)
summary(m_4)

m_6 <- lm(tree_height ~ 
            + I((christ_count_0-1)*ln_GDP) 
          + I(гор_мил_1*ln_tour) 
          + I(гор_мил_1*ln_GDP)
          + I(log(CORR_dev + 1)*гор_мил_1)
          + гор_мил_1
          + I((christ_count_0+1)*christ_hlds*гор_мил_1)
          + I(christ_count_0*гор_мил_1)
          + CORR_dev  + ln_tour + ln_GDP + christ_hlds
          , data = new_df1)
summary(m_6)

m_3 <- lm(tree_height ~ I(CORRUPT_IND*christ_hlds)  
          + I(гор_мил_1*ln_tour) 
          + I(гор_мил_1*ln_GDP)
          + I(log(CORR_dev + 1)*гор_мил_1)
          + I((christ_count_0+1)*christ_hlds*гор_мил_1)
          + I(christ_count_0*гор_мил_1) + christ_count_0 
          + CORR_dev  + ln_tour + ln_GDP + christ_hlds
          , data = new_df1)
summary(m_3)

stargazer(m_3, m_4, m_5, m_8, m_6,
          type = "html", 
          out = "results140.doc")


#modelsShort
m <- lm(tree_height ~ I(log(CORR_dev + 1)*гор_мил_1) 
        + гор_мил_1 + I(christ_count_0*гор_мил_1)  
        + I(christ_count_0*christ_hlds) 
        + I(гор_мил_1*ln_tour), data = new_df1)
summary(m)

m_ <- lm(tree_height ~ I(log(CORR_dev + 1)*гор_мил_1) 
         + I(christ_count_0*гор_мил_1)  
         + I(christ_count_0*christ_hlds) 
         + I(гор_мил_1*ln_tour), data = new_df1)
summary(m_)

m_1 <- lm(tree_height ~ I(log(CORR_dev + 1)*гор_мил_1)  
          + I(christ_count_0*christ_hlds) 
          + I(гор_мил_1*ln_tour), data = new_df1)
summary(m_1)

m_2 <- lm(tree_height ~ CORRUPT_IND 
          + I(CORRUPT_IND*christ_hlds) 
          + christ_hlds 
          + I(christ_hlds*ln_tour), data = new_df1)
summary(m_2)

m_9 <- lm(tree_height ~ I(log(CORR_dev + 1)*гор_мил_1) 
          + I(christ_count_0*гор_мил_1) 
          + гор_мил_1 
          + I((christ_count_0+1)*christ_hlds), data = new_df1)
summary(m_9)

m_10 <- lm(tree_height ~ I(log(CORR_dev + 1)*гор_мил_1) 
           + I(christ_count_0*гор_мил_1) 
           + гор_мил_1 
           + I((christ_count_0+1)*christ_hlds*гор_мил_1), data = new_df1)
summary(m_10)

m_11 <- lm(tree_height ~ I(christ_count_0*гор_мил_1) 
           + гор_мил_1 
           + I((christ_count_0+1)*christ_hlds*гор_мил_1), data = new_df1)
summary(m_11)

stargazer(m, m_, m_1, m_2, m_9, m_10, m_11,
          type = "html", 
          out = "results1801.doc")

# our favourite
m <- lm(tree_height ~ I(CORR_dev*гор_мил_1) + I(christ_count_0*гор_мил_1) + I(гор_мил_1*ln_tour), data = new_df1)
summary(m)
stargazer(m,
          type = "html", 
          out = "results100.doc")

