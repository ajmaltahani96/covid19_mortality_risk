#directory
setwd("C:/Users/ajmal/OneDrive/Documents/AA UM/SQB7002")

#clear environment
rm(list = ls())

#reading files
df1 <- read.csv("DatasetForPublication.csv", header = TRUE)

# eliminate unique ID
df2 <- df1[,-1]

# selecting predictors
fs = c(1:14)
df2 = df2[,fs]

# creating frequency table
n = ncol(df2)-1
tbl_frequency <- vector("list", length = n)

# creating a list of count
for (i in 1:n) {
  tbl_frequency[[i]] <- table(df2[,n+1],
                              df2[,i])
}

# creating the attributes title
xlabel <- c("Gender", "Age Category", "Ethnicity",
            "Index of Multiple Deprivation (Measure of Poverty)", "BMI Category",
            "Diabetes Type 1", "Diabetes Type 2", "Hypertension", "Cardiovascular Disease",
            "Asthma", "Chronic Obstructive Pulmonary Disease", "Cancer", "Renal Disease")
names(tbl_frequency) <- xlabel

# visualization
viz <- par(mfrow = c(1,1))
for (i in 1:length(tbl_frequency)) {
  viz[[i]] <- barplot(tbl_frequency[[i]],
                      xlab = xlabel[i],
                      beside = TRUE,
                      main = "Admission into ICU",
                      ylab = "Frequency",
                      col = c("#D7BDE2","#EC7063"))
  legend("topright",
         c("No","Yes"),
         fill = c("#D7BDE2","#EC7063")
  )
}

# creating probability table
tbl_prob <- vector("list", length = n)

for (i in 1:n) {
  tbl_prob[[i]] <- round(prop.table(table(df2[,n+1],
                                          df2[,i])), 3)
}

names(tbl_prob) <- xlabel

# chi-square ranking

# creating empty list
chi <- c()
dof <- c()
wh <- c()

# looping chi-square
for (i in 1:13) {
  chi[i] <- chisq.test(tbl_frequency[[i]])$statistic
  dof[i] <- chisq.test(tbl_frequency[[1]])$parameter
  wh[i] <- ((7/9)+ (sqrt((dof[i])) * (chi[i]/dof[i])^(1/3))-1+(2/(9*dof[i])))^3
  print(wh[i])
}

# creating data frame for chi-square
wh_df <- data.frame(names(df2[,1:13]), wh)
colnames(wh_df) <- c("Predictors","statistic")
wh_df <- wh_df[order(wh_df$statistic, decreasing = T),]
wh_df

#plotting the chi-square ranking
plot(wh_df$statistic, ylab = "F-stat")
