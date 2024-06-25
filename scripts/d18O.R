# d18O Analysis

# Set working directory
setwd("/Users/katiehefner/Documents/FAUPaleobiology/Semester_2/Research_Project/")

# Read in data and check it
data <- read.csv("data/d18O.csv")
str(data)

# Remove NAs
clean_data <- data[which(!data$d18O=="NA"),]
clean_data

# Export data
write.table(clean_data, file = "data/dO18Results.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

# Run linear regression
model <- lm(d18O ~ Year_Avg, data = clean_data)
model
summary(model)


# Plot
quartz()
plot(d18O ~ Year_Avg, data = clean_data, type = "b", xlab = "Year", 
     ylab = "", xlim=c(1970,2020))
title(ylab=expression("δ"^18 *"O (‰V-PDB)"), line=2.5)
abline(model)

## 


#############################################

# CSST Data

# Read in data
CSST <- read.csv("data/Leigh_Marine_Station_Annual_Mean_Temperature.csv")

# Remove years with NAs
clean_CSST <- CSST[which(CSST$Temperature..C!="NA"),]

# Perform linear regression
modelCSST <- lm(Temperature..C ~ Year, data = clean_CSST)
modelCSST
summary(modelCSST)

# Plot
plot(Temperature..C ~ Year, data = clean_CSST, type = "b", xlab = "Year", 
     ylab = "Temperature", xlim = c(1965, 2020))
abline(modelCSST)

################################
# Constrain 1972 - 2017
conCSST <- clean_CSST[5:44,]

# Perform linear regression
modconCSST <- lm(Temperature..C ~ Year, data = conCSST)
summary(modconCSST)

# Plot
plot(Temperature..C ~ Year, data = conCSST, type = "b", xlab = "Year", 
     ylab = "Average Annual Temperature", xlim = c(1970, 2020))
abline(modconCSST)

################################

# Modeled data

# Read in
modeled <- read.csv("data/modeledTemp.csv")

# Constrain from 1972 - 2023
conModeled <- modeled[385:1008,]

## Average months for each year

# Vector to save averages
averages <- rep(NA, length(unique(conModeled$year)))
names(averages) <- (unique(conModeled$year))


# Initialize loop
i = 1972
j = 1

for (j in 1:length(unique(conModeled$year))) {
  currentyear <- conModeled[which(conModeled$year==i),]
  sum <- sum(currentyear$SST_degC)
  averages[j] <- sum/length(currentyear$year)
  
  i = i + 1 
  j = j + 1
}
averages

# convert to data.frame
listAverages <- (as.list(averages))
df <- data.frame(names = names(averages), averages = unname(averages))

# Perform linear regression
linMod <- lm(SST_degC ~ year, data = conModeled)
summary(linMod)

# Plot
plot(df, type = "b", xlab = "Year", ylab = "Average Annual Temperature")
plot(SST_degC ~ year, data = conModeled, xlab = "Year", ylab = "Average Annual Temperature")
abline(linMod)
