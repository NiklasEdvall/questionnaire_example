
# Load packages ======================================================
library(readr)
library(arsenal)

# Read in data and specify format for some variables =================
dat <- read_csv("data/Data.csv", col_types = cols(logindate = col_date(format = "%Y-%m-%d"),  # Specify date format
                                                  logintime = col_time(format = "%H:%M:%S"),  # Specify time format
                                                  logofftime = col_time(format = "%H:%M:%S")))# Specify time format

# Inspect data =======================================================
head(dat)

#Create list of names that match names of THI items in data ==========
THI.names <- paste("THI", seq(1:25), sep="_")

# Specify/rename variable types and labels ===========================

# Factor (categorical variables)
dat$item.1 <- factor(dat$item.1, levels = c(1,2), labels = c("M", "F"))
dat$item.3 <- factor(dat$item.3, levels = c(1,2,3,4), labels = c("No", "Sometimes", "Often", "Always"))

# Rename variables
names(dat)[names(dat) == "item.1"] <- "sex"
names(dat)[names(dat) == "item.2"] <- "age"
names(dat)[names(dat) == "item.3"] <- "hearing"

# Calculate new variables of interest ================================
dat$THIscore <- apply(dat[,THI.names], 1, FUN=sum)
dat$t.online <- as.numeric((dat$logofftime - dat$logintime)/60) #divide by 60 for minutes

# Descriptive table using the arsenal-package function tableby() =====
tab1 <- tableby(sex ~ age + hearing + THIscore, data = dat, digits = 2)

summary(tab1, text = TRUE)

# Plot THI score as function of age ==================================
plot(dat$age, dat$THIscore) #Scatter plot
abline(lm(dat$age ~ dat$THIscore)) #Linear regression line

# Calculate linear regression model for age ~ THIscore
lin.mod1 <- lm(dat$age ~ dat$THIscore)
summary(lin.mod1)

# Plot time online as function of age ================================
plot(dat$age, dat$t.online) #Scatter plot
abline(lm(dat$t.online ~ dat$age)) #Linear regression line

# Calculate linear regression model for time online ~ age
lin.mod2 <- lm(dat$t.online ~ dat$age)
summary(lin.mod2)
