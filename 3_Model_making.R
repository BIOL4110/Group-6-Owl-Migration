#### creating models and visualising results

######### NW ##########

#linear model + gamma
NW_model <- glm(data = final_df_NW, formula = stadradized_count ~ NW + UCW + LCW + SW + obs_year, family = Gamma(link = 'log'))
summary(NW_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(NW_model)
plot(res)

# another way of checking residuals and assumptions
library(performance)
check_model(NW_model)


# plotting the model 
ggplot(final_df_NW, aes(y = stadradized_count)) +
  geom_point(aes(x = NW), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NW), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCW), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCW), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCW), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCW), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SW), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SW), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "North West",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()

######## UCW ########
#linear model + gamma
UCW_model <- glm(data = final_df_UCW, formula = stadradized_count ~ NW + UCW + LCW + SW + obs_year, family = Gamma(link = 'log'))
summary(UCW_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(UCW_model)
plot(res)

# another way of checking residuals and assumptions
library(performance)
check_model(UCW_model)

ggplot(final_df_UCW, aes(y = stadradized_count)) +
  geom_point(aes(x = NW), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NW), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCW), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCW), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCW), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCW), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SW), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SW), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Upper Center West",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


######### LCW ########
# linear model + gamma
LCW_model <- glm(data = final_df_LCW, formula = stadradized_count ~ NW + UCW + LCW + SW + obs_year, family = Gamma(link = 'log'))
summary(LCW_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(LCW_model)
plot(res)

# another way of checking residuals and assumptions
library(performance)
check_model(LCW_model)

ggplot(final_df_LCW, aes(y = stadradized_count)) +
  geom_point(aes(x = NW), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NW), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCW), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCW), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCW), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCW), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SW), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SW), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Lower Center West",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()
####### SW ##########
#linear model + gamma
SW_model <- glm(data = final_df_SW, formula = stadradized_count ~ NW + UCW + LCW + SW + obs_year, family = Gamma(link = 'log'))
summary(SW_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(SW_model)
plot(res)

# another way of checking residuals and assumptions
library(performance)
check_model(SW_model)


ggplot(final_df_SW, aes(y = stadradized_count)) +
  geom_point(aes(x = NW), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NW), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCW), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCW), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCW), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCW), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SW), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SW), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "South West",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


####### NC #########
#linear model + gamma
NC_model <- glm(data = final_df_NC, formula = stadradized_count ~ NC + UCC + LCC + SC + obs_year, family = Gamma(link = 'log'))
summary(NC_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(NC_model)
plot(res)

# another way of checking residuals and assumptions(this is the corelation one)
library(performance)
check_model(NC_model)

ggplot(final_df_NC, aes(y = stadradized_count)) +
  geom_point(aes(x = NC), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NC), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCC), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCC), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCC), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCC), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SC), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SC), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "North Central",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


######## UCC #########
#linear model + gamma
UCC_model <- glm(data = final_df_UCC, formula = stadradized_count ~ NC + UCC + LCC + SC + obs_year, family = Gamma(link = 'log'))
summary(UCC_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(UCC_model)
plot(res)

# another way of checking residuals and assumptions(this is the corelation one)
library(performance)
check_model(UCC_model)

ggplot(final_df_UCC, aes(y = stadradized_count)) +
  geom_point(aes(x = NC), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NC), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCC), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCC), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCC), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCC), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SC), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SC), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Upper Central Central",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


####### LCC #########
#linear model + gamma
LCC_model <- glm(data = final_df_LCC, formula = stadradized_count ~ NC + UCC + LCC + SC + obs_year, family = Gamma(link = 'log'))
summary(LCC_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(LCC_model)
plot(res)

# another way of checking residuals and assumptions(this is the corelation one)
library(performance)
check_model(LCC_model)

ggplot(final_df_LCC, aes(y = stadradized_count)) +
  geom_point(aes(x = NC), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NC), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCC), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCC), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCC), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCC), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SC), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SC), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Lower Central Central",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


######## SC ###########
#linear model + gamma
SC_model <- glm(data = final_df_SC, formula = stadradized_count ~ NC + UCC + LCC + SC + obs_year, family = Gamma(link = 'log'))
summary(SC_model)

# a way of checking the residuals of the data
library(DHARMa)
res <- simulateResiduals(SC_model)
plot(res)

# another way of checking residuals and assumptions(this is the corelation one)
library(performance)
check_model(SC_model)

ggplot(final_df_SC, aes(y = stadradized_count)) +
  geom_point(aes(x = NC), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NC), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCC), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCC), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCC), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCC), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SC), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SC), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "South Central",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


####### NE #########
#  gamma linear regression
NE_model <- glm(data = final_df_NE, formula = stadradized_count ~ NE + UCE + LCE + SE + obs_year, family = Gamma(link = 'log'))
summary(NE_model)

# showing residuasls 
library(DHARMa)
res <- simulateResiduals(NE_model)
plot(res)

# testing assumptions
library(performance)
check_model(NE_model)


ggplot(final_df_NE, aes(y = stadradized_count)) +
  geom_point(aes(x = NE), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NE), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCE), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCE), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCE), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCE), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SE), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SE), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "North East",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()

######## UCE #########
# running gamma linear regression
UCE_model <- glm(data = final_df_UCE, formula = stadradized_count ~ NE + UCE + LCE + SE + obs_year, family = Gamma(link = 'log'))
summary(UCE_model)

# showing residuasls 
library(DHARMa)
res <- simulateResiduals(UCE_model)
plot(res)

# testing assumptions
library(performance)
check_model(UCE_model)

ggplot(final_df_UCE, aes(y = stadradized_count)) +
  geom_point(aes(x = NE), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NE), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCE), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCE), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCE), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCE), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SE), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SE), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Upper Center East",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()


####### LCE #########
# running gamma linear regression
LCE_model <- glm(data = final_df_LCE, formula = stadradized_count ~ NE + UCE + LCE + SE + obs_year, family = Gamma(link = 'log'))
summary(LCE_model)

# showing residuasls 
library(DHARMa)
res <- simulateResiduals(LCE_model)
plot(res)

# testing assumptions
library(performance)
check_model(LCE_model)

ggplot(final_df_LCE, aes(y = stadradized_count)) +
  geom_point(aes(x = NE), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NE), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCE), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCE), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCE), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCE), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SE), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SE), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "Lower Center East",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()

######## SE ###########
# running gamma linear regression
SE_model <- glm(data = final_df_SE, formula = stadradized_count ~ NE + UCE + LCE + SE + obs_year, family = Gamma(link = 'log'))
summary(SE_model)

# showing residuasls 
library(DHARMa)
res <- simulateResiduals(SE_model)
plot(res)

# testing assumptions
library(performance)
check_model(SE_model)

ggplot(final_df_SE, aes(y = stadradized_count)) +
  geom_point(aes(x = NE), colour = "greenyellow") +  # Scatter plot of the data points
  geom_smooth(aes(x = NE), method = "lm", se = TRUE, colour = "greenyellow") +
  geom_point(aes(x = UCE), colour = "navy") +  # Scatter plot of the data points
  geom_smooth(aes(x = UCE), method = "lm", se = TRUE, colour = "navy") +
  geom_point(aes(x = LCE), colour = "hotpink") +  # Scatter plot of the data points
  geom_smooth(aes(x = LCE), method = "lm", se = TRUE, colour = "hotpink") +
  geom_point(aes(x = SE), colour = "peru") +  # Scatter plot of the data points
  geom_smooth(aes(x = SE), method = "lm", se = TRUE, colour = "peru") +
  labs(title = "South East",
       x = "Temperature (°C) ",
       y = "Standardized Snowy Owl Count") +
  theme_minimal()

