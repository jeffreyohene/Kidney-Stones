# Packages needed
library(tidyverse)
library(broom)

# Read datasets kidney_stone_data.csv into data
data <- read_csv("kidney_stone_data.csv")

# First few rows of the dataset
head(data)

# Number and frequency of success and failure of each treatment 
data %>% 
  group_by(treatment,success)%>%
  summarise(N = n()) %>% 
  mutate(Freq = round(N/sum(N),3))

# Number and frequency of success and failure by stone size for each treatment
sum_data <- 
  data %>% 
  group_by(treatment,stone_size,success) %>%
  summarise(N=n())%>%
  mutate(Freq=round(N/sum(N),3))

# Printing dataframe
sum_data


# Bar plot: stone size count within each treatment
sum_data %>%
  ggplot(
    aes(treatment,N)
  ) + 
  geom_bar(aes(fill =stone_size),stat="identity") 



# Chi-squared test
trt_ss <- chisq.test(data
                     stone_size)

# Result in tidy format 
tidy(trt_ss)



# Multiple logistic regression
m <- glm(data = data, success ~  stone_size + treatment, family="binomial")

# Model coefficient table in tidy format
tidy(m)

# Tidy model output into an object
tidy_m <- tidy(m)

# Plot the coefficient estimates with 95% CI for each term in the model
tidy_m %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(ymin=estimate-1.96*std.error, 
                      ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept = 0)

tidy_m

# Is small stone more likely to be a success after controlling for treatment option effect?
# Options: Yes, No 
small_high_success <- "Yes"

# Is treatment A significantly better than B?
# Options: Yes, No 
A_B_sig <- "No"
