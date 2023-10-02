library(tidyverse)

set.seed(1234)

# Return on investment of a stock portfolio
single_data <- data.frame(ROI=rnorm(30,mean = 0.6, sd = 4))

## Question 1
 # Create an appropriate visualization to see normality
ggplot(single_data, aes(x=ROI)) +
  geom_histogram() 

## Question 2
  # What is the mean of this data
q2 <- mean(single_data$ROI)

print(paste0("Mean = ", round(q2,3)))

## Question 3
  # Make this data into a z-score assuming it comes from a population with $\sigma = 4$. What is the largest Z-valu? what is the smallest?

single_data <- single_data %>%
  mutate(Z = (ROI - mean(ROI))/4)

print(paste0("Min = ", round(min(single_data$Z),3)))
print(paste0("Max = ", round(max(single_data$Z),3)))


## Question 4
  # The stock broker who is offering you this stock portfolio promise you an average return on investment of 300% 
  # With alpha = 0.05 and the data sample you recieved from this salesman state the null hypothesis for a t-test where 300% is $\mu$.

# $H_0: \bar{x} = 5$

## Question 5
  # Perform a t-test and report the p-value

q5 <- t.test(single_data, mu = 3, alternative = "two.sided")

print(paste0("p=", round(q5$p.value,3)))

single_data <- single_data %>%
  mutate(x_bar = mean(ROI),
         Num = ROI - x_bar,
         Z = Num/4)

print()
print(max(single_data$Z))

## Question 6
  # IWrite the practical conclusion of your test, do you believe this stock broker's claim? 
  # In addition to stating the plane english explanation of your conclusion write a one sentence 
    #formal statistical explanation of your conclusion about the null hypothesis and state the t-statistic, degrees of freedom and p-value.
    # There will be a bonus point if you put these numbers in APA format.

############################################################3

# Study of an anxiety treatment using a new scoring system
paired_data <- data.frame(Anxiety_Pre = rnorm(35, mean = 35, 5),
                          Anxiety_Post = rnorm(35, mean = 35, 2)) %>%
  gather("Group", "Score") %>%
  mutate(Group = factor(Group))


## Question 7
  # State the null hypothesis

# Question 8
  # What kind of t-test do you plan to use to test this hypothesis?

# Question 9
  # Create an appropriate visualization of both sets of observations that is different from how you visualized question 1
ggplot(paired_data, aes(x=Score, y = Group)) +
  geom_violin() 

## Question 10
  # Before doing anything else like a statistical test, what do you think the result will be based on your data visualization? 
  

## Question 11
# What is the mean of both groups from this data
paired_data <- paired_data %>%
  group_by(Group) %>%
  mutate(x_bar = mean(Score))

print(paste0("Mean = ", round(unique(paired_data$x_bar,3))))

## Question 12
  # Make each score of this data into a z-score assuming it comes from a population with $\sigma = 4$. What is the largest Z-value in each group? what is the smallest?

paired_data <- paired_data %>%
  group_by(Group) %>%
  mutate(Z = (Score - mean(Score))/4,
         min = min(Z),
         max = max(Z))

print(paste0("Min = ", round(unique(paired_data$min),3)))
print(paste0("Max = ", round(unique(paired_data$max),3)))

#Question 13
## Create an apropriate plot of theses z-distributions, what do you think the result will be now?

paired_data %>%
  ggplot(aes(x=Z, y = Group)) +
  geom_violin()

## Question 14
# Perform a t-test and report the p-value

pre <- paired_data %>%
  filter(Group == "Anxiety_Pre") %>%
  pull(Score)

post <- paired_data %>%
  filter(Group == "Anxiety_Post") %>%
  pull(Score)

q14 <- t.test(pre, post, paired = TRUE, alternative = "two.sided")


print(paste0("p=", round(q14$p.value,3)))

## Question 15
# Compute Cohen's D. How would you describe the practical signifignace?
mean_pre <- mean(pre)
mean_post <- mean(post)

# Pooled standard deviation
n_pre <- length(pre)
n_post <- length(post)
var_pre <- var(pre)
var_post <- var(post)

pooled_sd <- sqrt(((n_pre - 1) * var_pre + (n_post - 1) * var_post) / (n_pre + n_post - 2))

cohen_d <- (mean_pre - mean_post) / pooled_sd

# Print Cohen's d
print(paste0("Cohen's d = ", round(abs(cohen_d), 3)))


## Question 16
# Write up the practical conclusion of your test, do you believe this anxiety intervention worked? If so did it help?
# In addition to stating the plane English explanation of your conclusion write a one sentence 
#formal statistical explanation of your conclusion about the null hypothesis and state the t-statistic, degrees of freedom, p-value and Cohen's D.
# There will be a bonus point if you put these numbers in APA format.




###########################
# Data generator:
seed <- 12
set.seed(seed)
# Return on investment of a stock portfolio
single_data <- data.frame(ROI=rnorm(30,mean = 0.6, sd = 4))
paired_data <- data.frame(Anxiety_Pre = rnorm(35, mean = 35, 5),
                          Anxiety_Post = rnorm(35, mean = 35, 2)) 

write_csv(single_data, paste0("/home/sam/Regis/PSY423/Exam1Data/Exam1_",seed,"_data1.csv"))
write.csv(paired_data, paste0("/home/sam/Regis/PSY423/Exam1Data/Exam1_",seed,"_data2.csv"))

