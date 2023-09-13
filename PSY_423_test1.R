library(tidyverse)

set.seed(1234)

# Return on investment of a stock portfolio
single_data <- data.frame(ROI=rnorm(30,mean = 0.6, sd = 4))

t.test(single_data, mu = 0, alternative = "two.sided")

single_data <- single_data %>%
  mutate(x_bar = mean(ROI),
         Num = ROI - x_bar,
         Z = Num/4)

print()
print(max(single_data$Z))


# Study of an anxiety treatment using a new scoring system
paired_data <- data.frame(Anxiety_Pre = rnorm(35, mean = 35, 5),
                          Anxiety_Post = rnorm(35, mean = 35, 2))

t.test(paired_data$Anxiety_Pre, paired_data$Anxiety_Post, paired = TRUE, alternative = "two.sided")

