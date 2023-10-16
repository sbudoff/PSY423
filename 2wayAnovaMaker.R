# Set a random seed for reproducibility
set.seed(123)

# Number of observations for each combination of factors
n <- 20

# Create the levels for factor A and factor B
factor_A <- rep(letters[1:5], each = n)
factor_B <- rep(LETTERS[1:2], each = n)

# Generate random values for the response variable
Y <- rnorm(n * length(unique(factor_A)) * length(unique(factor_B)))

# Create a data frame
data <- data.frame(factor_A, factor_B, Y)


write_csv(data, file='/home/sam/Regis/PSY423/2wayANOVA_example1.csv')


# Create the levels for factor "Gender" and factor "Sumo"
gender <- rep(c("Male", "Female"), each = n)
sumo <- rep(c("Yes", "No"), each = n)

# Generate random weights for males and females
# Male weight will be greater, and Sumo males will have even greater weight
mean_weight_male_no_sumo <- 100
mean_weight_male_sumo <- 200
mean_weight_female <- 70

# Generate random weights based on factors
Y <- c(
  rnorm(n, mean_weight_male_sumo, sd = 5),
  rnorm(n, mean_weight_female, sd = 5),
  rnorm(n, mean_weight_male_no_sumo, sd = 5),
  rnorm(n, mean_weight_female, sd = 5)
)

# Create a data frame
data <- data.frame(Gender = gender, Sumo = sumo, Weight = Y)


write_csv(data, file='/home/sam/Regis/PSY423/2wayANOVA_sumoExample.csv')
