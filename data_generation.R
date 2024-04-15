# 0. Initialization----------------------------------------------------------------------
set.seed(2)

# 1. Set variables----------------------------------------------------------------------
n <- 500

Gender <- sample(c("Male", "Female"),
                 n, replace = TRUE, prob = c(0.35, 0.65))

Age <- round(rnorm(n, mean = 30, sd = 10))
Age <- ifelse(Age < 18, 18, ifelse(Age > 63, 63, Age))

Ethnicity <- sample(c("African", "American", "Asian", "European"),
                    n, replace = TRUE)

Neuroticism <- rnorm(n, mean = 3, sd = 1)
Neuroticism <- ifelse(Neuroticism < 1, 1, ifelse(Neuroticism > 5, 5, Neuroticism))

Conscientiousness <- rnorm(n, mean = 3, sd = 1)
Conscientiousness <- ifelse(Conscientiousness < 1, 1, ifelse(Conscientiousness > 5, 5, Conscientiousness))

Depression <- rnorm(n, mean = 5, sd = 2)
Depression <- ifelse(Depression < 1, 1, ifelse(Depression > 10, 10, Depression))

Self_Harm <- rpois(n, lambda = exp(-3 + 0.5 * Neuroticism - 0.3 * Conscientiousness + 0.2 * Neuroticism * (5 - Conscientiousness)))
Self_Harm <- ifelse(Gender == "Male", Self_Harm + rpois(1, lambda = 0.3), Self_Harm)

# 2. Moderate variables ---------------------------------------------------
Neuroticism <- ifelse(Ethnicity == "Asian", Neuroticism + rnorm(1, mean = 0.5, sd = 0.2),
                      ifelse(Ethnicity == "European", Neuroticism - rnorm(1, mean = 0.5, sd = 0.2), Neuroticism))
Conscientiousness <- ifelse(Ethnicity == "Asian", Conscientiousness + rnorm(1, mean = 0.5, sd = 0.2),
                            ifelse(Ethnicity == "European", Conscientiousness + rnorm(1, mean = 0.3, sd = 0.2), Conscientiousness))

Depression <- Depression + 0.6 * Neuroticism + rnorm(1, mean = 0, sd = 1)

Depression <- ifelse(Gender == "Female", Depression + rnorm(1, mean = 0.5, sd = 0.2), Depression)

# 3. Collate into data ----------------------------------------------------
data <- data.frame(Gender, Age, Ethnicity, Neuroticism, Conscientiousness, Depression, Self_Harm)

head(data)

write.csv(data, file = "data.csv", row.names = FALSE)
