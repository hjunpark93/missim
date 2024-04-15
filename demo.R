# 0. Read data ------------------------------------------------------------
data <- read.csv("data.csv", header = TRUE)
numeric.columns <- c("Age", "Neuroticism", "Conscientiousness", "Depression", "Self_Harm")
data <- data[,numeric.columns] # Only numeric columns

# 1. Specify models -------------------------------------------------------
model <- 'Depression ~ 0.4*Neuroticism - 0.6Conscientiousness
          Neuroticism ~ 0.9Neuroticism'

prop <- '0.8 ~ 1.0; 0.1;
0.9 ~ 1.0; 0.1'

# 2. Run functions --------------------------------------------------------

parsed.model_check <- parse_mis(data = data, model = model)
parsed.vars_check <- parse_var(data = data, model = model)
parsed.prop_check <- parse_prop(prop = prop)

mis.list_check <- mis_list(data = data, parsed.model = parsed.model_check,
                           input = "cov")

mis.prop_check <- mis_prop(data, model, prop, input = "cov")

check.vector <- c('Neuroticism', 'Neuroticism')
mis_plot(data = data, var.vector = check.vector, mis.matrix = mis.prop_check)


