# 1. Programs to implement the following statistical tests:
# i) Correlation test between two variables
# ii) Correlation Matrix between multiple variables
# iii) Comparing the means of two groups
# iv) Comparing the means of more than two groups
# Objective: To explore correlation between different variables and groups.
# Program1.R

# i) Correlation test between two variables
# Sample data
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
# Pearson correlation test
cor_test_result <- cor.test(x, y, method = "pearson") # Pearson is default
print(cor_test_result)

# ii) Correlation Matrix between multiple variables
# Sample data
data <- data.frame(
var1 = c(1, 2, 3, 4, 5),
var2 = c(2, 4, 6, 8, 10),
var3 = c(5, 6, 7, 8, 9)
)
# Correlation matrix
cor_matrix <- cor(data, method = "pearson") # Pearson is default
print(cor_matrix)

# iii) Comparing the means of two groups
# Sample data
group1 <- c(1, 2, 3, 4, 5)
group2 <- c(2, 3, 4, 5, 6)
# Independent two-sample t-test
t_test_result <- t.test(group1, group2, var.equal = TRUE) # Set var.equal = FALSE
# if variances are unequal
print(t_test_result)

# iv) Comparing the means of more than two groups
# Sample data
group <- factor(c("A", "A", "B", "B", "C", "C"))
value <- c(1, 2, 3, 4, 5, 6)
# One-way ANOVA (Analysis of Variance)
anova_result <- aov(value ~ group)
summary(anova_result)
