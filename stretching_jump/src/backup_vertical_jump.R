# Install and load packages
packages <- c("dplyr", "tidyr", "lme4", "emmeans", "ggplot2", "pwr")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)

# Load data
data <- read.csv("vertical_jump_data.csv")

data$condition <- as.factor(data$condition)

data <- data %>% 
  mutate(jump_diff = post_jump_height - pre_jump_height)

# View data
head(data)
str(data)
summary(data)

# Linear mixed-effects model
lmer_model <- lmer(jump_diff ~ condition + (1|sub_id), data = data)
summary(lmer_model)

# post hoc Estimated marginal means
emmeans_results <- emmeans(lmer_model, pairwise ~ condition)
emmeans_results

# Box plot
ggplot(data, aes(x = condition, y = jump_diff, fill = condition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "white", "white")) +
  geom_point(position = position_jitterdodge()) +
  labs(title = "Box and Whisker Plot Jump Height Difference by Condition",
       x = "Condition",
       y = "Post Jump Height (cm)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  guides(fill = "none")

ggsave("boxplot.pdf", width = 6, height = 4, units = "in", dpi = 300)

# Bar chart with error bars
summary_data <- data %>%
  group_by(condition) %>%
  summarise(mean_diff = mean(jump_diff),
            se_diff = sd(jump_diff) / sqrt(n()))

ggplot(summary_data, aes(x = condition, y = mean_diff)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(
    ymin = mean_diff - se_diff, 
    ymax = mean_diff + se_diff), width = 0.2) +
  labs(title = "Bar Chart with Error Bars of Jump Height Differences",
       x = "Condition",
       y = "Mean Jump Height Difference (cm)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

ggsave("barchart.pdf", width = 6, height = 4, units = "in", dpi = 300)

# Interaction plot
data_long <- reshape(data, 
                     varying = list(c("pre_jump_height", "post_jump_height")),
                     v.names = "jump_height", 
                     timevar = "time", 
                     times = c("before", "after"), 
                     direction = "long")

data_long$time <- factor(data_long$time, levels = c("before", "after"))

interaction_data <- data %>%
  gather(key = "time", 
         value = "jump_height", pre_jump_height, post_jump_height)

interaction_data$time <- factor(data_long$time, levels = c("before", "after"))

interaction_data
summary(interaction_data)

ggplot(interaction_data, aes(
  x = time, y = jump_height, color = condition, group = condition)) +
  geom_line(aes(
    linetype = condition), 
    stat = "summary", fun = mean, color = "black") +
  geom_point(aes(
    shape = condition), 
    stat = "summary", fun = mean, size = 3, color = "black") +
  geom_errorbar(
    stat = "summary", fun.data = mean_se, width = 0.1, color = "black") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  labs(title = "Interaction Plot of Jump Height Before and After by Condition",
       x = "Time",
       y = "Jump Height (cm)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("interaction.pdf", width = 6, height = 4, units = "in", dpi = 300)

################################################################################

# Define the means and standard deviations for each condition
mean_before <- c(47.1, 47.1, 47.1)  # Example means before stretching
mean_after_static <- 45.0  # Example mean after static stretching
mean_after_dynamic <- 48.5  # Example mean after dynamic stretching
mean_after_rest <- 47.0  # Example mean after rest
sd_before <- 9.7  # Example standard deviation before stretching
sd_after_static <- 9.2  # Example standard deviation after static stretching
sd_after_dynamic <- 9.5  # Example standard deviation after dynamic stretching
sd_after_rest <- 9.6  # Example standard deviation after rest

# Define the number of participants
n <- 16  # Example number of participants

# Calculate the effect sizes
effect_size_static <- (mean_before[1] - mean_after_static) / sd_before
effect_size_dynamic <- (mean_before[2] - mean_after_dynamic) / sd_before
effect_size_rest <- (mean_before[3] - mean_after_rest) / sd_before

# Calculate the power for each condition
power_static <- pwr.t.test(d = effect_size_static, 
                           n = n, type = "paired", 
                           alternative = "two.sided")$power
power_dynamic <- pwr.t.test(d = effect_size_dynamic, 
                            n = n, type = "paired", 
                            alternative = "two.sided")$power
power_rest <- pwr.t.test(d = effect_size_rest, 
                         n = n, type = "paired", 
                         alternative = "two.sided")$power

# Print the results
cat("Power for static stretching:", power_static, "\n")
cat("Power for dynamic stretching:", power_dynamic, "\n")
cat("Power for rest:", power_rest, "\n")


# Define the effect size
mean_before <- 47.1  # Example mean before stretching
mean_after <- 45.0  # Example mean after stretching
sd_before <- 9.7  # Example standard deviation before stretching

# Calculate the effect size (Cohen's d)
effect_size <- (mean_before - mean_after) / sd_before

# Calculate the required sample size for a power of 0.8
required_sample_size <- pwr.t.test(d = effect_size, 
                                   power = 0.8, type = "paired", 
                                   alternative = "two.sided")$n

# Print the result
cat("Minimum number of participants needed:", 
    ceiling(required_sample_size), "\n")
