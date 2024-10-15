# Install and load packages
packages <- c("dplyr", "tidyr", "lme4", "lmerTest", "emmeans", "ggplot2",
              "ggpubr", "pwr", "performance", "patchwork", "effects")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)

# Load data
data_wide <- read.csv("vertical_jump_data_wide.csv")

# View data
head(data_wide)
str(data_wide)
summary(data_wide)

n <- nrow(data_wide)
n
sex_count <- data_wide %>% 
  count(sex)
print(sex_count)
mean(data_wide$age)
sd(data_wide$age)
  

data <- data_wide %>% 
  pivot_longer(
    cols = c(dynamic_pre, dynamic_post, static_pre, static_post,
             control_pre, control_post),
    names_to = c("condition", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>% 
  rename(pre_stretch = pre,
         post_stretch = post)

data$condition <- as.factor(data$condition)
data$condition <- factor(data$condition,
                         levels = c("static", "dynamic", "control"))

data <- data %>% 
  mutate(jump_diff = post_stretch - pre_stretch)

# View data
head(data)
str(data)
summary(data)

# Linear mixed-effects model
lmer_model <- lmer(jump_diff ~ condition + (1|sub_id), data = data)
summary(lmer_model)
lmer_model

effects_model <- allEffects(lmer_model)
print(effects_model)
plot(effects_model)
effects_model

lmer_null <- lmer(jump_diff ~ (1|sub_id), data = data)
summary(lmer_null)
lmer_null

# ANOVA of model and null
anova(lmer_model, lmer_null)

# post hoc Estimated marginal means
emmeans_results <- emmeans(lmer_model, pairwise ~ condition)
emmeans_results

model_performance <- performance::check_model(lmer_model)
print(model_performance)


# emmeans_results <- emmeans(lmer_model, ~ condition)
# pairwise_comparisons <- pairs(emmeans_results)
# pairwise_comparisons

# Box plot
#stat_compare_means(jump_diff ~ condition, data = data, method = "t.test")

ggplot(data, aes(x = condition, y = jump_diff, fill = condition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "white", "white")) +
  geom_point(position = position_jitterdodge()) +
  labs(title = "Jump Height Difference by Condition",
       x = "Condition",
       y = "Jump Height Difference (cm)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")
        ) +
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
        axis.text = element_text(size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")
  )

ggsave("barchart.pdf", width = 6, height = 4, units = "in", dpi = 300)

interaction_data <- data %>%
  gather(key = "time", 
         value = "jump_height", pre_stretch, post_stretch)

interaction_data
summary(interaction_data)

# Interaction plot

interaction_data$time <- factor(interaction_data$time,
                                levels = c("pre_stretch", "post_stretch"))

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
  labs(title = "Jump Height Pre and Post Stretch by Condition",
       x = "Time",
       y = "Jump Height (cm)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")
        )

ggsave("interaction.pdf", width = 6, height = 4, units = "in", dpi = 300)

# Define the means and standard deviations for each condition
row_before_means <- rowMeans(data_wide[, c("dynamic_pre", "static_pre", 
                                           "control_pre")])
mean_before <- mean(row_before_means)
mean_before
mean_after_static <- mean(data_wide$static_post)
mean_after_static
mean_after_dynamic <- mean(data_wide$dynamic_post)
mean_after_dynamic
mean_after_rest <- mean(data_wide$control_post)
mean_after_rest
combined_scores <- c(data_wide$static_post, 
                     data_wide$dynamic_post, data_wide$control_post)

mean_before_static <- mean(data_wide$static_pre)
mean_before_static
mean_before_dynamic <- mean(data_wide$dynamic_pre)
mean_before_dynamic
mean_before_rest <- mean(data_wide$control_pre)
mean_before_rest

sd_before <- sd(combined_scores)
sd_before

sd_after_static <- sd(data_wide$control_post)
sd_after_static
sd_after_dynamic <- sd(data_wide$dynamic_post)
sd_after_dynamic
sd_after_rest <- sd(data_wide$control_post)
sd_after_rest

sd_before_static <- sd(data_wide$control_pre)
sd_before_static
sd_before_dynamic <- sd(data_wide$dynamic_pre)
sd_before_dynamic
sd_before_rest <- sd(data_wide$control_pre)
sd_before_rest

# Calculate the effect sizes
effect_size_static <- (mean_before - mean_after_static) / sd_before
effect_size_dynamic <- (mean_before - mean_after_dynamic) / sd_before
effect_size_rest <- (mean_before - mean_after_rest) / sd_before

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
row_after_means <- rowMeans(data_wide[, c("dynamic_post",
                                          "static_post", "control_post")])
mean_after <- mean(row_after_means)

# Calculate the effect size (Cohen's d)
effect_size <- (mean_before - mean_after) / sd_before
effect_size
# Calculate the required sample size for a power of 0.8
required_sample_size <- pwr.t.test(d = effect_size, 
                                   power = 0.8, type = "paired", 
                                   alternative = "two.sided")$n

# Print the result
cat("Minimum number of participants needed:", 
    ceiling(required_sample_size), "\n")

write.csv(data_wide, "data_wide_output.csv")
write.csv(data, "data_output.csv")

library(dplyr)
report::cite_packages()
