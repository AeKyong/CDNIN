library(tidyverse)
library(ggplot2)
library(effectsize)

setwd("results")

nReplicationsPerCondition <- 3
file_list <- list.files(pattern = "rep_.*\\.RData")
nFiles <- length(file_list)
all_results <- list()

i=1
for(i in 1:nFiles){

  file_name <- paste0("rep_", i, ".RData")
  if(file.exists(file_name)){
    load(file_name)

    cond_info <- conditionInformation(i, nReplicationsPerCondition)
    tmp <- data.frame(
      rep_id = i,
      condition = cond_info$conditionNumber,
      lStructure = cond_info$lStructure,
      mainA = ifelse(cond_info$mainA.min == 0.75, "low", "high"),
      mainT = ifelse(cond_info$mainT.min == 0.3, "low", "high"),
      tetra = cond_info$tetra,
      nSamples = cond_info$nSamples,
      misconcept = cond_info$misconcept,
      nItems = cond_info$nItems,
      estimation = cond_info$estimationName,
      ARI = C,
      Bias = B,
      AbsBias = A
    )
    all_results[[i]] <- tmp
  }
}

final_df <- do.call(rbind, all_results)

# summary table
summary_table <- final_df %>%
  group_by(estimation, nSamples, lStructure, misconcept) %>%
  summarise(
    mean_ARI = mean(ARI, na.rm = TRUE),
    mean_Bias = mean(Bias, na.rm = TRUE),
    mean_AbsBias = mean(AbsBias, na.rm = TRUE),
    sd_ARI = sd(ARI, na.rm = TRUE),
    .groups = 'drop'
  )
print(summary_table)

# plot
ggplot(summary_table, aes(x = as.factor(nSamples), y = mean_ARI, color = estimation, group = estimation)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_grid(lStructure ~ misconcept, labeller = label_both) +
  labs(
    title = "Comparison of Community Recovery Accuracy (ARI)",
    x = "Sample Size",
    y = "Mean Adjusted Rand Index (ARI)",
    color = "Estimation Method"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# ANOVA
fit <- aov(ARI ~ estimation * nSamples * lStructure * misconcept, data = final_df)
summary(fit)

# effect size of ANOVA
eta_squared(fit)
