rm(list = c())
devtools::load_all(".")
library(tidyverse)
library(ggplot2)
library(effectsize)
library(dplyr)

# setwd("results")

nReplicationsPerCondition <- 100
file_list <- list.files(pattern = "rep_.*\\.RData")

#check numbers
numbers <- as.numeric(gsub("rep_(\\d+)\\.RData", "\\1", file_list))

# 2. Define the full expected range
expected_range <- 1:216000

# 3. Check for completeness
missing_numbers <- setdiff(expected_range, numbers)
extra_numbers <- setdiff(numbers, expected_range) # In case there are numbers > 64800

nFiles <- length(file_list)
all_results <- list()

i=1
for(i in expected_range){

  file_name <- paste0("rep_", i, ".RData")
  print(file_name)
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
      undirectedRule = cond_info$undirectedRule,
      ARI = ARI,
      Correct = C,
      Bias = B,
      AbsBias = A
    )
    all_results[[i]] <- tmp
  }
}

final_df <- do.call(rbind, all_results)
write.csv(final_df, "final_df1-216000.csv")
final_df <- read.csv("final_df1-216000.csv")

# summary table
summary_table_temp <- final_df %>%
  group_by(lStructure, mainA, mainT, tetra, nSamples, misconcept, nItems, estimation, undirectedRule) %>%
  summarise(
    mean_ARI = mean(ARI, na.rm = TRUE),
    mean_Correct = mean(Correct, na.rm = TRUE),
    mean_Bias = mean(Bias, na.rm = TRUE),
    mean_AbsBias = mean(AbsBias, na.rm = TRUE),
    sd_ARI = sd(ARI, na.rm = TRUE),
    sd_Correct = sd(Correct, na.rm = TRUE),
    sd_Bias = sd(Bias, na.rm = TRUE),
    sd_AbsBias = sd(AbsBias, na.rm = TRUE),
    se_ARI = sd_ARI/sqrt(100),
    se_Correct = sd_Correct/sqrt(100),
    se_Bias = sd_Bias/sqrt(100),
    se_AbsBias = sd_AbsBias/sqrt(100),
    .groups = 'drop'
  )

# duplicate "zeroOrder" & "AND" to make row for "zeroOrder" & "OR"
zeroOrder_data = summary_table_temp[which(summary_table_temp$estimation == "zeroOrder"),]
zeroOrder_or = zeroOrder_data
zeroOrder_or[,"undirectedRule"] = "OR"
summary_table = rbind(summary_table_temp, zeroOrder_or)

# Plot ------------------------------------------------#

# 1. Prepare and filter the data
nlStructure = unique(summary_table$lStructure)
nSamplesLevels = unique(summary_table$nSamples)
nUndirectedRules = unique(summary_table$undirectedRule)
method_colors <- c(
  "zeroOrder"      = "darkgrey", # Red/Orange
  "nonRegularized" = "orange", # Blue
  "regularized"    = "blue"  # Green
)


# Reshape data: turn separate columns into 'metric' and 'value' columns
long_data <- summary_table %>%
  pivot_longer(
    cols = c(mean_ARI, mean_Correct, mean_Bias, mean_AbsBias,
             se_ARI, se_Correct, se_Bias, se_AbsBias),
    names_to = c(".value", "metric"),
    names_sep = "_"
  )

long_data <- long_data %>%
  mutate(
    lower_limit = mean - (1.96 * se),
    upper_limit = mean + (1.96 * se)
  )

# 3. Define metrics to loop through
metrics_to_plot <- unique(long_data$metric)
# Define color scheme for methods
method_colors <- c(
  "zeroOrder"      = "darkgrey",
  "nonRegularized" = "orange",
  "regularized"    = "blue"
)

for(str in nlStructure){
  for (i in nSamplesLevels) {
    for (j in nUndirectedRules) {
      for (m in metrics_to_plot) {

        plot_data <- long_data %>%
          filter(lStructure == str) %>%
          filter(nSamples == i) %>%
          filter(undirectedRule == j) %>%
          filter(metric == m) %>%
          mutate(
            misconcept = factor(misconcept, levels = c("low", "medium", "high")),
            tetra = factor(tetra, levels = c(0, 0.35, 0.7)),
            mainA = factor(mainA, levels = c("low", "high")),
            mainT = factor(mainT, levels = c("low", "high")),
            nItems = factor(nItems, levels = c(10, 20), labels = c("10 Items", "20 Items")),
            estimation = factor(estimation, levels = c("zeroOrder", "nonRegularized", "regularized"))
          )

        if (nrow(plot_data) > 0) {

          p <- ggplot(plot_data, aes(x = misconcept, y = mean,
                                     color = estimation,
                                     linetype = nItems,
                                     group = interaction(estimation, nItems))) +
            # Lines and Points for the mean
            geom_line(linewidth = 0.8) +
            geom_point(size = 1.5) +
            # --- ERROR BARS ---
            geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit),
                          width = 0.2, alpha = 0.5) +
            # ------------------
          facet_grid(mainA + mainT ~ tetra, labeller = label_both) +
            scale_color_manual(values = method_colors) +
            scale_linetype_manual(values = c("dotted", "solid")) +
            # Removed fixed y-limit, let ggplot scale per metric if necessary,
            # or add a conditional scale_y_continuous here.
            theme_bw() +
            labs(
              # title = paste(m, "| Struct:", str, "| Samples:", i, "| Rule:", j),
              x = "Misconception Level",
              y = m,
              color = "Estimation Method",
              linetype = "Number of Items"
            ) +
            theme(
              legend.position = "bottom",
              strip.text = element_text(face = "bold", size = 8)
            )

          fileName = paste0("plot_", m, "_str_", str, "_n_", i, "_rule_", j, ".png")
          ggsave(filename = fileName, plot = p, width = 12, height = 10, dpi = 300, bg = "white")

        }
      }
    }
  }
}

# ANOVA
fit <- aov(ARI ~ tetra * nSamples * misconcept * nItems * estimation , data = final_df)
summary(fit)

# effect size of ANOVA
eta_squared(fit)
