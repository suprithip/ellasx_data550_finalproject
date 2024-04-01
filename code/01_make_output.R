here::i_am("code/01_make_output.R")

library(gtsummary)
library(gt)
library(ggplot2)
library(ggpubr)

# Regression model

diabetes <- read.csv(here::here("data/diabetes.csv"))

model <- glm(
  Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Age + Insulin + BMI + DiabetesPedigreeFunction, 
  data = diabetes,
  family = binomial()
)

regression_table <- tbl_regression(model, exponentiate = TRUE) |>
  add_global_p() |>
  as_gt() |>
  tab_header(title = "Table 1. Diabetes Regression Model")

saveRDS(
  regression_table,
  file = here::here("output/regression_table.rds")
)

# Histograms
diabetes$Outcome <- as.factor(diabetes$Outcome)

preg <- ggplot(diabetes, aes(x = Outcome, y = Pregnancies, fill = Outcome)) + 
  labs(title = "Number of Pregenancies") +
  geom_boxplot() + 
  theme(legend.position="right") +
  scale_fill_discrete(name = "Diabetes Outcome",
                      breaks=c(0,1),
                      labels = c("No Diabetes", "Diabetes"))

glu <- ggplot(diabetes, aes(x = Outcome, y = Glucose, fill = Outcome)) + 
  labs(title = "Blood Glucose Level", y = "Blood Glucose") +
  geom_boxplot() + 
  theme(legend.position="right") +
  scale_fill_discrete(name = "Diabetes Outcome",
                      breaks=c(0,1),
                      labels = c("No Diabetes", "Diabetes"))

bmi <- ggplot(diabetes, aes(x = Outcome, y = BMI, fill = Outcome)) + 
  labs(title = "Body Mass Index(BMI)") +
  geom_boxplot() + 
  theme(legend.position="right") +
  scale_fill_discrete(name = "Diabetes Outcome",
                      breaks=c(0,1),
                      labels = c("No Diabetes", "Diabetes"))

dpf <- ggplot(diabetes, aes(x = Outcome, y = DiabetesPedigreeFunction, fill = Outcome)) + 
  labs(title = "Diabetes Percentage", y = "Diabetes Percentage") +
  geom_boxplot() + 
  theme(legend.position="right") +
  scale_fill_discrete(name = "Diabetes Outcome",
                      breaks=c(0,1),
                      labels = c("No Diabetes", "Diabetes"))

figure <- ggarrange(preg, glu, bmi, dpf,
                    common.legend=TRUE,
                    font.label = list(size = 10),
                    legend = "right",
                    ncol = 2, nrow = 2)

annotate_figure(figure, top = text_grob("Figure 1. Boxplots of Diagnostic Measurements by Outcome", 
                                        face = "bold", size = 15))

ggsave(
  here::here("output/boxplots.png"),
  plot = last_plot(),
  device = "png"
)

