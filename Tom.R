
data <- data.frame(
  Index = 1:20,
  Weight = c(4.32, 4.89, 5.80, 5.50, 6.03, 4.17, 4.61, 3.83, 4.17, 
             5.80, 5.50, 5.13, 6.17, 4.66, 4.43, 5.18, 6.57, 5.23, 4.88, 6.33),
  Group = c("trt2", "trt2", "trt1", "ctrl", "ctrl", "ctrl", "ctrl", "trt2", 
            "trt1", "trt2", "trt2", "trt1", "ctrl", "ctrl", "trt1", "trt2", "ctrl", "trt1", "trt1", "trt1")
)
# For ctrl group
shapiro.test(data$Weight[data$Group == "ctrl"])

# For trt1 group
shapiro.test(data$Weight[data$Group == "trt1"])

# For trt2 group
shapiro.test(data$Weight[data$Group == "trt2"])
anova_result <- aov(Weight ~ Group, data=data)
summary(anova_result)

