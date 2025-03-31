

# OKTAY CAN SEVIMGIN

main_data <- read.delim("C:\\Users\\Oktay Can\\Desktop\\Dataset\\DatasetNA.txt", sep=" ", header=TRUE)

manual_quantile <- function(x, probs) {
  x <- x[!is.na(x)]
  sorted_x <- sort(x)
  n <- length(sorted_x)
  index <- floor((n + 1) * probs)
  if (index < 1) index <- 1
  if (index > n) index <- n
  return(sorted_x[index])
}

custom_boxplot <- function(data, main = "Custom Boxplot", xlab = "Variable", ylab = "Values", xlim = NULL, ylim = NULL, col = "lightblue") {
  data <- data[!is.na(data)]
  Q1 <- manual_quantile(data, 0.25)
  Q3 <- manual_quantile(data, 0.75)
  median_val <- manual_quantile(data, 0.5)
  IQR <- Q3 - Q1
  lower_whisker <- max(min(data[data >= (Q1 - 1.5 * IQR)]), Q1 - 1.5 * IQR)
  upper_whisker <- min(max(data[data <= (Q3 + 1.5 * IQR)]), Q3 + 1.5 * IQR)
  outliers <- data[data < lower_whisker | data > upper_whisker]
  
  if (is.null(ylim)) {
    ylim <- c(min(data) - 0.1 * (max(data) - min(data)), max(data) + 0.1 * (max(data) - min(data)))
  }
  if (is.null(xlim)) {
    xlim <- c(0, 2)
  }
  
  plot(xlim, ylim, type = "n", main = main, xlab = xlab, ylab = ylab, xaxt = "n")
  rect(0.5, Q1, 1.5, Q3, border = "red", col = col)
  lines(c(0.5, 1.5), c(median_val, median_val), col = "red", lwd = 2)
  lines(c(1, 1), c(lower_whisker, Q1), col = "red")
  lines(c(1, 1), c(Q3, upper_whisker), col = "red")
  lines(c(0.8, 1.2), c(lower_whisker, lower_whisker), col = "red")
  lines(c(0.8, 1.2), c(upper_whisker, upper_whisker), col = "red")
  
  if (length(outliers) > 0) {
    points(rep(1, length(outliers)), outliers, col = "red", pch = 19)
  }
  
  cat("Boxplot Statistics:\n")
  cat("Minimum:", min(data), "\n")
  cat("Q1:", Q1, "\n")
  cat("Median:", median_val, "\n")
  cat("Q3:", Q3, "\n")
  cat("Maximum:", max(data), "\n")
  cat("IQR:", IQR, "\n")
  cat("Lower Whisker:", lower_whisker, "\n")
  cat("Upper Whisker:", upper_whisker, "\n")
  cat("Number of Outliers:", length(outliers), "\n")
}

multi_custom_boxplot <- function(data, variables, main = "", xlab = "Variables", ylab = "Values", col = "lightblue") {
  n_plots <- length(variables)
  par(mfrow = c(1, n_plots))
  
  for (i in 1:n_plots) {
    var_name <- variables[i]
    custom_boxplot(data[[var_name]], main = paste(main, "-", var_name), xlab = var_name, ylab = ylab, col = col)
  }
  
  par(mfrow = c(1, 1))
}

categorical_custom_boxplot <- function(data, variable, category, main = "Categorical Boxplot", xlab = "Categories", ylab = "Values", col = NULL) {
  categories <- unique(data[[category]])
  categories <- categories[!is.na(categories)]
  
  if (is.null(col)) {
    col <- rep("lightblue", length(categories))
  }
  
  plot_data <- data[[variable]]
  ylim <- c(min(plot_data, na.rm = TRUE), max(plot_data, na.rm = TRUE))
  
  plot(c(0, length(categories) + 1), ylim, type = "n", main = paste(main, "-", variable, "by", category), xlab = category, ylab = variable, xaxt = "n")
  axis(1, at = 1:length(categories), labels = categories)
  
  for (i in 1:length(categories)) {
    cat_data <- data[[variable]][data[[category]] == categories[i]]
    cat_data <- cat_data[!is.na(cat_data)]
    
    Q1 <- manual_quantile(cat_data, 0.25)
    Q3 <- manual_quantile(cat_data, 0.75)
    median_val <- manual_quantile(cat_data, 0.5)
    IQR <- Q3 - Q1
    
    lower_whisker <- max(min(cat_data[cat_data >= (Q1 - 1.5 * IQR)]), Q1 - 1.5 * IQR)
    upper_whisker <- min(max(cat_data[cat_data <= (Q3 + 1.5 * IQR)]), Q3 + 1.5 * IQR)
    outliers <- cat_data[cat_data < lower_whisker | cat_data > upper_whisker]
    
    rect(i - 0.3, Q1, i + 0.3, Q3, border = "red", col = col[i])
    lines(c(i - 0.3, i + 0.3), c(median_val, median_val), col = "red", lwd = 2)
    lines(c(i, i), c(lower_whisker, Q1), col = "red")
    lines(c(i, i), c(Q3, upper_whisker), col = "red")
    lines(c(i - 0.15, i + 0.15), c(lower_whisker, lower_whisker), col = "red")
    lines(c(i - 0.15, i + 0.15), c(upper_whisker, upper_whisker), col = "red")
    
    if (length(outliers) > 0) {
      points(rep(i, length(outliers)), outliers, col = "red", pch = 19)
    }
    
    cat("Category:", categories[i], "\n")
    cat("Minimum:", min(cat_data), "\n")
    cat("Q1:", Q1, "\n")
    cat("Median:", median_val, "\n")
    cat("Q3:", Q3, "\n")
    cat("Maximum:", max(cat_data), "\n")
    cat("IQR:", IQR, "\n")
    cat("Outliers:", length(outliers), "\n\n")
  }
}

custom_boxplot(main_data$Var1, main = "Boxplot for Var1", xlab = "Var1", ylab = "Values", col = "lightblue")
custom_boxplot(main_data$Var2, main = "Boxplot for Var2", xlab = "Var2", ylab = "Values", col = "lightgreen")
custom_boxplot(main_data$Var3, main = "Boxplot for Var3", xlab = "Var3", ylab = "Values", col = "lightcoral")
custom_boxplot(main_data$Var4, main = "Boxplot for Var4", xlab = "Var4", ylab = "Values", col = "lightcoral")
custom_boxplot(main_data$Var5, main = "Boxplot for Var5", xlab = "Var5", ylab = "Values", col = "lightcoral")
custom_boxplot(main_data$Var6, main = "Boxplot for Var6", xlab = "Var6", ylab = "Values", col = "lightcoral")
custom_boxplot(main_data$Var7, main = "Boxplot for Var7", xlab = "Var7", ylab = "Values", col = "lightcoral")
custom_boxplot(main_data$Var8, main = "Boxplot for Var8", xlab = "Var8", ylab = "Values", col = "lightcoral")

par(mfrow = c(4, 2))
multi_custom_boxplot(main_data, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"), main = "BX.PLT")
par(mfrow = c(1, 1))

categorical_custom_boxplot(main_data, "Var1", "Gender", main = "Var1 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var2", "Gender", main = "Var2 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var3", "Gender", main = "Var3 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var4", "Gender", main = "Var4 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var5", "Gender", main = "Var5 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var6", "Gender", main = "Var6 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var7", "Gender", main = "Var7 Boxplot by Gender", col = c("lightblue", "lightpink"))
categorical_custom_boxplot(main_data, "Var8", "Gender", main = "Var8 Boxplot by Gender", col = c("lightblue", "lightpink"))
