#' Perform One-Way Statistical Analysis and Plotting
#'
#' This function performs one-way ANOVA or Kruskal-Wallis tests
#' (depending on whether the data meet normality and variance assumptions)
#' on the specified dependent variable, grouped by the factor variable.
#' It outputs a bar plot with standard error bars and compact letter displays (CLD)
#' indicating statistical significance.
#' @param df A data frame in a long format with columns for dependent and factor variables.
#' @param dependent_var Written in "", the name of the dependent variable (as a string)must be numeric).
#' @param factor_var Written in "", the name of the factor variable as a string.
#' @param df_file_name Written in "", a string specifying the output file name for a summary data frame; preferably .csv or .txt.
#' @param logFile Written in "", a string specifying the output file name with statistics report; in .txt format.
#' @return A list with the following components:
#' \describe{
#'   \item{summary_df}{A data frame summarizing the analysis, including cld letters.}
#'   \item{plot}{A ggplot2 object representing the plotted dependent_var vs factor_var.}
#'   \item{summary_df file}{A file containing summary_df}
#'   \item{file with stat report}{A file containing a report from the performed statistical analyses.}
#' }
#' @importFrom plyr ddply
#' @importFrom car leveneTest
#' @importFrom FSA dunnTest
#' @importFrom multcompView multcompLetters multcompLetters4
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar geom_text theme position_dodge element_text
#' @importFrom stats TukeyHSD aov as.formula bartlett.test kruskal.test lm na.omit sd shapiro.test
#' @importFrom utils capture.output write.table
#' @importFrom dplyr arrange desc %>%
#' @importFrom rlang sym
#' @export



cedR <- function(df, dependent_var, factor_var, df_file_name, logFile) {


  if (!dependent_var %in% colnames(df)) {
    stop(paste("Dependent variable", dependent_var, "does not exist in the data frame."))
  }
  if (!factor_var %in% colnames(df)) {
    stop(paste("Factor variable", factor_var, "does not exist in the data frame."))
  }

  se <- function(x) {
    sd(x) / sqrt(length(x))
  }

  data_summary <- function(data, varname, groupnames) {
    summary_func <- function(x, col) {
      c(mean = mean(x[[col]], na.rm = TRUE),
        se = se(x[[col]], na.rm = TRUE),
        sd = sd(x[[col]], na.rm = TRUE))
    }
    data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
    colnames(data_sum)[colnames(data_sum) == "mean"] <- varname  # Ensure renaming only if "mean" exists
    return(data_sum)
  }






  # Perform summary statistics
  summary_df <- data_summary(df, varname = dependent_var, groupnames = factor_var) %>%
    arrange(desc(get(dependent_var)))

  # Perform Shapiro-Wilk test for normality
  df_cleaned <- na.omit(df[[dependent_var]])
  shapiro_test_result <- if (length(df_cleaned) >= 3 && length(df_cleaned) <= 5000) shapiro.test(df_cleaned) else NULL

  # Perform a test for equality of variances
  var_test_result <- if (!is.null(shapiro_test_result) && shapiro_test_result$p.value >= 0.05) {
    bartlett.test(as.formula(paste(dependent_var, "~", factor_var)), data = df)
  } else {
    leveneTest(as.formula(paste(dependent_var, "~", factor_var)), data = df)
  }

  # Open the log file for appending
  cat(paste0("Statistic output for ", dependent_var, " "), file = logFile, append = FALSE, sep = "\n")

  # Log the test results
  if (!is.null(shapiro_test_result)) {
    cat("Shapiro-Wilk Test:\n", file = logFile, append = TRUE)
    cat(capture.output(shapiro_test_result), file = logFile, append = TRUE, sep = "\n")
  }
  if (!is.null(var_test_result)) {
    cat("Variance Test:\n", file = logFile, append = TRUE)
    cat(capture.output(var_test_result), file = logFile, append = TRUE, sep = "\n")
  }

  # Determine the test to use based on normality and equality of variance
  if (!is.null(shapiro_test_result) && shapiro_test_result$p.value < 0.05 ||
      !is.null(var_test_result) && var_test_result$p.value < 0.05) {
    # Non-parametric test (Kruskal-Wallis + Dunn's Test)
    test <- kruskal.test(as.formula(paste(dependent_var, "~", factor_var)), data = df)
    Phocdunns <- dunnTest(as.formula(paste(dependent_var, "~", factor_var)), data = df, method = "holm")

    Diff <- Phocdunns$res$P.adj < 0.05
    Names <- gsub(" ", "", Phocdunns$res$Comparison)
    names(Diff) <- Names
    cld <- multcompLetters(Diff)

    cld_df <- data.frame(factor_var = names(cld$Letters), cld_letters = cld$Letters, stringsAsFactors = FALSE)

    cat("Kruskal-Wallis Test:\n", file = logFile, append = TRUE)
    cat(capture.output(test), file = logFile, append = TRUE, sep = "\n")

    cat("Dunn's Test:\n", file = logFile, append = TRUE)
    cat(capture.output(Phocdunns), file = logFile, append = TRUE, sep = "\n")
  } else {
    # Parametric test with ANOVA (Tukey's Test)
    lm_model <- lm(as.formula(paste(dependent_var, "~", factor_var)), data = df)
    res.aov2 <- aov(lm_model)
    Tukey <- TukeyHSD(res.aov2)

    cld <- multcompLetters4(res.aov2, Tukey)
    cld_df <- data.frame(factor_var = names(cld[[factor_var]]$Letters), cld_letters = cld[[factor_var]]$Letters, stringsAsFactors = FALSE)

    cat("ANOVA Test:\n", file = logFile, append = TRUE)
    cat(capture.output(res.aov2), file = logFile, append = TRUE, sep = "\n")

    cat("Tukey's Test:\n", file = logFile, append = TRUE)
    cat(capture.output(Tukey), file = logFile, append = TRUE, sep = "\n")
  }

  # Prepare final output
  colnames(cld_df)[colnames(cld_df) == "factor_var"] <- factor_var
  summary_df <- merge(summary_df, cld_df, by = factor_var, all = TRUE)
  write.table(summary_df, file = df_file_name, sep = "\t", row.names = FALSE, col.names = TRUE)

  # Create and display the plot

  plot <- ggplot(summary_df, aes(y = !!sym(dependent_var), x = !!sym(factor_var))) +
    geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 0.5) +
    geom_errorbar(aes(ymin = !!sym(dependent_var) -se,
                      ymax = !!sym(dependent_var) +se),
                  width = 0.2, position = position_dodge(0.9)) +
    geom_text(aes(label = cld_letters,
                  y = !!sym(dependent_var) +se),
              position = position_dodge(0.9)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  print(plot)

  cat("\nThe plot displays standard errors.\n")
  cat("\n")

  return(list(summary_df = summary_df, plot = plot))
}

