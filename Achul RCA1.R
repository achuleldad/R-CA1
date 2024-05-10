# Set up the data frame
student_no <- c(1:17)
no_visual_aids_scores <- c(50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61)
with_visual_aids_scores <- c(58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70)

# Create the data frame
Students_scores <- data.frame(
  Student_id =  student_no,
  No_Visual_Aids = no_visual_aids_scores,
  With_Visual_Aids = with_visual_aids_scores
)
View(Students_scores)

library(ggplot2)

windows(20,10)
boxplot(Students_scores$No_Visual_Aids, Students_scores$With_Visual_Aids,
        names = c("Scores without Visual Aids", "Scores with Visual Aids"),
        main = "Impact of Visual Aids on Lecture Quality",
        ylab = "Students_Score out of 100",
        col = c("orange", "skyblue"))

# Load necessary library
install.packages("moments")
library(moments)

# Shapiro-Wilk Normality Test
shapiro_test_no_aids <- shapiro.test(Students_scores$No_Visual_Aids)
shapiro_test_with_aids <- shapiro.test(Students_scores$With_Visual_Aids)

#Descriptive Statistics
mean_no_aids <- mean(Students_scores$No_Visual_Aids)
median_no_aids <- median(Students_scores$No_Visual_Aids)
skewness_no_aids <- skewness(Students_scores$No_Visual_Aids)

mean_with_aids <- mean(Students_scores$With_Visual_Aids)
median_with_aids <- median(Students_scores$With_Visual_Aids)
skewness_with_aids <- skewness(Students_scores$With_Visual_Aids)

#standard deviation
sd_no_aids <- sd(Students_scores$No_Visual_Aids)
sd_with_aids <- sd(Students_scores$With_Visual_Aids)
print(sd_no_aids)
print(sd_with_aids)

#confidence Interval
n_no_aids <- length(Students_scores$No_Visual_Aids)
n_with_aids <- length(Students_scores$With_Visual_Aids)

se_no_aids <- sd_no_aids / sqrt(n_no_aids)
se_with_aids <- sd_with_aids / sqrt(n_with_aids)

# Assuming a t-distribution
t_value <- qt(0.7695, df=n_no_aids-1)
t_value <- qt(0.8065, df=n_no_aids-1)

ci_lower_no_aids <- mean_no_aids - t_value * se_no_aids
ci_upper_no_aids <- mean_no_aids + t_value * se_no_aids

ci_lower_with_aids <- mean_with_aids - t_value * se_with_aids
ci_upper_with_aids <- mean_with_aids + t_value * se_with_aids

# Print the results for No Visual Aids
cat("Confidence Interval for No Visual Aids:\n")
cat("Lower Bound:", ci_lower_no_aids, "\n")
cat("Upper Bound:", ci_upper_no_aids, "\n\n")

# Print the results for With Visual Aids
cat("Confidence Interval for With Visual Aids:\n")
cat("Lower Bound:", ci_lower_with_aids, "\n")
cat("Upper Bound:", ci_upper_with_aids, "\n")


# Print out results
print(paste("No Visual Aids - Mean:", mean_no_aids, "Median:", median_no_aids, "Skewness:", skewness_no_aids))
print(paste("With Visual Aids - Mean:", mean_with_aids, "Median:", median_with_aids, "Skewness:", skewness_with_aids))
print(shapiro_test_no_aids)
print(shapiro_test_with_aids)

# t-test
t_test_results <- t.test(Students_scores$With_Visual_Aids, Students_scores$No_Visual_Aids, paired = TRUE)
print(t_test_results)

library(psych)
windows(20,10)
pairs.panels(Students_scores,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "spearman",
             pch = 21,
             lm =FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)

