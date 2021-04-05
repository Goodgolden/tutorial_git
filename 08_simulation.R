## packages --------------------------------------------------------------------
library("tidymodels")
library("tidyverse")
library("here")
library("janitor")

## table1 and formates
library("knitr")
library("gtsummary")
library("flextable")

## simulation ------------------------------------------------------------------
set.seed(555)
Randomize <- sample(c("Control", "Intervention"),
                    size = 500,
                    replace = TRUE)
Binary <- rbinom(n = 500, size = 1, prob = 0.25)
Normal <- rnorm(n = 500)
Category <- sample(c("I", "II", "III"),
                  size = 500,
                  replace = TRUE)
Poisson <- rpois(n = 500, lambda = 5)
Exponential <- rexp(n = 500, rate = 5) 
data <- data.frame(Randomize, Binary, Normal, 
                   Category, Poisson, Exponential) 
data$Outcome <- Poisson * I(Randomize == "Control") + Normal


### Wed Mar 17 13:29:27 2021 ------------------------------
write.csv(data, "simulation_data.csv")
data0 <- read.csv("simulation_data.csv", row.names = 1)
# View(data0)



## table1 ----------------------------------------------------------------------
table1 <-  
  gtsummary::tbl_summary(
    data = data0,
    by = Randomize,
    statistic = list(all_continuous() ~ 
                       "{mean}({sd}) ; {median}({p25}, {p75})"))
table1



## modification ----------------------------------------------------------------
table1_1 <- add_p(table1)
table1_2 <- bold_p(table1_1)
table1_3 <- add_overall(table1_2)
table1_3



## add customized statistics ---------------------------------------------------
### Wed Mar 17 13:28:39 2021 ------------------------------
#' @description a function to calculate cohen's d
#' @param data the dataset contain the vector
#' @param variable the interested variable
#' @param by the group variable
cohen <- function(data, variable, by, ...) {
  # Cohen's D
  effect <- t.test(data[[variable]] ~ as.factor(data[[by]]))$statistic 
  effect <- abs(round(effect, 2))
  return(effect)
}

table1_4 <- 
  add_stat(table1_3,
           location = "level",
           ## the customized function
           fns = all_continuous() ~ cohen,
           header = "**Cohen'D**") 
table1_4



## print -----------------------------------------------------------------------
table1_5 <- flextable::autofit(as_flex_table(table1_4))
table1_5
table1_6 <- flextable::theme_tron_legacy(table1_5)
table1_6



## cross table -----------------------------------------------------------------
table2 <- 
  tbl_cross(data = data0,
            row = Category,
            col = Randomize,
            percent = "cell")
table2_1 <- add_p(table2)
table2_2 <- as_flex_table(table2_1)
table2_3 <- flextable::autofit(table2_2)
table2_4 <- flextable::theme_zebra(table2_3)
table2_4


