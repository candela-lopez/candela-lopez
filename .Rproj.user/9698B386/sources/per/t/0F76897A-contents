# Carga de datos y librerias --------------------------
library(readr)
library(naniar)
library(dplyr)
library(e1071)
library(caret)
library(class)
library(VIM)
library(nortest)

world_happiness_report <- read_csv("world-happiness-report.csv")
world_happiness_report_2021 <- read_csv("world-happiness-report-2021.csv")


# Data discovery --------------------------
str(world_happiness_report)
str(world_happiness_report_2021)
dim(world_happiness_report)
dim(world_happiness_report_2021)


#estadisticos descriptivos / variables no numericas
estadisticos_d <- data.frame(summary(world_happiness_report[, 1]))
estadisticos_d

#estadisticos numericos
estadisticos <- data.frame(summary(world_happiness_report[, -(1)]))
estadisticos <- estadisticos[-1]

#missings
miss_case_summary(world_happiness_report)
miss_var_summary(world_happiness_report)

gg_miss_var(world_happiness_report)
vis_miss(world_happiness_report)

sum_ausencias_pais <- world_happiness_report %>%
  mutate(var = is.na(.)) %>%
  group_by(`Country name`) %>%
  summarise(sum(var))

sum_ausencias_año <- world_happiness_report %>%
  mutate(var = is.na(.)) %>%
  group_by(year) %>%
  summarise(sum(var))


# Imputar ---------------
for (i in (3:11)){world_happiness_report <- kNN(world_happiness_report , 
                                                 colnames(world_happiness_report)[i] ,
                                                 dist_var= c("year" , "Country name"))
  }

vis_miss(world_happiness_report)

# Outliers ---------------

boxplot(world_happiness_report[,3:6])
boxplot(world_happiness_report[,7:12])
boxplot(world_happiness_report[,13:20])

#Si el número de valores es menor a 30 -> shapiro.test
#Si el número de valores es mayor a 30 -> lillie.test

lillie.test(world_happiness_report[,3])

# normalizar y discretizar ---------------














