library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(vegan)

prot_expr <- 
  read_xls('/home/stephen/Git_Repositories/BI_2019_Stats/Stat_project_3/Data_Cortex_Nuclear.xls')

length(prot_expr$MouseID)
str(prot_expr)
summary(prot_expr)


nrow(prot_expr) - length(which(rowSums(is.na(prot_expr)) > 0))
rowSums(is.na(prot_expr_num_omited))
rowSums(is.na(prot_expr_prots))

prot_expr_chars <- prot_expr %>% select_if(is.character)
apply(prot_expr_chars[,-1], MARGIN = 2, function(x) table(x))


qplot(prot_expr$BDNF_N, data = prot_expr, geom = 'histogram')

pairwise.t.test(prot_expr$BDNF_N, prot_expr$class, p.adjust.method = 'bonferroni')

prot_expr_num <- prot_expr %>% select_if(is.numeric)
prot_expr_omited <- na.omit(prot_expr_num)

model_full <- lm(ERBB4_N ~ ., prot_expr_omited)
model_null <- lm(ERBB4_N ~ 1, prot_expr_omited)
optimal_model <- step(model_full, scope = list(lower = model_null, upper = model_full))

mod_diag <- fortify(optimal_model)
gg_resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red")

gg_resid

metric <- 4/nrow(prot_expr_omited)

ggplot(mod_diag, aes(x = 1:nrow(mod_diag), y = .cooksd)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = metric, color = "red")

durbinWatsonTest(optimal_model)


# prot_expr_pca <- rda(prot_expr_num_omited, scale = TRUE)
prot_expr_pca <- rda(prot_expr_omited, scale = TRUE)
res <- summary(prot_expr_pca)

biplot(prot_expr_pca, scaling = "species", display = "species")
