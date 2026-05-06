# clear envs ----
rm(list = ls())

# load packages ----
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(extrafont)
library(FSA)
library(MASS)
library(DHARMa)
loadfonts()

# load data----
df <- read.csv("Fig5FGHI_Muscle_and_Gonads_Final.csv")

str(df)
summary(df)

df$Morph <- as.factor(df$Morph)
df$Muscle_type <- as.factor(df$Muscle_type)
df$Ave_Wing_length.mm. <- as.numeric(df$Ave_Wing_length.mm.)
df$Egg_number <- as.numeric(df$Egg_number)
df$Box_ID <- as.factor(df$Box_ID)


# Fig 5F ----
df %>%
  group_by(Sex) %>%
  summarise(N = n())

# Statistical tests

df %>%
  group_by(Muscle_type) %>%
  summarise(p_value = shapiro.test(W_DLM.g.)$p.value)

# DLM
# Male
kruskal.test(W_DLM.g. ~ Muscle_type, data = subset(df, Sex == "M"))

dunnTest(W_DLM.g. ~ Muscle_type, data = subset(df, Sex == "M"), method="bh")

# Female
kruskal.test(W_DLM.g. ~ Muscle_type, data = subset(df, Sex == "F"))

dunnTest(W_DLM.g. ~ Muscle_type, data = subset(df, Sex == "F"), method="bh")

# DVM
# Male
kruskal.test(W_DVM.g.~ Muscle_type, data = subset(df, Sex == "M"))

dunnTest(W_DVM.g. ~ Muscle_type, data = subset(df, Sex == "M"), method="bh")

# Female
kruskal.test(W_DVM.g. ~ Muscle_type, data = subset(df, Sex == "F"))

dunnTest(W_DVM.g. ~ Muscle_type, data = subset(df, Sex == "F"), method="bh")

# Fig 5F ----
df$Muscle_type <- factor(df$Muscle_type, 
                         levels = c("Lw_F_Pink or Red", "Lw_M_Pink or Red", 
                                    "Sw_M_Pink", "Sw_F_Pink", # add two factor levels
                                    "Lw_M_White", "Sw_M_White",
                                    "Lw_F_White", "Sw_F_White" 
                         )) 

# DLM 

fig5F.DLM <- df%>%  
  ggplot(aes(x = Muscle_type, y = W_DLM.g.*1000, color = Muscle_type)) +
  geom_boxplot(linetype = "dashed",
               outlier.alpha = 0, width = 0.5) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.5) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.15) + 
  geom_jitter(width = 0.25, size = 2, alpha = 0.6) +
  xlab("") +
  ylab("DLM (mg)") +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 0,angle = -45),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("#9cc37b","#e97a42", "#d06128", "#afbde0", "#BBD6B1", "#106ab3")) +    ##e97a42", "#afbde0"    "#e49a5c", "#3f77a3"
  theme(text = element_text(family = "Arial", size = 20)) +
  theme(axis.ticks.x = element_blank()) +
  scale_x_discrete(drop = FALSE)

fig5F.DLM

# fig5F.DVM

fig5F.DVM <- df %>%
  ggplot(aes(x = Muscle_type, y = W_DVM.g.*1000, color = Muscle_type)) +
  geom_boxplot(linetype = "dashed",
               outlier.alpha = 0, width = 0.5) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.5) + 
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.15) + 
  geom_jitter(width = 0.25, size = 2, alpha = 0.6) +
  xlab("") +
  ylab("DVM (mg)") +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 12),
        legend.position = "none") +
  theme(axis.text.x = element_text(size = 0),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("#9cc37b","#e97a42", "#d06128", "#afbde0", "#BBD6B1", "#106ab3")) +    ##e97a42", "#afbde0"    "#e49a5c", "#3f77a3"
  theme(text = element_text(family = "Arial", size = 20)) +
  scale_x_discrete(drop = FALSE)

fig5F.DVM

fig.5F <- ggarrange(fig5F.DLM, fig5F.DVM,
                    nrow = 2,
                    align = "hv")

fig.5F

ggsave("Fig5F.pdf", fig.5F, dpi = 600, height = 6, width = 6)

# Fisher exact test: Do Lw males histolyze their muscles faster than females?
df %>%
  group_by(Muscle_type) %>%
  summarise(N = n())

muscle.his <- matrix(c(43, 6, 11, 38), nrow = 2, ncol =2)

fisher.test(muscle.his)

# Fig 5G ----
m.df <- df %>% filter(Sex == "M") 

m.df %>% filter(Morph == "Lw") -> Lw.df
m.df %>% filter(Morph == "Sw") -> Sw.df

shapiro.test(Lw.df$W_Gonads.g.) 
shapiro.test(Sw.df$W_Gonads.g.) 

var.test(Lw.df$W_Gonads.g., Sw.df$W_Gonads.g.)

t.test(W_Gonads.g. ~ Morph, data = m.df, var.equal = T) # p-value = 0.007348

fig.5G <-
  m.df %>%  
  ggplot(aes(x = Morph, y = W_Gonads.g.*1000, color = Morph)) +
  geom_boxplot(linetype = "dashed",
               outlier.alpha = 0, width = 0.35) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.35) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.1) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.1) + 
  geom_jitter(width = 0.15, size = 4, alpha = 0.7) +
  ylab("Testis mass (mg)") +
  xlab("") +
  ylim(c(16,38)) +
  theme_classic2() +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("#e97a42", "#afbde0")) +    
  theme(text = element_text(family = "Arial", size = 12)) 

fig.5G 

ggsave("fig.5G.pdf", fig.5G, dpi = 600, height = 6, width = 5)

# Fig 5H ----
df %>%  filter(Sex == "F") -> f.df

Fig.5H <- f.df %>%  
  ggplot(aes(x = Phenotype, y = W_Gonads.g.*1000, color = Phenotype)) +
  geom_boxplot(linetype = "dashed",
               outlier.alpha = 0, width = 0.35) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.35) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.1) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.1) + 
  geom_jitter(width = 0.15, size = 4, alpha = 0.7) +
  ylim(0, 300) +
  ylab("Ovary mass (mg)") +
  theme_classic2() +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("#7cc17b", "#106ab3")) +  
  theme(text = element_text(family = "Arial", size = 16)) +
  scale_x_discrete(labels = c("Lw_F" = "Lw(F)", "Sw_F" = "Sw(F)")) +
  xlab("")

Fig.5H


ggsave("Fig5H.pdf", Fig.5H, dpi = 600, width = 5, height = 6)

f.df %>% 
  group_by(Morph) %>%
  summarise(
    sample_size = n()
  )

df_summary.ovary <- f.df %>%
  group_by(Morph) %>%   
  summarise(
    mean_value = round(mean(W_Gonads.g.*1000, na.rm = TRUE), 2),
    se_value = round(sd(W_Gonads.g.*1000, na.rm = TRUE) / sqrt(n()), 2)
  )

df_summary.ovary

# Statistical tests
f.df %>%
  group_by(Morph) %>%
  summarise(p_value = shapiro.test(W_Gonads.g.)$p.value)

wilcox.test(W_Gonads.g. ~ Morph, data = f.df) 

# Fig 5I ----
fig5I <- f.df %>%  
  ggplot(aes(x = Phenotype, y = Egg_number, color = Phenotype)) +
  geom_boxplot(linetype = "dashed",
               outlier.alpha = 0, width = 0.35) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.35) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.1) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.1) + 
  geom_jitter(width = 0.15, size = 4, alpha = 0.7) +
  ylim(0, 500) +
  ylab("Egg numbers") +
  theme_classic2() +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("#7cc17b", "#106ab3")) +  
  theme(text = element_text(family = "Arial", size = 16)) +
  scale_x_discrete(labels = c("Lw_F" = "Lw(F)", "Sw_F" = "Sw(F)")) +
  xlab("")

fig5I

ggsave("Fig5I.pdf", fig5I, dpi = 600, width = 5, height = 6)

f.df %>%
  group_by(Morph) %>%
  summarise(p_value = shapiro.test(Egg_number)$p.value)

wilcox.test(Egg_number ~ Morph, data = f.df) 


f.df %>% 
  group_by(Morph) %>%
  summarise(
    sample_size = n()
  )

df_summary.egg <- f.df %>%
  group_by(Morph) %>% 
  summarise(
    mean_value = round(mean(Egg_number, na.rm = TRUE), 2),
    se_value = round(sd(Egg_number, na.rm = TRUE) / sqrt(n()), 2)
  )

df_summary.egg 

# Lw-1 = 48 # 1 individual has NA for egg number
# Sw = 38

# GLM model controlling for the body size effect----
# test overdisperson 
test.ov <- glm(Egg_number ~ Muscle_type + Pronotum.mm.,
               family = 'poisson',
               data = f.df)

testDispersion(test.ov)

simulationOutput <- simulateResiduals(fittedModel = test.ov, plot = F)

plot(simulationOutput)

# Negative binomial model
model.egg <- glm.nb(Egg_number ~ Muscle_type + Pronotum.mm., data = f.df)

summary(model.egg)

Anova(model.egg, type = "II")

# FigS10
newdat <- expand.grid(
  Pronotum.mm. = seq(min(f.df$Pronotum.mm.), max(f.df$Pronotum.mm.), length.out = 100),
  Muscle_type = unique(f.df$Muscle_type)
)

pred <- predict(model.egg, newdata = newdat, type = "link", se.fit = TRUE)


newdat$fit <- exp(pred$fit)
newdat$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
newdat$upr <- exp(pred$fit + 1.96 * pred$se.fit)

fig.S10 <- ggplot(f.df, aes(x = Pronotum.mm., y = Egg_number, color = Muscle_type)) +
  geom_point(alpha = 0.5,
             size = 2) +
  geom_ribbon(data = newdat,
              aes(x = Pronotum.mm., ymin = lwr, ymax = upr, fill = Muscle_type),
              inherit.aes = FALSE,
              alpha = 0.2, color = NA) +
  geom_line(data = newdat,
            aes(x = Pronotum.mm., y = fit, color = Muscle_type),
            linewidth = 1) +
  labs(y = "Egg number", x = "Body size (Pro)") + 
  theme_bw() + 
  theme(text = element_text(size = 16),
        legend.position = "top") +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(family = "Arial", size = 16)) +
  scale_color_manual(values = c("#7cc17b", "#9cc37b","#106ab3")) 

fig.S10

ggsave("figS11_.png", fig.S10, dpi = 600, height = 5, width = 7)

sessionInfo()
# R version 4.3.0 (2023-04-21)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 26.4
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Asia/Shanghai
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] ggeffects_1.5.2 rstatix_0.7.2   multcomp_1.4-25 TH.data_1.1-2  
# [5] survival_3.6-4  mvtnorm_1.2-4   emmeans_1.11.1  DHARMa_0.4.6   
# [9] MASS_7.3-60.0.1 lmerTest_3.1-3  lme4_1.1-35.5   Matrix_1.6-5   
# [13] FSA_0.9.5       extrafont_0.19  cowplot_1.1.3   ggpmisc_0.5.5  
# [17] ggpp_0.5.6      ggpubr_0.6.0    lubridate_1.9.3 forcats_1.0.0  
# [21] stringr_1.5.1   purrr_1.0.2     readr_2.1.5     tidyr_1.3.1    
# [25] tibble_3.2.1    tidyverse_2.0.0 dplyr_1.1.4     ggplot2_3.5.1  
# 
# loaded via a namespace (and not attached):
# [1] tidyselect_1.2.1    farver_2.1.2        fastmap_1.1.1      
# [4] promises_1.3.0      digest_0.6.37       mime_0.12          
# [7] timechange_0.3.0    estimability_1.5    lifecycle_1.0.4    
# [10] magrittr_2.0.3      compiler_4.3.0      rlang_1.1.4        
# [13] tools_4.3.0         utf8_1.2.4          ggsignif_0.6.4     
# [16] labeling_0.4.3      plyr_1.8.9          gap.datasets_0.0.6 
# [19] abind_1.4-5         withr_3.0.1         numDeriv_2016.8-1.1
# [22] grid_4.3.0          fansi_1.0.6         dunn.test_1.3.6    
# [25] xtable_1.8-4        colorspace_2.1-1    extrafontdb_1.0    
# [28] iterators_1.0.14    scales_1.3.0        insight_1.3.1      
# [31] cli_3.6.3           crayon_1.5.2        generics_0.1.3     
# [34] rstudioapi_0.16.0   tzdb_0.4.0          minqa_1.2.6        
# [37] polynom_1.4-1       splines_4.3.0       parallel_4.3.0     
# [40] vctrs_0.6.5         boot_1.3-30         sandwich_3.1-0     
# [43] SparseM_1.81        carData_3.0-5       car_3.1-2          
# [46] hms_1.1.3           qgam_1.3.4          foreach_1.5.2      
# [49] gap_1.5-3           glue_1.7.0          nloptr_2.0.3       
# [52] codetools_0.2-20    stringi_1.8.4       gtable_0.3.5       
# [55] later_1.3.2         munsell_0.5.1       pillar_1.9.0       
# [58] htmltools_0.5.8.1   quantreg_5.97       R6_2.5.1           
# [61] Rdpack_2.6          doParallel_1.0.17   shiny_1.8.1.1      
# [64] lattice_0.22-6      rbibutils_2.2.16    backports_1.4.1    
# [67] broom_1.0.5         httpuv_1.6.15       MatrixModels_0.5-3 
# [70] Rcpp_1.0.13         coda_0.19-4.1       nlme_3.1-164       
# [73] Rttf2pt1_1.3.12     mgcv_1.9-1          zoo_1.8-12         
# [76] pkgconfig_2.0.3    