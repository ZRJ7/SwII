# Clean env
rm(list = ls())

# loading packages -----
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(readr)
library(extrafont)
font_import()
loadfonts()

# Input data ----
juv_wing <- read_csv("Fig4B&C_Juv_Morphology_Final.csv")

juv_wing$Morph <- as.factor(juv_wing$Morph)
juv_wing$Sex_Morph <- as.factor(juv_wing$Sex_Morph)
juv_wing$Sex <- as.factor(juv_wing$Sex)

# fore-wing 
fw.1 <- juv_wing %>%
  ggplot(aes(x = Morph, y = Juv_Forewing_Length, color = Sex_Morph)) +
  geom_boxplot(linetype = "dashed", outlier.alpha = 0, width = 0.4) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.4) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.15) + 
  geom_jitter(shape=16,position = position_jitter(0.2), 
              alpha = 0.6, size = 1) +
  ylim(1,8) +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  xlab("") +
  ylab("Forewing length (mm) ") +
  facet_grid(~Sex, scales = "free_x") +
  scale_color_manual(values = c("#9cc37b", "#106ab3","#e97a42", "#afbde0")) + 
  theme(text = element_text("Arial",size = 16),
        legend.position = "none")

fw.1

# hindwing
hw.1 <- juv_wing %>%
  ggplot(aes(x = Morph, y = Juv_Hindwing_Length, color = Sex_Morph) ) +
  geom_boxplot(linetype = "dashed", outlier.alpha = 0, width = 0.4) +
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0, width = 0.4) + 
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.15) + 
  geom_jitter(shape=16,position = position_jitter(0.2),   
              alpha = 0.6, size = 1) +
  ylim(1,8) +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  xlab("") +
  ylab("Hindwing length (mm) ") +
  facet_grid(~Sex, scales = "free_x") +
  scale_color_manual(values = c("#9cc37b", "#106ab3","#e97a42", "#afbde0")) + 
  theme(text = element_text("Arial",size = 16),
        legend.position = "none")

hw.1

fw.hw.1 <- ggarrange(fw.1, hw.1,
                     nrow = 1,
                     ncol = 2,
                     common.legend = T,
                     legend = "none")

fw.hw.1

# stat-comparison of fore-wing and hind-wing between morphs in both sexes -----
juv_wing %>% filter(Sex_Morph == "M_Lw") -> Lw.m.wing
juv_wing %>% filter(Sex_Morph == "M_Sw") -> Sw.m.wing
juv_wing %>% filter(Sex_Morph == "F_Lw") -> Lw.f.wing
juv_wing %>% filter(Sex_Morph == "F_Sw") -> Sw.f.wing

juv_wing %>%
  group_by(Sex_Morph) %>%
  summarise(p.fw = shapiro.test(Juv_Forewing_Length)$p.value,
            p.hw = shapiro.test(Juv_Hindwing_Length)$p.value)

# Wilcoxon rank-sum tests for forewing and hindwing for both sexs (for consistency)
# male
wilcox.test(Lw.m.wing$Juv_Forewing_Length, Sw.m.wing$Juv_Forewing_Length) # W = 8931.5, p-value < 2.2e-16
wilcox.test(Lw.m.wing$Juv_Hindwing_Length, Sw.m.wing$Juv_Hindwing_Length) # W = 9201, p-value < 2.2e-16

# female 
wilcox.test(Lw.f.wing$Juv_Forewing_Length, Sw.f.wing$Juv_Forewing_Length) # W = 3211, p-value < 2.2e-16
wilcox.test(Lw.f.wing$Juv_Hindwing_Length, Sw.f.wing$Juv_Hindwing_Length) # W = 3212, p-value < 2.2e-16

# Sum of each morph ----
std_mean <- function(x) sd(x)/sqrt(length(x))
# Basic statistic of wing length 
# Fore-wing
# Lw male: Sample size = 86;Mean ± SE = 4.085 + 0.053
length(Lw.m.wing$Juv_Forewing_Length) # Sample size = 86
summary(Lw.m.wing$Juv_Forewing_Length) # Mean = 4.085
std_mean(Lw.m.wing$Juv_Forewing_Length) # SE = 0.053

# Sw male: Sample size = 107;Mean ± SE = 2.829 + 0.040   
length(Sw.m.wing$Juv_Forewing_Length) # Sample size = 107
summary(Sw.m.wing$Juv_Forewing_Length) # Mean = 2.829
std_mean(Sw.m.wing$Juv_Forewing_Length) # SE = 0.040

# Lw female: Sample size = 73;Mean ± SE = 3.515 + 0.030  
length(Lw.f.wing$Juv_Forewing_Length) # Sample size = 73
summary(Lw.f.wing$Juv_Forewing_Length) # Mean = 3.515
std_mean(Lw.f.wing$Juv_Forewing_Length) # SE = 0.030

# Sw female: Sample size = 44;Mean ± SE = 2.395 + 0.040  
length(Sw.f.wing$Juv_Forewing_Length) # Sample size = 44
summary(Sw.f.wing$Juv_Forewing_Length) # Mean = 2.395
std_mean(Sw.f.wing$Juv_Forewing_Length) # SE = 0.040

# Hind-wing
# Lw male: Sample size = 86;Mean ± SE = 5.884 + 0.033 
length(Lw.m.wing$Juv_Hindwing_Length) # Sample size = 86
summary(Lw.m.wing$Juv_Hindwing_Length) # Mean = 5.884
std_mean(Lw.m.wing$Juv_Hindwing_Length) # SE = 0.033

# Sw male: Sample size = 107;Mean ± SE = 4.512 + 0.027 
length(Sw.m.wing$Juv_Hindwing_Length) # Sample size = 107
summary(Sw.m.wing$Juv_Hindwing_Length) # Mean = 4.512
std_mean(Sw.m.wing$Juv_Hindwing_Length) # SE = 0.027

# Lw female: Sample size = 73;Mean ± SE = 5.746 + 0.032 
length(Lw.f.wing$Juv_Hindwing_Length) # Sample size = 73
summary(Lw.f.wing$Juv_Hindwing_Length) # Mean = 5.746
std_mean(Lw.f.wing$Juv_Hindwing_Length) # SE = 0.032

# Sw female: Sample size = 44;Mean ± SE = 4.583 + 0.036 
length(Sw.f.wing$Juv_Hindwing_Length) # Sample size = 44
summary(Sw.f.wing$Juv_Hindwing_Length) # Mean = 4.583
std_mean(Sw.f.wing$Juv_Hindwing_Length) # SE = 0.036

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
# [1] ggpubr_0.6.0    extrafont_0.19  lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   purrr_1.0.2    
# [7] readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    tidyverse_2.0.0 dplyr_1.1.4     ggplot2_3.5.1  
# 
# loaded via a namespace (and not attached):
# [1] utf8_1.2.4        generics_0.1.3    rstatix_0.7.2     stringi_1.8.4     extrafontdb_1.0  
# [6] hms_1.1.3         magrittr_2.0.3    grid_4.3.0        timechange_0.3.0  backports_1.4.1  
# [11] fansi_1.0.6       scales_1.3.0      abind_1.4-5       cli_3.6.3         rlang_1.1.4      
# [16] crayon_1.5.2      cowplot_1.1.3     bit64_4.5.2       munsell_0.5.1     withr_3.0.1      
# [21] tools_4.3.0       parallel_4.3.0    tzdb_0.4.0        ggsignif_0.6.4    colorspace_2.1-1 
# [26] broom_1.0.5       vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4   car_3.1-2        
# [31] bit_4.0.5         vroom_1.6.5       pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5     
# [36] glue_1.7.0        tidyselect_1.2.1  rstudioapi_0.16.0 farver_2.1.2      carData_3.0-5    
# [41] labeling_0.4.3    Rttf2pt1_1.3.12   compiler_4.3.0   