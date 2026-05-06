# clean env ----
rm(list = ls())

# Packages----
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(readr)
library(FSA) 
library(extrafont)
library(readr)
loadfonts()

# load data ----
Immunity <- read_csv("Fig5C_Final_Immunity.csv")

View(Immunity)

str(Immunity)

Immunity$Group <- factor(Immunity$Group, 
                         levels = c("Control", "Lw_M", "Sw_M", "Lw_F", "Sw_F"))


head(Immunity)
            
# Statistical tests ----
Immunity %>% filter(Group == "Lw_M")  -> Lw_M

shapiro.test(Lw_M$Ave_Grey) # P < 0.05

Immunity %>% filter(Group == "Sw_M") -> Sw_M

shapiro.test(Sw_M$Ave_Grey) # P < 0.05

Immunity %>% filter(Group == "Lw_F") -> Lw_F

shapiro.test(Lw_F$Ave_Grey) # P < 0.05

Immunity %>% filter(Group == "Sw_F") -> Sw_F

shapiro.test(Sw_F$Ave_Grey) # P < 0.05

# P value correction----

kruskal.test(Immunity$Ave_Grey~ Immunity$Group, data = Immunity)

dunnTest(Immunity$Ave_Grey~ Immunity$Group, data = Immunity, method="bh")

f1 <- Immunity %>% 
  ggplot(aes(x = Group, y = Ave_Grey, color = Group)) +
  geom_boxplot(linetype = "dashed", outlier.alpha = 0) + 
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.alpha = 0) +  
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) + 
  stat_boxplot(geom = "errorbar", 
               aes(ymax = after_stat(min)),
               width = 0.15) + 
  geom_jitter(shape=16,position = position_jitter(0.25), 
              alpha = .6, size = 3) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(
    labels = c("Control",
               "Lw(M)",
               "Sw(M)",
               "Lw(F)",
               "Sw(F)")
  ) +
  ylab("Average implant darkness") +
  scale_y_continuous(limits = c(0, 270),
    breaks = c(0, 50 , 100, 150, 200, 250)) +
  scale_color_manual(name = "Group",
                     values = c("grey50", "#e97a42", "#afbde0","#9cc37b", "#106ab3")) + 
  theme(text = element_text("Arial",size = 16),
        legend.position = "none") +
  xlab("")

f1

ggsave("Immunity_2026Apr17.pdf", f1, dpi = 600, height = 5, width = 4)

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
# [1] extrafont_0.19  FSA_0.9.5       ggpubr_0.6.0    lubridate_1.9.3
# [5] forcats_1.0.0   stringr_1.5.1   purrr_1.0.2     readr_2.1.5    
# [9] tidyr_1.3.1     tibble_3.2.1    tidyverse_2.0.0 dplyr_1.1.4    
# [13] ggplot2_3.5.1  
# 
# loaded via a namespace (and not attached):
# [1] utf8_1.2.4        generics_0.1.3    rstatix_0.7.2     stringi_1.8.4    
# [5] extrafontdb_1.0   hms_1.1.3         magrittr_2.0.3    grid_4.3.0       
# [9] timechange_0.3.0  backports_1.4.1   fansi_1.0.6       scales_1.3.0     
# [13] abind_1.4-5       cli_3.6.3         rlang_1.1.4       crayon_1.5.2     
# [17] bit64_4.5.2       munsell_0.5.1     withr_3.0.1       tools_4.3.0      
# [21] parallel_4.3.0    tzdb_0.4.0        ggsignif_0.6.4    colorspace_2.1-1 
# [25] dunn.test_1.3.6   broom_1.0.5       vctrs_0.6.5       R6_2.5.1         
# [29] lifecycle_1.0.4   car_3.1-2         bit_4.0.5         vroom_1.6.5      
# [33] pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5      glue_1.7.0       
# [37] tidyselect_1.2.1  rstudioapi_0.16.0 farver_2.1.2      carData_3.0-5    
# [41] Rttf2pt1_1.3.12   compiler_4.3.0   