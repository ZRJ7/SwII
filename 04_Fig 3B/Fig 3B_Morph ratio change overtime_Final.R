# Clean Env ----
rm(list=ls())

# Packages ----
library(ggplot2)
library(dplyr)
library(readr)
library(extrafont)
loadfonts()

# Plot -----
Morph.ratio <- read_csv("Fig3B_Field Survey_Final.csv")

str(Morph.ratio)

Morph.ratio$Sex <- as.factor(Morph.ratio$Sex)
Morph.ratio$Morph <- as.factor(Morph.ratio$Morph)
Morph.ratio$Phe <- as.factor(Morph.ratio$Phe)

str(Morph.ratio)

Morph.ratio$Time <- factor(Morph.ratio$Time,
                        levels = c("2014",
                                   "2015",
                                   "2016",
                                   "2017",
                                   "2018",
                                   "2019.Sum",
                                   "2019.Win",
                                   "2020",
                                   "2021.Sum",
                                   "2021.Win",
                                   "2022.Spr",
                                   "2022.Sum",
                                   "2022.Win",
                                   "2023",
                                   "2024"))

str(Morph.ratio)

# Fig 3B
P.Morph_ratio <-  
  Morph.ratio %>% 
  ggplot(aes(x = Year, y = Ratio*100, color = Sex, group = Sex)) +
  geom_line(linetype = "solid", data = subset(Morph.ratio, !is.na(Ratio))) +
  geom_point(data = subset(Morph.ratio, !is.na(Ratio))) +
  geom_point(aes(size = Sample_size), alpha = 0.8) + 
  scale_size_continuous(range = c(2, 5)) +
  theme_test() + 
  scale_color_manual(values = c("#D55E4A","#4C72B0")) +   
  theme(text = element_text(family = "Arial", size = 10)) +
  ylab('Sw morph frequency (%)') +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(0,25,50,75,100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_continuous(
    breaks = Morph.ratio$Year,
    labels = Morph.ratio$Time) +
  theme(text = element_text(family = "Arial", size = 14),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Sex")

P.Morph_ratio


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
# [1] extrafont_0.19 readr_2.1.5    dplyr_1.1.4    ggplot2_3.5.1 
# 
# loaded via a namespace (and not attached):
# [1] crayon_1.5.2      vctrs_0.6.5       cli_3.6.3         rlang_1.1.4       Rttf2pt1_1.3.12  
# [6] generics_0.1.3    labeling_0.4.3    bit_4.0.5         glue_1.7.0        colorspace_2.1-1 
# [11] extrafontdb_1.0   hms_1.1.3         scales_1.3.0      fansi_1.0.6       grid_4.3.0       
# [16] munsell_0.5.1     tibble_3.2.1      tzdb_0.4.0        lifecycle_1.0.4   compiler_4.3.0   
# [21] pkgconfig_2.0.3   rstudioapi_0.16.0 farver_2.1.2      R6_2.5.1          tidyselect_1.2.1 
# [26] utf8_1.2.4        parallel_4.3.0    vroom_1.6.5       pillar_1.9.0      magrittr_2.0.3   
# [31] bit64_4.5.2       tools_4.3.0       withr_3.0.1       gtable_0.3.5