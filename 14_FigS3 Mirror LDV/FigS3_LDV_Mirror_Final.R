rm(list = ls())

#Load Package----
library(dplyr)
library(ggplot2)
library(cowplot)
library(extrafont)
loadfonts()

#Fig S3 Mirror LDV----

# Long-wing samples ----
lw.left.mirror <- read.csv("FigS3_LDV_Lw_Left_Mirror_Final.csv")
str(lw.left.mirror)

p.lw.1eft <- lw.left.mirror %>% 
  ggplot(aes(x = Frequency.Hz./1000, y = Mean.Magnitude.m.pa.*1000000000, fill = "#F1A340")) +
  geom_line(colour = "#F1A340", linewidth = 0.5) +
  geom_line(aes(y = Mean.SD*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = Mean.SD.1*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-100,700) +
  geom_vline(xintercept = 4.80, linetype = "dashed") +
  annotate("text", x =13, y = 500,
           label = "Left wing peak resonance\n 4.80 kHz",
           hjust = 0.5,
           size = 5,
           color = "#F1A340")

p.lw.1eft


lw.right.mirror <- read.csv("FigS3_LDV_Lw_Right_Mirror_Final.csv")

str(lw.left.mirror)

p.lw.right <-
  lw.right.mirror %>% 
  ggplot(aes(x = Frequency.Hz./1000, y = Mean.Magnitude.m.pa.*1000000000, fill = "#F1A340")) +
  geom_line(colour = "#F1A340", linewidth = 0.5) + 
  geom_line(aes(y = Mean.SD*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = Mean.SD.1*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
 # scale_x_continuous(limits = c(0,50), breaks =c(5,10,15,20,25,30,35,40,45,50)) + #scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-100,700) +
  geom_vline(xintercept = 4.61, linetype = "dashed") +
  annotate("text", x =13, y = 500,
           label = "Right wing peak resonance\n 4.61 kHz",
           hjust = 0.5,
           size = 5,
           color = "#F1A340")

p.lw.right

plot_grid(p.lw.1eft, p.lw.right, align = "v",
          nrow = 2)


# Small-wing samples ----

sw.left.mirror <- read.csv("FigS3_LDV_Sw_Left_Mirror_Final.csv")

str(sw.left.mirror)

p.sw.1eft <- 
  sw.left.mirror %>% 
  ggplot(aes(x = Frequency.Hz./1000, y = Mean.Magnitude.m.pa.*1000000000, fill = "#998EC3")) +
  geom_line(colour = "#998EC3", linewidth = 0.5) + 
  geom_line(aes(y = Mean.SD*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = Mean.SD.1*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) + #scale_x_continuous(limits = c(0,50), breaks =c(5,10,15,20,25,30,35,40,45,50))
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-100,700) +
  geom_vline(xintercept = 6.80, linetype = "dashed") +
  annotate("text", x =15, y = 500,
           label = "Left wing peak resonance\n 6.80 kHz",
           hjust = 0.5,
           size = 5,
           color = "#998EC3")

p.sw.1eft 


sw.right.mirror <- read.csv("FigS3_LDV_Sw_Right_Mirror_Final.csv")

str(sw.right.mirror)

p.sw.right <-
  sw.right.mirror %>% 
  ggplot(aes(x = Frequency.Hz./1000, y = Mean.Magnitude.m.pa.*1000000000, fill = "#998EC3")) +
  geom_line(colour = "#998EC3", linewidth = 0.5) +
  geom_line(aes(y = Mean.SD*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = Mean.SD.1*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-100,700) +
  geom_vline(xintercept = 6.90, linetype = "dashed") +
  annotate("text", x =15, y = 500,
           label = "Right wing peak resonance\n 6.90 kHz",
           hjust = 0.5,
           size = 5,
           color = "#998EC3")

p.sw.right 

plot_grid(p.sw.1eft, p.sw.right, align = "v",
          nrow = 2)

all.fig <- plot_grid(p.lw.1eft, p.sw.1eft, 
                     p.lw.right, p.sw.right, 
                     labels = c("A", "B", "C", "D"),
                     label_size = 18,
                     align = "hv",
                     nrow = 2,
                     ncol = 2)


all.fig 

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
# [1] extrafont_0.19 ggpubr_0.6.0   cowplot_1.1.3  ggplot2_3.5.1  dplyr_1.1.4    readxl_1.4.3  
# 
# loaded via a namespace (and not attached):
# [1] gtable_0.3.5      compiler_4.3.0    ggsignif_0.6.4    tidyselect_1.2.1  tidyr_1.3.1      
# [6] scales_1.3.0      R6_2.5.1          labeling_0.4.3    generics_0.1.3    backports_1.4.1  
# [11] tibble_3.2.1      car_3.1-2         munsell_0.5.1     pillar_1.9.0      rlang_1.1.4      
# [16] utf8_1.2.4        broom_1.0.5       Rttf2pt1_1.3.12   cli_3.6.3         withr_3.0.1      
# [21] magrittr_2.0.3    grid_4.3.0        rstudioapi_0.16.0 lifecycle_1.0.4   vctrs_0.6.5      
# [26] rstatix_0.7.2     glue_1.7.0        farver_2.1.2      cellranger_1.1.0  extrafontdb_1.0  
# [31] abind_1.4-5       carData_3.0-5     fansi_1.0.6       colorspace_2.1-1  purrr_1.0.2      
# [36] tools_4.3.0       pkgconfig_2.0.3  