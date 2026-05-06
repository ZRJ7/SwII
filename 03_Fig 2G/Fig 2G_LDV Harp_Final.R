# Clean env
rm(list = ls())

#Load Package----
library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(extrafont)
loadfonts()

#Fig 2G LDV----

# Long-wing samples ----
lw.left.harp <- read_csv("Fig2G_LDV_Lw_Left_Harp_Final.csv")
str(lw.left.harp)

p.lw.1eft <- lw.left.harp %>% 
  ggplot(aes(x = `Frequency(Hz)`/1000, y = `Mean Magnitude(m/pa)`*1000000000, fill = "#F1A340")) +
  geom_line(colour = "#F1A340", linewidth = 0.5) +
  geom_line(aes(y = `Mean+SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = `Mean-SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-300,1500) +
  geom_vline(xintercept = 4.84, linetype = "dashed") 

p.lw.1eft


lw.right.harp <- read_csv("Fig2G_LDV_Lw_Right_Harp_Final.csv")

str(lw.left.harp)

p.lw.right <-
  lw.right.harp %>% 
  ggplot(aes(x = `Frequency(Hz)`/1000, y = `Mean Magnitude(m/pa)`*1000000000, fill = "#F1A340")) +
  geom_line(colour = "#F1A340", linewidth = 0.5) + 
  geom_line(aes(y = `Mean+SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = `Mean-SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-300,1500) +
  geom_vline(xintercept = 4.78, linetype = "dashed") 

p.lw.right


# Small-wing samples ----

sw.left.harp <- read_csv("Fig2G_LDV_Sw_Left_Harp_Final.csv")

str(sw.left.harp)

p.sw.1eft <- 
  sw.left.harp %>% 
  ggplot(aes(x = `Frequency(Hz)`/1000, y = `Mean Magnitude(m/pa)`*1000000000, fill = "#998EC3")) +
  geom_line(colour = "#998EC3", linewidth = 0.5) + 
  geom_line(aes(y = `Mean+SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = `Mean-SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-300,1500) +
  geom_vline(xintercept = 5.95, linetype = "dashed") 

p.sw.1eft 


sw.right.harp <- read_csv("Fig2G_LDV_Sw_Right_Harp_Final.csv")

str(sw.right.harp)

p.sw.right <-
  sw.right.harp %>% 
  ggplot(aes(x = `Frequency(Hz)`/1000, y = `Mean Magnitude(m/pa)`*1000000000, fill = "#998EC3")) +
  geom_line(colour = "#998EC3", linewidth = 0.5) +
  geom_line(aes(y = `Mean+SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  geom_line(aes(y = `Mean-SD`*1000000000), colour = "grey", alpha = 0.75, linewidth = 0.5) +
  theme_test() +
  scale_x_continuous(limits = c(1,25), breaks =c(5,10,15,20,25,30,35,40,45,50)) +
  ylab("Magnitude (nm/Pa)") +
  xlab("Frequency (kHz)") +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  ylim(-300,1500) +
  geom_vline(xintercept = 6.78, linetype = "dashed") 

p.sw.right 

# Final
all.fig.2G <- plot_grid(p.lw.1eft, p.sw.1eft, 
                       p.lw.right, p.sw.right, 
                       align = "hv",
                       nrow = 2,
                       ncol = 2)


all.fig.2G


# statistical test----

ave.harp <- read_csv("Fig2G_Average_harp_resonance_intensity_Final.csv")

str(ave.harp)

ave.harp$Morph <- as.factor(ave.harp$Morph)

ave.harp %>%
  group_by(Morph) %>%
  summarise(mean.res = mean(Ave_resonance/1000),
            mean.int = mean(Ave_intensity*1000000000),
            se.res = sd(Ave_resonance/1000)/sqrt(n()),
            se.int = sd(Ave_intensity*1000000000)/sqrt(n()),
            p.res = shapiro.test(Ave_resonance)$p.value,
            p.int = shapiro.test(Ave_intensity)$p.value) 

var.test(Ave_resonance ~ Morph, data = ave.harp)

t.test(Ave_resonance ~ Morph, equal.var = T, data = ave.harp)

wilcox.test(Ave_intensity ~ Morph, data = ave.harp)

sessionInfo()
R version 4.3.0 (2023-04-21)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.4

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] extrafont_0.19 cowplot_1.1.3  ggplot2_3.5.1  dplyr_1.1.4    readr_2.1.5   

loaded via a namespace (and not attached):
[1] crayon_1.5.2      vctrs_0.6.5       cli_3.6.3         rlang_1.1.4       Rttf2pt1_1.3.12  
[6] generics_0.1.3    labeling_0.4.3    bit_4.0.5         glue_1.7.0        colorspace_2.1-1 
[11] extrafontdb_1.0   hms_1.1.3         scales_1.3.0      fansi_1.0.6       grid_4.3.0       
[16] munsell_0.5.1     tibble_3.2.1      tzdb_0.4.0        lifecycle_1.0.4   compiler_4.3.0   
[21] pkgconfig_2.0.3   rstudioapi_0.16.0 farver_2.1.2      R6_2.5.1          tidyselect_1.2.1 
[26] utf8_1.2.4        parallel_4.3.0    vroom_1.6.5       pillar_1.9.0      magrittr_2.0.3   
[31] bit64_4.5.2       tools_4.3.0       withr_3.0.1       gtable_0.3.5     