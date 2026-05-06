# Calling song diversification analysis
# Clean env ----
rm(list = ls())

# Load packages ----
library(ggplot2)
library(readr)
library(dplyr)
library(extrafont)
loadfonts()


# load data ----
Calling_song <- read_csv("Fig2B&D_Calling song diversification_Final.csv")

str(Calling_song)

Calling_song$Phenotype <- as.factor(Calling_song$Phenotype)

Calling_song$Phenotype <- factor(Calling_song$Phenotype, levels = c("Lw","Sw"))

Calling_song_without_NA <- 
  Calling_song %>% 
  filter(!is.na(Frequency)) %>%
  filter(!is.na(Amplitude))

# Plot ----
# Fig 2B
song_spectrum <-
  Calling_song_without_NA %>%
  ggplot(aes(x = Frequency/1000, y = Amplitude , color = Phenotype)) +
  geom_point(alpha = 1, size = 3.5) + #alpha = .7
  scale_size_continuous(guide = NULL) +
  ylab("Peak amplitude (dB SPL)") +
  scale_x_continuous(label = c(4, 5,6,7,8),
                     limits = c(4, 8),
                     breaks = c(4, 5, 6, 7, 8)) +
  xlab("Carrier frequency (kHz)") +
  theme_test() +
  theme(text = element_text(family = "Arial",size = 16),
        legend.position = "bottom") +
  scale_color_manual(name = "Phenotype", values = c("#F1A340","#998EC3")) +
  geom_vline(aes(xintercept = 4.94), colour = "#F1A340", linetype = "dashed") +
  geom_vline(aes(xintercept = 6.04), colour = "#998EC3", linetype = "dashed")

song_spectrum

# Fig 2D
res.cf <-
  Calling_song %>%
  ggplot(aes(x=Mean_Harp_Size, y = Frequency/1000)) +
  geom_point(aes(colour = Phenotype), size = 2.5) +
  geom_smooth(method = 'lm',
              linewidth = 0.5,
              colour = 'red',
              se = F) +
  xlab("Resonator size (mm2)") +
  ylab("Carrier frequency (kHz)") +
  theme_test() +
  theme(legend.position = ("none"),
        text = element_text(family = "Arial", size = 16))  +
  scale_color_manual(values = c("#F1A340","#998EC3")) 

res.cf



# Correlation test 
cor.test(Calling_song $Mean_Harp_Size, Calling_song $Frequency, method = "spearman")


# Mean and SE of Carrier frequency -----
Calling_song_without_NA %>%       
  group_by(Phenotype) %>%
  summarise(
    mean = mean(Frequency, na.rm = TRUE),
    sd = sd(Frequency, na.rm = TRUE),
    n = sum(!is.na(Frequency)),
    se = sd / sqrt(n)
  )

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

loaded via a namespace (and not attached):
[1] utf8_1.2.4        R6_2.5.1          tidyselect_1.2.1  magrittr_2.0.3   
[5] gtable_0.3.5      glue_1.7.0        tibble_3.2.1      pkgconfig_2.0.3  
[9] generics_0.1.3    dplyr_1.1.4       lifecycle_1.0.4   ggplot2_3.5.1    
[13] cli_3.6.3         fansi_1.0.6       scales_1.3.0      grid_4.3.0       
[17] vctrs_0.6.5       compiler_4.3.0    rstudioapi_0.16.0 tools_4.3.0      
[21] munsell_0.5.1     pillar_1.9.0      colorspace_2.1-1  rlang_1.1.4    
