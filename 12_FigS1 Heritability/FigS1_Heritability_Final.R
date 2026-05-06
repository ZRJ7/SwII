rm(list = ls())

# pacakges
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemr)
library(ggthemes)
library(readr)
library(ggh4x)
library(extrafont)

# load data 
cross <- read.csv("FigS1_Heritability_Final.csv")

cross$Cross_Type <- factor(cross$Cross_Type,
                                 levels = c("Lw(M)-Lw(F)(Homo)",
                                            "Lw(M)-Lw(F)(Hete)",
                                            "Sw(M)-Lw(F)(Homo)",
                                            "Sw(M)-Lw(F)(Hete)",
                                            "Lw(M)-Sw(F)",
                                            "Sw(M)-Sw(F)"))

cross$Parental_genotypes <- factor(cross$Parental_genotype,
                                         levels = c("XL x XLXL",
                                                    "XL x XLXs",
                                                    "Xs x XLXL",
                                                    "Xs x XLXs",
                                                    "XL x XsXs",
                                                    "Xs x XsXs"))

cross$Parental_phenotypes <- factor(cross$Parental_phenotype,
                                          levels = c("Lw(M) x Lw(F)",
                                                     "Sw(M) x Lw(F)",
                                                     "Lw(M) x Sw(F)",
                                                     "Sw(M) x Sw(F)"))

cross$Off <- factor(cross$Off_Sex, levels = c("M","F"))
cross.1 <- cross %>% filter(Not_Use != "Y")
cross$Prop <- as.numeric(cross$Prop)
cross$Order <- as.numeric(cross$Order)
cross$Pair_ID <- as.factor(cross$Pair_ID)
str(cross.1)

# plot 
cols = c("#f47821","#8db6e1","#faac70","#3f95d1", "#80cdc1","#9aa7c1")

male <- cross.1 %>% filter(Off == "M") %>%
  ggplot(x = Pairs, y = Prop*100, fill = Cross_Type) +
  geom_point(aes(x = Pair_ID, y = Prop*100, color = Cross_Type),
             size = 1) +
  geom_hline(yintercept = 50, linetype = "dotdash", color = "grey") +
  facet_wrap2(~Cross_Type,
              nrow = 1,
              scales = "free_x",
              strip = strip_nested(background_x = elem_list_rect(fill = cols, color = NA))) +
  theme_test() +
  ylab("Proportion of Sw Male (%)") +
  xlab("") +
  theme(axis.text.x = element_blank(),
        text = element_text("Arial", size = 8)) +
  scale_color_manual(values = cols)

male

female <- cross.1 %>% filter(Off == "F") %>%
  ggplot(x = Pairs, y = Prop*100, fill = Cross_Type) +
  geom_point(aes(x = Pair_ID, y = Prop*100, color = Cross_Type),
             size = 1) +
  geom_hline(yintercept = 50, linetype = "dotdash", color = "grey") +
  facet_wrap2(~Cross_Type,
              nrow = 1,
              scales = "free_x",
              strip = strip_nested(background_x = elem_list_rect(fill = cols, color = NA))) +
  theme_test() +
  ylab("Proportion of Sw Female (%)") +
  xlab("Pairs") +
  scale_color_manual(values = cols) +
  theme(axis.text.x = element_blank(),
        text = element_text("Arial", size = 8))
female

ggarrange(male, female,
          common.legend = T,
          nrow = 2,
          legend = "none",
          heights = 4.5,
          widths = 7)

cross.1 %>%
  group_by(Cross_Type) %>%
  summarise(
    n = n()/2,
    total_individuals = sum(Off_M + Off_F, na.rm = TRUE)/2
  )

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
# [1] extrafont_0.19 ggh4x_0.2.8    readr_2.1.5    ggthemes_5.1.0 ggthemr_1.1.0  ggpubr_0.6.0   dplyr_1.1.4   
# [8] ggplot2_3.5.1 
# 
# loaded via a namespace (and not attached):
# [1] gtable_0.3.5      compiler_4.3.0    ggsignif_0.6.4    tidyselect_1.2.1  stringr_1.5.1    
# [6] tidyr_1.3.1       scales_1.3.0      R6_2.5.1          labeling_0.4.3    generics_0.1.3   
# [11] backports_1.4.1   tibble_3.2.1      car_3.1-2         munsell_0.5.1     pillar_1.9.0     
# [16] tzdb_0.4.0        rlang_1.1.4       utf8_1.2.4        Rttf2pt1_1.3.12   broom_1.0.5      
# [21] stringi_1.8.4     cli_3.6.3         withr_3.0.1       magrittr_2.0.3    grid_4.3.0       
# [26] rstudioapi_0.16.0 hms_1.1.3         cowplot_1.1.3     lifecycle_1.0.4   vctrs_0.6.5      
# [31] rstatix_0.7.2     glue_1.7.0        extrafontdb_1.0   farver_2.1.2      abind_1.4-5      
# [36] carData_3.0-5     fansi_1.0.6       colorspace_2.1-1  purrr_1.0.2       tools_4.3.0      
# [41] pkgconfig_2.0.3  