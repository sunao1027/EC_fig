pacman::p_load(readxl,tidyverse,ggplot2)
windowsFonts(Meiryo = windowsFont("Meiryo"))
windowsFonts(Arial = windowsFont("Arial"))
windowsFonts("Yu Gothic" = windowsFont("Yu Gothic"))

EC_growth <- readxl::read_xlsx("EC_biological assay.xlsx",col_names = TRUE)
EC_growth <- EC_growth |> mutate(
  log_Cells = log10(Cells),
)

EC_growth <- EC_growth |> mutate(
  Elapsed_time = hms::as_hms(
    as.numeric(
      difftime(Elapsed_time, as.POSIXct("1899-12-31 00:00:00", tz = "UTC"),units = "secs"))
    ))

scientific_10 <- function(x) {
  parse(text = gsub("e", " %*% 10^", scales::scientific_10(x)))
}



EC_growth_curve <- EC_growth %>% ggplot(aes(x = Elapsed_time, y = log_Cells, colour = Methods)) +
  geom_line(linewidth = 1, n = 50000) + 
  geom_point(size = 3) +
  theme_classic() +
  theme(axis.title = element_text(family = "Yu Gothic", face = "bold"),
        plot.title = element_text(hjust = 0.5, family = "Yu Gothic", face = "bold"),
        axis.text.x = element_text(family = "Meiryo", face = "bold"),
        axis.text.y = element_text(family = "Meiryo", face = "bold"),
        legend.text = element_text(family = "Meiryo"),
        legend.title = element_text(family = "Yu Gothic", face = "bold")) + 
  ylim(6,9)+
  labs(title = "Enterococcus cesorum 宮チキNo.4株 培養検討(BHI broth, 微好気条件)",
       colour = "計測方法",
       x = "培養時間",
       y = "細菌数(常用対数)")
ggsave(filename = "EC_growth_log.png", EC_growth_curve)

EC_ddPCR_test <- readxl::read_xlsx("qEC_ddPCR_test.xlsx",col_names = TRUE)
EC_ddPCR_test <- EC_ddPCR_test |> mutate(
  log_Conc = log10((Conc+1)))
EC_ddPCR_test_long <- EC_ddPCR_test %>%
  pivot_longer(cols = c(Negatives, Positives),
               names_to = "Type",
               values_to = "Count")

EC_ddPCR_test$`Sample description 1` <- factor(EC_ddPCR_test$`Sample description 1`,
                                                    levels = c("Undiluted", 
                                                               "×10"
                                                               ,"×100",
                                                               "×1000",
                                                               "×10000",
                                                               "Negative Control"))

EC_ddPCR_test_long$`Sample description 1` <- factor(EC_ddPCR_test_long$`Sample description 1`,
                                                    levels = c("Undiluted", 
                                                               "×10"
                                                               ,"×100",
                                                               "×1000",
                                                               "×10000",
                                                               "Negative Control"))

EC_ddPCR_test_long$Type <- factor(EC_ddPCR_test_long$Type,
                                                    levels = c("Positives", 
                                                               "Negatives"))
pn <- EC_ddPCR_test_long |>
  ggplot(aes(x = `Sample description 1`,
             y = Count, 
             fill = Type)) +
  geom_col(width = 0.5,colour = "#000000")+
  labs(title = "Numbers of droplets (Positives / Negatives)",
       y = "Numbers of droplets",
       x = NULL,
       fill = "Droplets type"
         )+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#dc143c","#4169e1" ))+
  theme_classic()
ggsave("ドロップレット数.png", pn, width = 8, height = 6, dpi = 300)

conc <- EC_ddPCR_test |>
  ggplot(aes(x = `Sample description 1`,
             y = log_Conc)) +
  geom_col(width = 0.5,colour = "#000000")+
  labs(title = "log Concentration (copies / μl)",
       y = "log Concentration (copies / μl)",
       x = NULL
  )+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()
ggsave("DNA濃度.png", conc, width = 8, height = 6, dpi = 300)