#Autor: Jozef Kollár
#Názov bakalárskej práce: ANALÝZA METÓD PRE VÝPOČET INDEXU SPOTREBITEĽSKÝCH CIEN
#Rok: 2026

#Popis: Skript spracuje DFF udaje na vypocet cenovych indexov: 
#Carli, Jevons, ChJevons, Young-Averaged, Theil-Tornqvist, taktiez aj vizualizuje vypocitane indexy.

#Struktura priecinkov:
#parent_folder/
#├── skript.R              (skript)
#├── data/                 (priecinok so vstupnymi datami)
#│    ├── wana.csv        
#│    └── wcso.csv
#└── grafy/                (priecinok pre graficke vystupy)
#     ├── ana/             (sem budu ulozene grafy z analgetik)
#     └── cso/             (sem budu ulozene grafy z konzervovanych polievok) 

#Instrukcie: Skript je potrebne spustit v ramci R projektu, alebo nastavte setwd() do priecinka so skriptom (parent_folder) v strukture.

#setwd("cesta/k/parent_folder")

#instalacia balickov
#install.packages("data.table")
#install.packages("PriceIndices")
#install.packages("ggplot2")

#inicializacia balickov
library(data.table)
library(PriceIndices)
library(ggplot2)



#-----------------FUNCKIE----------------------

#prirpavi surove data: odstrani irelevantne pozorovania a definuje casovy ramec
preparation <- function(df){
  df <- df[WEEK >= 13 & WEEK <= 68]  
  df <- df[PRICE != 0 & QTY != 0 & MOVE != 0]  
  df[, week_idx := WEEK - 12L]              
  df[, time := cas[week_idx]]
  
  df[, prices     := PRICE / QTY]            
  df[, quantities := MOVE]                   
  df[, prodID     := as.character(UPC)]
  df[, retID      := STORE]
  
  df <- df[, .(time, prodID, retID, prices, quantities)]
  
  df_prep <- data_preparing(
    df,
    time          = "time",
    prices        = "prices",
    quantities    = "quantities",
    prodID        = "prodID",
    retID         = "retID",
    description   = NULL,         
    zero_prices   = FALSE,         
    zero_quantities = FALSE       
  )
  return(df_prep)
}

#vrati dataframe s produktami predavanymi v kazdom mesiaci casoveho ramca
basket <- function(data, start_date, end_date) {
  dt <- as.data.table(data)
  
  period_times <- seq(
    from = as.Date(paste0(start_date, "-01")),
    to   = as.Date(paste0(end_date,   "-01")),
    by   = "1 month"
  )
  
  dt_period <- dt[time %in% period_times]
  n_periods <- length(period_times)
  
  complete_prods <- dt_period[, .(n = .N), by = prodID][n == n_periods, prodID]
  basket_dt <- dt_period[prodID %in% complete_prods]
  
  basket_df <- as.data.frame(basket_dt)
  
  cat("=== FIXED BASKET (", start_date, "–", end_date, ") ===\n")
  cat("Pôvodný počet produktov v období: ", uniqueN(dt_period$prodID), "\n")
  cat("Počet produktov v fixed baskete: ", length(complete_prods), "\n")
  cat("Počet riadkov v baskete: ", nrow(basket_df), "\n\n")
  
  return(basket_df)
}

#vypocita vybrane indexy za kazdy mesiac
compute_indices <- function(dataf, fil, startp, endp){
  dataf <- basket(dataf, startp, endp)
  
  P_CHjev <- chjevons(fil, start = startp, end = endp, interval = TRUE)
  P_Jevons <- jevons(dataf, start = startp, end = endp, interval = TRUE)
  P_Carli <- carli(dataf, start = startp, end = endp, interval = TRUE)
  P_Tornquist <- tornqvist(dataf, start = startp, end = endp, interval = TRUE)
  P_Young0 <- young(dataf, start = startp, end = endp, base = startp, interval = TRUE)
  
  datef <- paste(startp, "-01", sep="")
  timevar <- seq(from = as.Date(datef), by = "1 month", length.out = 13)
  timevar <- format(timevar, "%Y-%m")
  
  P_Young1 <- c()
  for(i in timevar){
    P_Young1 <- c(P_Young1, young(dataf, start = startp, end = i, base = i))
  }
  
  P_Young <- (P_Young0 + P_Young1)/2
  
  df_indices <- data.frame(
    Carli = P_Carli,
    Jevons = P_Jevons,
    YoungA = P_Young,
    Tornquist = P_Tornquist,
    CHjevons = P_CHjev
  )
  
  return(df_indices)
}

#zobrazi priebeh vybranych indexov
plot_indices <-function(indices_df, startp, title_txt){
  ylab <- paste("Hodnota indexu (", startp, " = 1)", sep="")
  datef <- paste(startp, "-01", sep="")
  tvar <- seq(from = as.Date(datef), by = "1 month", length.out = 13)
  
  ggplot() +
    geom_line(aes(x = tvar, y = indices_df$Carli, color = "Carli")) +
    geom_line(aes(x = tvar, y = indices_df$Jevons, color = "Jevons")) +
    geom_line(aes(x = tvar, y = indices_df$YoungA, color = "YoungA")) +
    geom_line(aes(x = tvar, y = indices_df$Tornquist, color = "Törnqvist")) +
    geom_line(aes(x = tvar, y = indices_df$CHjevons, color = "ChainedJevons")) +
    
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      breaks = c(0.97, 0.98, 0.99, 1, 1.01, 1.02, 1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.09, 1.10, 1.11)
    ) +
    
    scale_color_manual(values = c(
      "ChainedJevons" = "#1f77b4",
      "Jevons" = "#ff7f0e",
      "Carli" = "#2ca02c",
      "Törnqvist" = "#d62728",
      "YoungA" = "#c49cde"
    )) +
    
    labs(
      x = "Čas",
      y = ylab,
      color = "Typ indexu"
    ) +
    
    guides(color = guide_legend(nrow = 1)) +
    
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line()
    )
}

#vypocita standardnu odchylku vybranych indexov v danom case
compute_sd <- function(indices_df){
  stdev <- c()
  for(i in 1:13){
    stdev_parc <- c(
      indices_df$Carli[i],
      indices_df$Jevons[i],
      indices_df$Tornquist[i],
      indices_df$CHjevons[i],
      indices_df$YoungA[i]
    )
    stdev <- c(stdev, sd(stdev_parc))
  }
  return(stdev)
}

#vizualizuje odchýlku v case na filtrovanych a nefiltrovanych datach
plot_sd <- function(nonfil, fil, startp, tit){
  datef <- paste(startp, "-01", sep="")
  tvar <- seq(from = as.Date(datef), by = "1 month", length.out = 13)
  
  ggplot() +
    geom_line(aes(x = tvar, y = nonfil, color = "Nefiltrované"), linewidth = 0.75) +
    geom_line(aes(x = tvar, y = fil, color = "Filtrované"), linewidth = 0.75) +
    
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      expand = c(0, 0)
    ) +
    
    scale_color_manual(values = c(
      "Nefiltrované" = "#1f77b4",
      "Filtrované" = "#d62728"
    )) +
    
    labs(
      x = "Čas",
      y = "Štandardná odchýlka indexov",
      color = "Typ úpravy dát"
    ) +
    
    guides(color = guide_legend(nrow = 1)) +
    
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line()
    )
}

#vypocita podiel spotrebneho kosa na mesacnych trzbach
compute_basket_share <- function(basket_source, total_data, start_date, end_date) {
  basket_df <- basket(basket_source, start_date, end_date)
  basket_prods <- unique(basket_df$prodID)
  
  period_times <- seq(
    from = as.Date(paste0(start_date, "-01")),
    to   = as.Date(paste0(end_date,   "-01")),
    by   = "1 month"
  )
  
  shares <- numeric(length(period_times))
  
  for (i in seq_along(period_times)) {
    month <- period_times[i]
    
    rev_basket <- sum(
      basket_df[basket_df$time == month, "prices"] *
        basket_df[basket_df$time == month, "quantities"],
      na.rm = TRUE
    )
    
    rev_total <- sum(
      total_data[total_data$time == month, "prices"] *
        total_data[total_data$time == month, "quantities"],
      na.rm = TRUE
    )
    
    shares[i] <- if (rev_total > 0) rev_basket / rev_total * 100 else NA
  }
  
  data.frame(time = period_times, share = shares)
}

#vypocita podiel spotrebneho kosa na celkovych trzbach
compute_overall_basket_share <- function(basket_source, total_data, start_date, end_date) {
  basket_df <- basket(basket_source, start_date, end_date)
  
  rev_basket <- sum(basket_df$prices * basket_df$quantities, na.rm = TRUE)
  
  period_times <- seq(
    as.Date(paste0(start_date, "-01")),
    as.Date(paste0(end_date,   "-01")),
    by = "1 month"
  )
  
  full_period <- total_data[total_data$time %in% period_times, ]
  rev_total <- sum(full_period$prices * full_period$quantities, na.rm = TRUE)
  
  share <- if (rev_total > 0) rev_basket / rev_total * 100 else NA
  
  cat(sprintf(
    "Celkový podiel fixed basketu %s – %s: %.2f %% (zdroj: %s)\n",
    start_date, end_date, share, deparse(substitute(basket_source))
  ))
  
  return(share)
}

#vizualizuje priebeh mesacnych podielov spotrebneho kosa v case
plot_basket_share <- function(share_nf, share_f, startp, title_period, category) {
  ggplot() +
    geom_line(aes(x = share_nf$time, y = share_nf$share, color = "Nefiltrované"), linewidth = 0.9) +
    geom_line(aes(x = share_f$time,  y = share_f$share,  color = "Filtrované"), linewidth = 0.9) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = scales::percent_format(scale = 1, suffix = "%"),
      breaks = seq(0, 100, by = 10)
    ) +
    
    scale_color_manual(values = c(
      "Nefiltrované" = "#1f77b4",
      "Filtrované" = "#d62728"
    )) +
    
    labs(
      x = "Čas",
      y = "Podiel koša na celkových tržbách (%)",
      color = "Typ úpravy dát"
    ) +
    
    guides(color = guide_legend(nrow = 1)) +
    
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#------------------------Nacitanie dat a definicia casu--------------------------------------
cas <- seq(from = as.Date("1989-12-07"), by = "7 days", length.out = 56)
commodities_path <- "data/"
df_wana <- fread(paste0(commodities_path, "wana.csv"))
df_wcso <- fread(paste0(commodities_path, "wcso.csv"))

df_wana <- preparation(df_wana)
df_wcso <- preparation(df_wcso)

df_agg_ana <- data_aggregating(df_wana, join_outlets = TRUE)
df_fil_ana <- data_filtering(df_agg_ana, start = "1989-12", end = "1990-12",
                             filters = c("extremeprices", "dumpprices", "lowsales"),
                             plimits = c(0.5, 2), dplimits = c(0.8, 0.2), interval = TRUE)

df_agg_cso <- data_aggregating(df_wcso, join_outlets = TRUE)
df_fil_cso <- data_filtering(df_agg_cso, start = "1989-12", end = "1990-12",
                             filters = c("extremeprices", "dumpprices", "lowsales"),
                             plimits = c(0.5, 2), dplimits = c(0.8, 0.2), interval = TRUE)

#------------------------VYPOCET INDEXOV---------------------------

#ANA 1989–1990
ana_nf_89 <- compute_indices(df_agg_ana, df_fil_ana, "1989-12", "1990-12")
ana_f_89  <- compute_indices(df_fil_ana, df_fil_ana, "1989-12", "1990-12")

#CSO 1989–1990
cso_nf_89 <- compute_indices(df_agg_cso, df_fil_cso, "1989-12", "1990-12")
cso_f_89  <- compute_indices(df_fil_cso, df_fil_cso, "1989-12", "1990-12")

#---------------Vizualizaia INDEXOV---------------
plot_ana_nf_89 <- plot_indices(ana_nf_89, "1989-12",
                               "Vývoj cenových indexov 1989–1990 – Analgetiká (nefiltrované)")
plot_ana_f_89  <- plot_indices(ana_f_89, "1989-12",
                               "Vývoj cenových indexov 1989–1990 – Analgetiká (filtrované)")

plot_cso_nf_89 <- plot_indices(cso_nf_89, "1989-12",
                               "Vývoj cenových indexov 1989–1990 – Konzervované polievky (nefiltrované)")
plot_cso_f_89  <- plot_indices(cso_f_89, "1989-12",
                               "Vývoj cenových indexov 1989–1990 – Konzervované polievky (filtrované)")

#-----------------Vypocet ochylky-----------------
sd_ana_nf_89 <- compute_sd(ana_nf_89)
sd_ana_f_89  <- compute_sd(ana_f_89)

sd_cso_nf_89 <- compute_sd(cso_nf_89)
sd_cso_f_89  <- compute_sd(cso_f_89)
#-------------vizualizacia ochylky-------------
p_ana_sd89 <- plot_sd(sd_ana_nf_89, sd_ana_f_89, "1989-12", tit = "Vývoj variability cenových indexov - Analgetiká")
p_cso_sd89 <- plot_sd(sd_cso_nf_89, sd_cso_f_89, "1989-12", tit = "Vývoj variability cenových indexov - Polievky")
#----------------podiely----------------
share_ana_nf_89 <- compute_basket_share(df_agg_ana, df_agg_ana, "1989-12", "1990-12")
share_ana_f_89  <- compute_basket_share(df_fil_ana, df_agg_ana, "1989-12", "1990-12")

share_cso_nf_89 <- compute_basket_share(df_agg_cso, df_agg_cso, "1989-12", "1990-12")
share_cso_f_89  <- compute_basket_share(df_fil_cso, df_agg_cso, "1989-12", "1990-12")

overall_ana_nf_89 <- compute_overall_basket_share(df_agg_ana, df_agg_ana, "1989-12", "1990-12")
overall_ana_f_89  <- compute_overall_basket_share(df_fil_ana, df_agg_ana, "1989-12", "1990-12")

overall_cso_nf_89 <- compute_overall_basket_share(df_agg_cso, df_agg_cso, "1989-12", "1990-12")
overall_cso_f_89  <- compute_overall_basket_share(df_fil_cso, df_agg_cso, "1989-12", "1990-12")
#----------------vizualizacia podielov----------------
p_ana_share89 <- plot_basket_share(share_ana_nf_89, share_ana_f_89, "1989-12",
                                   "1989-12 až 1990-12", "Analgetiká")

p_cso_share89 <- plot_basket_share(share_cso_nf_89, share_cso_f_89, "1989-12",
                                   "1989-12 až 1990-12", "Konzervované polievky")
#----------------stahovanie grafov----------------
ggsave("grafy/ana/ana_inf_89.pdf", 
       plot = plot_ana_nf_89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

ggsave("grafy/ana/ana_if_89.pdf", 
       plot = plot_ana_f_89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

ggsave("grafy/cso/cso_inf_89.pdf", 
       plot = plot_cso_nf_89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

ggsave("grafy/cso/cso_if_89.pdf", 
       plot = plot_cso_f_89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)
#-------plot SD-----------
ggsave("grafy/ana/ana_sd89.pdf", 
       plot = p_ana_sd89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

ggsave("grafy/cso/cso_sd89.pdf", 
       plot = p_cso_sd89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

#-------exp shares-----------
ggsave("grafy/ana/ana_basket_share_89.pdf", 
       plot = p_ana_share89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

ggsave("grafy/cso/cso_basket_share_89.pdf", 
       plot = p_cso_share89, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

#samostatna odchylka, vyluceny chJevons
sd_89_nch_nf <- c()
for(i in 1:13){
  stdev_parc_1 <- c(
    ana_nf_89$Carli[i],
    ana_nf_89$Jevons[i],
    ana_nf_89$Tornquist[i],
    ana_nf_89$YoungA[i]
  )
  sd_89_nch_nf <- c(sd_89_nch_nf, sd(stdev_parc_1))
}

sd_89_nch_f <- c()
for(i in 1:13){
  stdev_parc_2 <- c(
    ana_f_89$Carli[i],
    ana_f_89$Jevons[i],
    ana_f_89$Tornquist[i],
    ana_f_89$YoungA[i]
  )
  sd_89_nch_f <- c(sd_89_nch_f, sd(stdev_parc_2))
}

p_sd_nch <- plot_sd(sd_89_nch_nf, sd_89_nch_f, "1989-12", "Vývoj variability indexov (bez metódy chJevons)")
print(p_sd_nch)

ggsave("grafy/ana/ana_sd_nch.pdf", 
       plot = p_sd_nch, 
       width = 11, height = 6.5, bg = "white",
       device = cairo_pdf)

dfx <- data.frame(nefiltrovane = sd_89_nch_nf, filtrovane = sd_89_nch_f)



