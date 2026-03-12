# ============================================================
# Oppgave 3g -- Symmetrisk 90% intervallestimat
# Genererer pdf, CDF og iCDF for alle 8 grupper
# med markerte 5. og 95. prosentiler
# ============================================================


install.packages(c("ggplot2", "patchwork", "scales"))

library(ggplot2)
library(patchwork)

col_curve <- "red3"
col_shade <- scales::alpha("red3", 0.2)
col_mark  <- "steelblue"

theme_oblig <- theme_classic() +
  theme(
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold")
  )

# Hjelpefunksjon for segmenter (unngaar advarsler fra geom_segment med aes)
seg <- function(x1, y1, x2, y2, ...) {
  annotate("segment", x = x1, xend = x2, y = y1, yend = y2, ...)
}

# Lagringsfunksjon med feilhåndtering
lagre_gruppe <- function(navn, pdf_plot, cdf_plot, icdf_plot) {
  tryCatch({
    kombinert <- pdf_plot + cdf_plot + icdf_plot +
      plot_annotation(
        title = paste("Gruppe", navn),
        theme = theme(plot.title = element_text(size = 12, face = "bold"))
      )
    ggsave(paste0("gruppe_", navn, ".png"), kombinert,
           width = 12, height = 4, dpi = 150)
    message("OK: gruppe_", navn, ".png lagret")
  }, error = function(e) {
    message("FEIL gruppe ", navn, ": ", conditionMessage(e))
  })
}

# x-vektor for kontinuerlige fordelinger (brukes av d, e, f, g, h)
x_kont <- seq(0, 10, length.out = 500)

# ============================================================
# GRUPPE a -- Diskret, ujevn
# ============================================================
tryCatch({
  vals_a <- 1:10
  prob_a <- c(0.03, 0.08, 0.09, 0.03, 0.03, 0.065, 0.14, 0.21, 0.19, 0.075)
  prob_a <- prob_a / sum(prob_a)
  cdf_a  <- cumsum(prob_a)
  p05_a  <- min(vals_a[cdf_a >= 0.05])
  p95_a  <- min(vals_a[cdf_a >= 0.95])
  df_a   <- data.frame(x = vals_a, f = prob_a, F = cdf_a)
  
  pdf_a <- ggplot(df_a, aes(x, f)) +
    geom_bar(stat = "identity",
             fill  = ifelse(vals_a >= p05_a & vals_a <= p95_a, col_shade, "white"),
             color = col_curve, width = 0.4) +
    geom_vline(xintercept = c(p05_a, p95_a), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_a, y = max(prob_a) * 0.95,
             label = paste0("x[0.05]==", p05_a), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_a, y = max(prob_a) * 0.95,
             label = paste0("x[0.95]==", p95_a), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "pdf a", x = "x", y = "f(x)") + theme_oblig
  
  cdf_a_plot <- ggplot(df_a, aes(x, F)) +
    geom_step(color = col_curve, linewidth = 0.8) +
    geom_point(color = col_curve, size = 2) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_a, 0, p05_a, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_a, 0, p95_a, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_a, y = 0.08,
             label = paste0("x[0.05]==", p05_a), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_a, y = 0.92,
             label = paste0("x[0.95]==", p95_a), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "CDF E", x = "x", y = "F(x)") + theme_oblig
  
  icdf_a_df <- data.frame(p = c(0, cdf_a), x = c(vals_a[1], vals_a))
  icdf_a_plot <- ggplot(icdf_a_df, aes(p, x)) +
    geom_step(color = col_curve, linewidth = 0.8, direction = "vh") +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    annotate("text", x = 0.05, y = 9.5, label = "p==0.05", parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9.5, label = "p==0.95", parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_y_continuous(breaks = 1:10) +
    labs(title = "iCDF (a)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("a", pdf_a, cdf_a_plot, icdf_a_plot)
}, error = function(e) message("FEIL gruppe a: ", e$message))

# ============================================================
# GRUPPE b -- Diskret, synkende
# ============================================================
tryCatch({
  vals_b <- 1:10
  prob_b <- c(0.40, 0.25, 0.13, 0.08, 0.05, 0.04, 0.02, 0.015, 0.01, 0.005)
  prob_b <- prob_b / sum(prob_b)
  cdf_b  <- cumsum(prob_b)
  p05_b  <- min(vals_b[cdf_b >= 0.05])
  p95_b  <- min(vals_b[cdf_b >= 0.95])
  df_b   <- data.frame(x = vals_b, f = prob_b, F = cdf_b)
  
  pdf_b <- ggplot(df_b, aes(x, f)) +
    geom_bar(stat = "identity",
             fill  = ifelse(vals_b >= p05_b & vals_b <= p95_b, col_shade, "white"),
             color = col_curve, width = 0.4) +
    geom_vline(xintercept = c(p05_b, p95_b), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_b, y = max(prob_b) * 0.95,
             label = paste0("x[0.05]==", p05_b), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_b, y = max(prob_b) * 0.70,
             label = paste0("x[0.95]==", p95_b), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "pdf b", x = "x", y = "f(x)") + theme_oblig
  
  cdf_b_plot <- ggplot(df_b, aes(x, F)) +
    geom_step(color = col_curve, linewidth = 0.8) +
    geom_point(color = col_curve, size = 2) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_b, 0, p05_b, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_b, 0, p95_b, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_b, y = 0.08,
             label = paste0("x[0.05]==", p05_b), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_b, y = 0.92,
             label = paste0("x[0.95]==", p95_b), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "CDF C", x = "x", y = "F(x)") + theme_oblig
  
  icdf_b_df <- data.frame(p = c(0, cdf_b), x = c(vals_b[1], vals_b))
  icdf_b_plot <- ggplot(icdf_b_df, aes(p, x)) +
    geom_step(color = col_curve, linewidth = 0.8, direction = "vh") +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    annotate("text", x = 0.05, y = 9.5, label = "p==0.05", parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9.5, label = "p==0.95", parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_y_continuous(breaks = 1:10) +
    labs(title = "iCDF (b)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("b", pdf_b, cdf_b_plot, icdf_b_plot)
}, error = function(e) message("FEIL gruppe b: ", e$message))

# ============================================================
# GRUPPE c -- Diskret, uniform
# ============================================================
tryCatch({
  vals_c <- 1:10
  prob_c <- rep(0.1, 10)
  cdf_c  <- cumsum(prob_c)
  p05_c  <- min(vals_c[cdf_c >= 0.05])
  p95_c  <- min(vals_c[cdf_c >= 0.95])
  df_c   <- data.frame(x = vals_c, f = prob_c, F = cdf_c)
  
  pdf_c <- ggplot(df_c, aes(x, f)) +
    geom_bar(stat = "identity", fill = col_shade, color = col_curve, width = 0.4) +
    geom_vline(xintercept = c(p05_c, p95_c), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_c, y = 0.13,
             label = paste0("x[0.05]==", p05_c), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_c, y = 0.13,
             label = paste0("x[0.95]==", p95_c), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) + ylim(0, 0.15) +
    labs(title = "pdf c", x = "x", y = "f(x)") + theme_oblig
  
  cdf_c_plot <- ggplot(df_c, aes(x, F)) +
    geom_step(color = col_curve, linewidth = 0.8) +
    geom_point(color = col_curve, size = 2) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_c, 0, p05_c, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_c, 0, p95_c, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_c, y = 0.08,
             label = paste0("x[0.05]==", p05_c), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_c, y = 0.92,
             label = paste0("x[0.95]==", p95_c), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "CDF G", x = "x", y = "F(x)") + theme_oblig
  
  icdf_c_df <- data.frame(p = c(0, cdf_c), x = c(vals_c[1], vals_c))
  icdf_c_plot <- ggplot(icdf_c_df, aes(p, x)) +
    geom_step(color = col_curve, linewidth = 0.8, direction = "vh") +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    annotate("text", x = 0.05, y = 9.5, label = "p==0.05", parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9.5, label = "p==0.95", parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    scale_y_continuous(breaks = 1:10) +
    labs(title = "iCDF (c)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("c", pdf_c, cdf_c_plot, icdf_c_plot)
}, error = function(e) message("FEIL gruppe c: ", e$message))

# ============================================================
# GRUPPE d -- Kontinuerlig, bimodal
# ============================================================
tryCatch({
  pdf_d_fun <- function(x) 0.55 * dnorm(x, 2, 0.9) + 0.45 * dnorm(x, 6, 1.2)
  cdf_d_fun <- function(x) 0.55 * pnorm(x, 2, 0.9) + 0.45 * pnorm(x, 6, 1.2)
  
  p05_d <- uniroot(function(x) cdf_d_fun(x) - 0.05, c(-5, 15))$root
  p95_d <- uniroot(function(x) cdf_d_fun(x) - 0.95, c(-5, 15))$root
  df_d  <- data.frame(x = x_kont, f = pdf_d_fun(x_kont), F = cdf_d_fun(x_kont))
  
  # iCDF via approxfun (unngaar uniroot-feil)
  x_vid     <- seq(-3, 13, length.out = 3000)
  icdf_d_fn <- approxfun(cdf_d_fun(x_vid), x_vid, rule = 2)
  p_seq_d   <- seq(0.001, 0.999, length.out = 400)
  df_icdf_d <- data.frame(p = p_seq_d, x = icdf_d_fn(p_seq_d))
  
  pdf_d <- ggplot(df_d, aes(x, f)) +
    geom_area(data = subset(df_d, x >= p05_d & x <= p95_d), fill = col_shade) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(p05_d, p95_d), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_d, y = max(df_d$f) * 0.95,
             label = sprintf("x[0.05]==%.2f", p05_d), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_d, y = max(df_d$f) * 0.95,
             label = sprintf("x[0.95]==%.2f", p95_d), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    labs(title = "pdf d", x = "x", y = "f(x)") + theme_oblig
  
  cdf_d_plot <- ggplot(df_d, aes(x, F)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_d, 0, p05_d, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_d, 0, p95_d, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_d, y = 0.08,
             label = sprintf("%.2f", p05_d), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = p95_d, y = 0.92,
             label = sprintf("%.2f", p95_d), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "CDF A", x = "x", y = "F(x)") + theme_oblig
  
  icdf_d_plot <- ggplot(df_icdf_d, aes(p, x)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(0.05, 0, 0.05, p05_d, color = col_mark, linewidth = 0.8) +
    seg(0.95, 0, 0.95, p95_d, color = col_mark, linewidth = 0.8) +
    annotate("text", x = 0.05, y = 9.5, label = "p==0.05", parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9.5, label = "p==0.95", parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    labs(title = "iCDF (d)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("d", pdf_d, cdf_d_plot, icdf_d_plot)
}, error = function(e) message("FEIL gruppe d: ", e$message))

# ============================================================
# GRUPPE e -- Normal(5, 1.5)
# ============================================================
tryCatch({
  mu_e <- 5; sigma_e <- 1.5
  p05_e <- qnorm(0.05, mu_e, sigma_e)
  p95_e <- qnorm(0.95, mu_e, sigma_e)
  df_e  <- data.frame(x = x_kont,
                      f = dnorm(x_kont, mu_e, sigma_e),
                      F = pnorm(x_kont, mu_e, sigma_e))
  p_seq_e   <- seq(0.001, 0.999, length.out = 300)
  df_icdf_e <- data.frame(p = p_seq_e, x = qnorm(p_seq_e, mu_e, sigma_e))
  
  pdf_e <- ggplot(df_e, aes(x, f)) +
    geom_area(data = subset(df_e, x >= p05_e & x <= p95_e), fill = col_shade) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(p05_e, p95_e), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_e, y = max(df_e$f) * 0.95,
             label = sprintf("x[0.05]==%.2f", p05_e), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_e, y = max(df_e$f) * 0.95,
             label = sprintf("x[0.95]==%.2f", p95_e), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    labs(title = "pdf e", x = "x", y = "f(x)") + theme_oblig
  
  cdf_e_plot <- ggplot(df_e, aes(x, F)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_e, 0, p05_e, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_e, 0, p95_e, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_e, y = 0.08,
             label = sprintf("%.2f", p05_e), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = p95_e, y = 0.92,
             label = sprintf("%.2f", p95_e), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "CDF H", x = "x", y = "F(x)") + theme_oblig
  
  icdf_e_plot <- ggplot(df_icdf_e, aes(p, x)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(0.05, 0, 0.05, p05_e, color = col_mark, linewidth = 0.8) +
    seg(0.95, 0, 0.95, p95_e, color = col_mark, linewidth = 0.8) +
    annotate("text", x = 0.05, y = 9,
             label = sprintf("%.2f", p05_e), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9,
             label = sprintf("%.2f", p95_e), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "iCDF (e)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("e", pdf_e, cdf_e_plot, icdf_e_plot)
}, error = function(e) message("FEIL gruppe e: ", e$message))

# ============================================================
# GRUPPE f -- Uniform(0, 10)
# ============================================================
tryCatch({
  p05_f <- qunif(0.05, 0, 10)
  p95_f <- qunif(0.95, 0, 10)
  df_f  <- data.frame(x = x_kont,
                      f = dunif(x_kont, 0, 10),
                      F = punif(x_kont, 0, 10))
  p_seq_f   <- seq(0, 1, length.out = 300)
  df_icdf_f <- data.frame(p = p_seq_f, x = qunif(p_seq_f, 0, 10))
  
  pdf_f <- ggplot(df_f, aes(x, f)) +
    geom_area(data = subset(df_f, x >= p05_f & x <= p95_f), fill = col_shade) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(p05_f, p95_f), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_f, y = 0.13,
             label = sprintf("x[0.05]==%.1f", p05_f), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_f, y = 0.13,
             label = sprintf("x[0.95]==%.1f", p95_f), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    ylim(0, 0.15) +
    labs(title = "pdf f", x = "x", y = "f(x)") + theme_oblig
  
  cdf_f_plot <- ggplot(df_f, aes(x, F)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_f, 0, p05_f, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_f, 0, p95_f, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_f, y = 0.08,
             label = sprintf("%.1f", p05_f), hjust = -0.3, size = 3, color = col_mark) +
    annotate("text", x = p95_f, y = 0.92,
             label = sprintf("%.1f", p95_f), hjust = 1.3, size = 3, color = col_mark) +
    labs(title = "CDF B", x = "x", y = "F(x)") + theme_oblig
  
  icdf_f_plot <- ggplot(df_icdf_f, aes(p, x)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(0.05, 0, 0.05, p05_f, color = col_mark, linewidth = 0.8) +
    seg(0.95, 0, 0.95, p95_f, color = col_mark, linewidth = 0.8) +
    annotate("text", x = 0.05, y = 9,
             label = sprintf("%.1f", p05_f), hjust = -0.3, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9,
             label = sprintf("%.1f", p95_f), hjust = 1.3, size = 3, color = col_mark) +
    labs(title = "iCDF (f)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("f", pdf_f, cdf_f_plot, icdf_f_plot)
}, error = function(e) message("FEIL gruppe f: ", e$message))

# ============================================================
# GRUPPE g -- Triangel(0, 10). F(x) = x^2/100
# ============================================================
tryCatch({
  p05_g   <- 10 * sqrt(0.05)
  p95_g   <- 10 * sqrt(0.95)
  pdf_g_f <- function(x) ifelse(x >= 0 & x <= 10, x / 50, 0)
  cdf_g_f <- function(x) ifelse(x < 0, 0, ifelse(x > 10, 1, x^2 / 100))
  df_g    <- data.frame(x = x_kont, f = pdf_g_f(x_kont), F = cdf_g_f(x_kont))
  
  p_seq_g   <- seq(0, 1, length.out = 300)
  df_icdf_g <- data.frame(p = p_seq_g, x = 10 * sqrt(p_seq_g))
  
  pdf_g <- ggplot(df_g, aes(x, f)) +
    geom_area(data = subset(df_g, x >= p05_g & x <= p95_g), fill = col_shade) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(p05_g, p95_g), color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_g, y = 0.23,
             label = sprintf("x[0.05]==%.2f", p05_g), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = p95_g, y = 0.23,
             label = sprintf("x[0.95]==%.2f", p95_g), parse = TRUE,
             hjust = 1.1, size = 3, color = col_mark) +
    labs(title = "pdf g", x = "x", y = "f(x)") + theme_oblig
  
  cdf_g_plot <- ggplot(df_g, aes(x, F)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_g, 0, p05_g, 0.05, color = col_mark, linewidth = 0.8) +
    seg(p95_g, 0, p95_g, 0.95, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_g, y = 0.08,
             label = sprintf("%.2f", p05_g), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = p95_g, y = 0.92,
             label = sprintf("%.2f", p95_g), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "CDF F", x = "x", y = "F(x)") + theme_oblig
  
  icdf_g_plot <- ggplot(df_icdf_g, aes(p, x)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(0.05, 0, 0.05, p05_g, color = col_mark, linewidth = 0.8) +
    seg(0.95, 0, 0.95, p95_g, color = col_mark, linewidth = 0.8) +
    annotate("text", x = 0.05, y = 9.5,
             label = sprintf("%.2f", p05_g), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = 9.5,
             label = sprintf("%.2f", p95_g), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "iCDF (g)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("g", pdf_g, cdf_g_plot, icdf_g_plot)
}, error = function(e) message("FEIL gruppe g: ", e$message))

# ============================================================
# GRUPPE h -- Eksponensiell(rate=0.2)
# 95. prosentil er ~15, utenfor x-aksen [0,10]
# ============================================================
tryCatch({
  rate_h <- 0.2
  p05_h  <- qexp(0.05, rate_h)
  p95_h  <- qexp(0.95, rate_h)
  df_h   <- data.frame(x = x_kont,
                       f = dexp(x_kont, rate_h),
                       F = pexp(x_kont, rate_h))
  p_seq_h   <- seq(0.001, 0.99, length.out = 300)
  df_icdf_h <- data.frame(p = p_seq_h, x = qexp(p_seq_h, rate_h))
  
  pdf_h <- ggplot(df_h, aes(x, f)) +
    geom_area(data = subset(df_h, x >= p05_h), fill = col_shade) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = p05_h, color = col_mark,
               linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = p05_h, y = max(df_h$f) * 0.9,
             label = sprintf("x[0.05]==%.2f", p05_h), parse = TRUE,
             hjust = -0.1, size = 3, color = col_mark) +
    annotate("text", x = 4.5, y = max(df_h$f) * 0.45,
             label = "x[0.95] == 14.98 (utenfor)", parse = FALSE,
             hjust = 0, size = 3, color = col_mark) +
    labs(title = "pdf h", x = "x", y = "f(x)") + theme_oblig
  
  cdf_h_plot <- ggplot(df_h, aes(x, F)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_hline(yintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(p05_h, 0, p05_h, 0.05, color = col_mark, linewidth = 0.8) +
    annotate("text", x = p05_h, y = 0.08,
             label = sprintf("%.2f", p05_h), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = 4, y = 0.95,
             label = "x[0.95] = 14.98 (utenfor)", parse = FALSE,
             hjust = 0, size = 3, color = col_mark) +
    labs(title = "CDF D", x = "x", y = "F(x)") + theme_oblig
  
  icdf_h_plot <- ggplot(df_icdf_h, aes(p, x)) +
    geom_line(color = col_curve, linewidth = 0.8) +
    geom_vline(xintercept = c(0.05, 0.95), color = col_mark, linetype = "dashed") +
    seg(0.05, 0, 0.05, p05_h, color = col_mark, linewidth = 0.8) +
    seg(0.95, 0, 0.95, p95_h, color = col_mark, linewidth = 0.8) +
    annotate("text", x = 0.05, y = max(df_icdf_h$x) * 0.9,
             label = sprintf("%.2f", p05_h), hjust = -0.2, size = 3, color = col_mark) +
    annotate("text", x = 0.95, y = max(df_icdf_h$x) * 0.9,
             label = sprintf("%.2f", p95_h), hjust = 1.2, size = 3, color = col_mark) +
    labs(title = "iCDF (h)", x = "p", y = "F-inv(p)") + theme_oblig
  
  lagre_gruppe("h", pdf_h, cdf_h_plot, icdf_h_plot)
}, error = function(e) message("FEIL gruppe h: ", e$message))

message("\nFerdig! Sjekk konsollen for eventuelle feilmeldinger.")