# ==============================================================================
# PROJET : Le << paradoxe du bonheur >> en France : le revenu par habitant 
#          explique-t-il réellement la satisfaction de vie ?
# AUTEURS : PESENTI Alexandre, WOJTCZAK Baptiste, LACROIX Igor
# DATE : Décembre 2025
# DESCRIPTION : Script générant les figures du compte rendu (Fig 1 à 7)
# ==============================================================================

# ==============================================================================
# 0. CONFIGURATION ET IMPORTATION
# ==============================================================================

# --- Chargement des paquets ---
library(dplyr)
library(ggplot2)
library(broom)
library(viridisLite)
library(scales)
library(WDI)

# --- Importation des données ---
# Chemin à adapter selon l'utilisateur : mettez le votre
file_path <- "C:/Users/dizzy/OneDrive/Documents/1A/R/DM/data/ESS9e03_2.csv"

# Import principal
data_ess <- read.csv(file_path, sep = ",", dec = ".", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# ==============================================================================
# 1. PRÉPARATION DES INDICATEURS (SETUP GLOBAL)
# ==============================================================================

# --- Fonctions utilitaires ---
ess_to_na_scale <- function(x, valid_min, valid_max) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  x[x < 0] <- NA_real_
  x[x %in% c(77,88,99, 777,888,999, 7777,8888,9999)] <- NA_real_
  x[x < valid_min | x > valid_max] <- NA_real_
  x
}

rowMeanMin <- function(M, min_nonmiss = 1) {
  nn <- rowSums(!is.na(M))
  out <- rowMeans(M, na.rm = TRUE)
  out[nn < min_nonmiss] <- NA_real_
  out
}

rev_scale <- function(x, minv, maxv) ifelse(is.na(x), NA_real_, (maxv + minv) - x)

to10_minmax <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  x[x < 0] <- NA_real_
  x[x %in% c(77,88,99, 777,888,999, 7777,8888,9999)] <- NA_real_
  mn <- min(x, na.rm = TRUE); mx <- max(x, na.rm = TRUE)
  if (!is.finite(mn) || !is.finite(mx) || mx == mn) return(rep(NA_real_, length(x)))
  10 * (x - mn) / (mx - mn)
}

# --- Construction : SWB_10 (Subjective Well-Being) ---
data_ess$happy   <- ess_to_na_scale(data_ess$happy,   0, 10)
data_ess$stflife <- ess_to_na_scale(data_ess$stflife, 0, 10)
data_ess$SWB_10  <- rowMeanMin(cbind(data_ess$happy, data_ess$stflife), min_nonmiss = 2)

# --- Construction : Indicateur Mean4_10 ---
# Trust_10
for (v in c("ppltrst","pplfair","pplhlp")) data_ess[[v]] <- ess_to_na_scale(data_ess[[v]], 0, 10)
data_ess$Trust_10 <- rowMeanMin(cbind(data_ess$ppltrst, data_ess$pplfair, data_ess$pplhlp), min_nonmiss = 2)

# Social_10
sclmeet_10 <- to10_minmax(data_ess$sclmeet)
inprdsc_10 <- to10_minmax(data_ess$inprdsc)
sclact_10  <- to10_minmax(data_ess$sclact)
data_ess$Social_10 <- rowMeanMin(cbind(sclmeet_10, inprdsc_10, sclact_10), min_nonmiss = 2)

# Health_10
data_ess$health  <- ess_to_na_scale(data_ess$health,  1, 5)
data_ess$hlthhmp <- ess_to_na_scale(data_ess$hlthhmp, 1, 3)
health_pos  <- rev_scale(data_ess$health,  1, 5)
hlthhmp_pos <- rev_scale(data_ess$hlthhmp, 1, 3)
health_10  <- 10 * (health_pos  - 1) / (5 - 1)
hlthhmp_10 <- 10 * (hlthhmp_pos - 1) / (3 - 1)
data_ess$Health_10 <- rowMeanMin(cbind(health_10, hlthhmp_10), min_nonmiss = 2)

# Safety_10
data_ess$aesfdrk <- ess_to_na_scale(data_ess$aesfdrk, 1, 4)
aesfdrk_pos <- rev_scale(data_ess$aesfdrk, 1, 4)
aesfdrk_10  <- 10 * (aesfdrk_pos - 1) / (4 - 1)
data_ess$crmvct <- ess_to_na_scale(data_ess$crmvct, 1, 2)
not_victim <- ifelse(is.na(data_ess$crmvct), NA_real_,
                     ifelse(data_ess$crmvct == 2, 1, ifelse(data_ess$crmvct == 1, 0, NA_real_)))
not_victim_10 <- 10 * not_victim
data_ess$Safety_10 <- rowMeanMin(cbind(aesfdrk_10, not_victim_10), min_nonmiss = 2)

# Global Mean4_10
data_ess$Mean4_10 <- rowMeanMin(
  cbind(data_ess$Trust_10, data_ess$Social_10, data_ess$Health_10, data_ess$Safety_10),
  min_nonmiss = 3
)

# --- Nettoyage Revenu (Déciles 1-10) ---
x <- suppressWarnings(as.numeric(as.character(data_ess$hinctnta)))
x[x < 0] <- NA_real_
x[!(x %in% 1:10)] <- NA_real_
data_ess$hinctnta <- x

# ==============================================================================
# 2. PARTIE 1 : LA FRANCE (Figures 1 & 2)
# ==============================================================================

# Préparation données France
data_france <- data_ess[data_ess$cntry == "FR", ]
data_france$stflife_clean <- suppressWarnings(as.numeric(data_france$stflife))
data_france$stflife_clean[data_france$stflife_clean < 0 | data_france$stflife_clean > 10] <- NA
data_france$hinctnta_clean <- data_france$hinctnta
data_france$anweight_clean <- suppressWarnings(as.numeric(data_france$anweight))

data_france_clean <- data_france[!is.na(data_france$stflife_clean) & 
                                   !is.na(data_france$hinctnta_clean) & 
                                   !is.na(data_france$anweight_clean), ]

# ------------------------------------------------------------------------------
# FIGURE 1 : Relation Revenu-Satisfaction (Moyennes pondérées)
# ------------------------------------------------------------------------------
deciles <- sort(unique(data_france_clean$hinctnta_clean))
satisfaction_moyenne_vector <- numeric(length(deciles))

for (i in seq_along(deciles)) {
  decile_data <- data_france_clean[data_france_clean$hinctnta_clean == deciles[i], ]
  satisfaction_moyenne_vector[i] <- weighted.mean(
    x = decile_data$stflife_clean, w = decile_data$anweight_clean, na.rm = TRUE
  )
}

satisfaction_par_revenu_base <- data.frame(Decile_Revenu = deciles, satisfaction_moyenne = satisfaction_moyenne_vector)

plot(
  x = satisfaction_par_revenu_base$Decile_Revenu,
  y = satisfaction_par_revenu_base$satisfaction_moyenne,
  type = "b", pch = 19, col = "#0072B2",
  ylim = c(min(satisfaction_par_revenu_base$satisfaction_moyenne) - 0.1, max(satisfaction_par_revenu_base$satisfaction_moyenne) + 0.1),
  xaxt = "n",
  xlab = "Décile de Revenu Net (1 = plus faible, 10 = plus élevé)",
  ylab = "Satisfaction de Vie Moyenne (Échelle 0-10)",
  main = "Relation Revenu–Satisfaction en France (Moyennes pondérées)"
)
axis(side = 1, at = 1:10)
model_reg <- lm(satisfaction_moyenne ~ Decile_Revenu, data = satisfaction_par_revenu_base)
abline(model_reg, col = "red", lty = 2)
legend("topleft", legend = c("Moyenne par décile", "Ligne de Tendance"), col = c("#0072B2", "red"), lty = c(1, 2), pch = c(19, NA), bty = "n")

# ------------------------------------------------------------------------------
# FIGURE 2a : Distribution des déciles selon la satisfaction (Barres empilées)
# ------------------------------------------------------------------------------

# 1. Préparation des données spécifiques pour ce graphique
# On repart de data_ess pour garantir les types (integer) nécessaires à xtabs
df_bar <- data_ess[data_ess$cntry == "FR", c("stflife", "hinctnta", "anweight")]

# Nettoyage et conversion en entiers
df_bar$stflife  <- suppressWarnings(as.integer(df_bar$stflife))
df_bar$hinctnta <- suppressWarnings(as.integer(df_bar$hinctnta))
df_bar$anweight <- suppressWarnings(as.numeric(df_bar$anweight))

# Exclusion des valeurs hors limites et NA
df_bar$stflife[df_bar$stflife < 0 | df_bar$stflife > 10] <- NA
df_bar$hinctnta[df_bar$hinctnta < 1 | df_bar$hinctnta > 10] <- NA
df_bar <- na.omit(df_bar)

# 2. Création de la matrice pondérée
W <- xtabs(anweight ~ stflife + hinctnta, data = df_bar)

# Création d'une matrice complète (0-10 satisfaction x D1-D10 revenu)
# Cela garantit que le graphique a bien toutes les barres même si une catégorie est vide
W_full <- matrix(0, nrow = 11, ncol = 10, 
                 dimnames = list(stflife = 0:10, decile = paste0("D", 1:10)))

# Remplissage avec les données observées
if (nrow(W) > 0) {
  rows_present <- rownames(W)
  cols_present <- paste0("D", colnames(W))
  W_full[rows_present, cols_present] <- W
}

# 3. Calcul des proportions (Profils lignes)
# Chaque barre de satisfaction totalisera 100% (répartis entre les déciles)
row_tot <- rowSums(W_full)
P_plot <- W_full
# Division par la somme en ligne (si > 0)
P_plot[row_tot > 0, ] <- P_plot[row_tot > 0, ] / row_tot[row_tot > 0]
P_plot[is.na(P_plot)] <- 0 # Sécurité

# 4. Tracé du graphique
# Sauvegarde des paramètres graphiques
op <- par(no.readonly = TRUE)
# Marge droite élargie pour la légende
par(mar = c(6, 4, 4, 8) + 0.1)

# Palette de couleurs (Bleu clair vers Bleu foncé)
dec_cols <- colorRampPalette(c("#deebf7", "#3182bd", "#08306b"))(10)

barplot(
  height = t(P_plot),      # Transposé pour empiler les déciles
  beside = FALSE,          # Barres empilées
  col = dec_cols,
  border = NA,
  ylim = c(0, 1),
  yaxt = "n",
  xlab = "Satisfaction dans la vie (0–10)",
  ylab = "Proportion",
  names.arg = rownames(P_plot),
  main = "Figure 2a : Distribution des déciles selon la satisfaction"
)

# Axe Y en pourcentage
ticks <- seq(0, 1, by = 0.1)
axis(2, at = ticks, labels = paste0(round(100 * ticks), "%"), las = 1)

# Légende sur la droite
legend(
  "topright",
  inset = c(-0.25, 0),     # Décalage vers la droite (hors du cadre)
  xpd = TRUE,              # Autorise l'affichage hors du cadre
  legend = paste0("D", 1:10),
  fill = dec_cols,
  border = NA,
  title = "Décile revenu",
  cex = 0.8
)

# Restauration des paramètres graphiques
par(op)

# ------------------------------------------------------------------------------
# FIGURE 2b : Distribution de la Satisfaction par Décile (Boxplot)
# ------------------------------------------------------------------------------
boxplot(
  stflife_clean ~ hinctnta_clean,
  data = data_france_clean,
  col = colorRampPalette(c("#deebf7", "#3182bd", "#08306b"))(10),
  xlab = "Décile de Revenu Net",
  ylab = "Distribution de la Satisfaction de Vie (0-10)",
  main = "Distribution de la Satisfaction par Décile de Revenu (France)",
  outline = FALSE
)

# ==============================================================================
# 3. PARTIE 1.2 : ÉTUDE MULTIVARIÉE (Figure 3)
# ==============================================================================

# Note: Ce bloc utilise un nettoyage spécifique indépendant
# On réutilise data_ess pour filtrer la France, mais on applique le nettoyage "Modèle" du script original

ess <- data_ess[data_ess$cntry == "FR", ] # Filtre France
w <- if ("anweight" %in% names(ess)) suppressWarnings(as.numeric(ess$anweight)) else rep(1, nrow(ess))
w[is.na(w) | w <= 0] <- 1
ess$w <- w

y_var <- "stflife" 
vars_15 <- c("hinctnta","hincfel","uempla","eisced","health","hlthhmp","sclmeet","sclact","ppltrst","stfeco","trstprl")

# Nettoyage spécifique régression
ess[[y_var]] <- suppressWarnings(as.numeric(ess[[y_var]]))
ess[[y_var]][ess[[y_var]] > 10] <- NA

for(v in vars_15) {
  if(v %in% names(ess)) {
    ess[[v]] <- suppressWarnings(as.numeric(ess[[v]]))
    if(v %in% c("stfeco", "ppltrst", "trstprl", "hinctnta")) ess[[v]][ess[[v]] > 10] <- NA
    if(v %in% c("eisced", "uempla")) ess[[v]][ess[[v]] > 60] <- NA
  }
}

# Inversions (Santé, Aisance)
ess$health <- ifelse(ess$health %in% 1:5, 6 - ess$health, NA_real_)
ess$hincfel <- ifelse(ess$hincfel %in% 1:4, 5 - ess$hincfel, NA_real_)

# Modèle
mod_df <- ess[, c(y_var, vars_15, "w")]
mod_df <- mod_df[complete.cases(mod_df), ]
form <- as.formula(paste(y_var, "~", paste(vars_15, collapse = " + ")))
m_full <- lm(formula = form, data = mod_df, weights = mod_df$w)

# Coefficients et Graphique 3
b <- coef(m_full); ci <- suppressMessages(confint(m_full))
terms <- names(b); keep <- terms != "(Intercept)"
coef_tab <- data.frame(term = terms[keep], estimate = unname(b[keep]), ci_low = ci[keep, 1], ci_high = ci[keep, 2])

label_term <- function(t) {
  switch(t,
         "hinctnta" = "Décile du revenu du ménage", "hincfel" = "Aisance financière ressentie",
         "uempla" = "Au chômage (Oui/Non)", "eisced" = "Niveau d'études (ISCED)",
         "health" = "Santé (auto-évaluée)", "hlthhmp" = "Limité par santé/handicap",
         "sclmeet" = "Sociabilité (rencontres)", "sclact" = "Vie sociale (activités)",
         "ppltrst" = "Confiance interpersonnelle", "stfeco" = "Satisfaction économie",
         "trstprl" = "Confiance parlement", t)
}
coef_tab$label <- vapply(coef_tab$term, label_term, character(1))
coef_tab <- coef_tab[order(coef_tab$estimate), ]

# Trace Figure 3
par(mar = c(5, 12, 4, 2) + 0.1)
y_pos <- seq_len(nrow(coef_tab))
plot(
  coef_tab$estimate, y_pos, pch = 19,
  xlim = range(c(coef_tab$ci_low, coef_tab$ci_high), na.rm = TRUE),
  yaxt = "n", xlab = "Effet estimé (points de satisfaction)", ylab = "",
  main = "FR - Régression multivariée"
)
axis(2, at = y_pos, labels = coef_tab$label, las = 1)
abline(v = 0, lty = 2)
segments(coef_tab$ci_low, y_pos, coef_tab$ci_high, y_pos)

# Affichage Tableau 1 dans la console
print(summary(m_full))
par(mar = c(5, 4, 4, 2) + 0.1) # Reset marges

# ==============================================================================
# 4. PARTIE 2 : COMPARAISON INTERNATIONALE (Figure 4)
# ==============================================================================

# --- Récupération PIB (WDI) ---
year_ref <- 2018
# Utilisation de tryCatch pour éviter l'erreur si pas internet, mais le code suppose WDI
wb <- try(WDI::WDI(country = "all", indicator = c(gdp_pc_ppp = "NY.GDP.PCAP.PP.CD"), start = year_ref, end = year_ref, extra = FALSE), silent=TRUE)

if(!inherits(wb, "try-error")) {
  gdp_df <- wb %>% transmute(cntry = iso2c, gdp_pc_ppp = as.numeric(gdp_pc_ppp), country_name = country) %>% distinct(cntry, .keep_all = TRUE)
  
  # Préparation données
  w <- if ("anweight" %in% names(data_ess)) suppressWarnings(as.numeric(data_ess$anweight)) else rep(1, nrow(data_ess))
  w[is.na(w) | w <= 0] <- 1
  dat_g <- data.frame(cntry = data_ess$cntry, w = w, stflife = suppressWarnings(as.numeric(data_ess$stflife)), hinctnta = suppressWarnings(as.integer(data_ess$hinctnta)), stringsAsFactors = FALSE)
  dat_g <- dat_g %>% filter(!is.na(cntry), !is.na(stflife), stflife >= 0, stflife <= 10, !is.na(hinctnta), hinctnta %in% 1:10)
  
  # ----------------------------------------------------------------------------
  # FIGURE 4a : Bonheur moyen selon le revenu (Spaghetti plot)
  # ----------------------------------------------------------------------------
  curve_df <- dat_g %>% group_by(cntry, hinctnta) %>% summarise(mean_stflife = weighted.mean(stflife, w, na.rm = TRUE), .groups = "drop")
  curve_gdp <- curve_df %>% left_join(gdp_df %>% distinct(cntry, gdp_pc_ppp), by = "cntry") %>% filter(!is.na(gdp_pc_ppp))
  
  p_spaghetti <- ggplot(curve_gdp, aes(x = hinctnta, y = mean_stflife, group = cntry, color = gdp_pc_ppp)) +
    geom_line(alpha = 0.45, linewidth = 0.7) + geom_point(alpha = 0.45, size = 0.9) +
    scale_x_continuous(breaks = 1:10) + coord_cartesian(ylim = c(4, 9)) +
    scale_color_viridis_c(option = "plasma", direction = -1, name = paste0("PIB/hab PPP\n(", year_ref, ")")) +
    labs(title = "Bonheur moyen selon le revenu", x = "Catégorie de revenu", y = "Satisfaction de vie moyenne") +
    theme_minimal(base_size = 12)
  print(p_spaghetti)
  
  # ----------------------------------------------------------------------------
  # FIGURE 4b : Pentes par pays (Barplot)
  # ----------------------------------------------------------------------------
  slope_df <- dat_g %>% group_by(cntry) %>% group_modify(~{
    if (dplyr::n_distinct(.x$hinctnta, na.rm = TRUE) < 2) return(tibble(slope = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
    m <- lm(stflife ~ hinctnta, data = .x, weights = .x$w)
    b <- broom::tidy(m, conf.int = TRUE) %>% filter(term == "hinctnta")
    tibble(slope = b$estimate, ci_low = b$conf.low, ci_high = b$conf.high)
  }) %>% ungroup() %>% left_join(gdp_df, by = "cntry") %>% filter(!is.na(gdp_pc_ppp))
  
  slope_df <- slope_df %>% arrange(slope) %>% mutate(country_name = factor(cntry, levels = unique(cntry))) # Utilisation code ISO pour affichage
  
  p_slopes <- ggplot(slope_df, aes(x = country_name, y = slope, fill = gdp_pc_ppp)) +
    geom_col(alpha = 0.95) + geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, alpha = 0.6) +
    scale_fill_viridis_c(option = "plasma", direction = -1, name = paste0("PIB/hab PPP\n(", year_ref, ")")) +
    labs(title = "Pente revenu–bonheur par pays", x = "Pays", y = "Pente") +
    theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(p_slopes)
} else {
  message("Impossible de télécharger les données PIB (WDI). Figures 4a/4b ignorées.")
}

# ==============================================================================
# 5. PARTIE 2.2 : HEATMAP (Figure 5)
# ==============================================================================

# --- dépendance minimale pour tracer ---
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

theme_set(theme_minimal(base_size = 12))

# --- paramètres (tu peux ajuster) ---
min_n <- 300         # cible de cas complets par pays pour la régression
min_n_floor <- 50    # minimum requis pour autoriser le resampling
do_resample <- TRUE  # TRUE = complète à min_n par tirage avec remise si n < min_n

# --- source de données ---
ess <- data_ess

# --- poids (si anweight absent -> 1) ---
w <- if ("anweight" %in% names(ess)) suppressWarnings(as.numeric(ess$anweight)) else rep(1, nrow(ess))
w[is.na(w) | w <= 0] <- 1

# --- liste variables candidates (15) ---
vars_15 <- c(
  "hinctnta","hincfel","uempla","eisced","agea","health","hampered",
  "sclmeet","sclact","ppltrst","stfdem","trstprl","trstplt","trstlgl","stfeco"
)

vars_present <- intersect(vars_15, names(ess))
message("Variables présentes utilisées : ", paste(vars_present, collapse = ", "))
if (length(setdiff(vars_15, vars_present)) > 0) {
  message("Variables absentes ignorées : ", paste(setdiff(vars_15, vars_present), collapse = ", "))
}

# --- fonctions de nettoyage (fidèles à l’esprit ESS) ---
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

clean_010 <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(77,88,99)] <- NA_real_; x } # 0-10
clean_110 <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(77,88,99)] <- NA_real_; x } # 1-10
clean_17  <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(77,88,99)] <- NA_real_; x } # 1-7
clean_15  <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(7,8,9)]  <- NA_real_; x }  # 1-5
clean_14  <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(7,8,9)]  <- NA_real_; x }  # 1-4
clean_13  <- function(x) { x <- to_num(x); x[x < 0] <- NA_real_; x[x %in% c(7,8,9)]  <- NA_real_; x }  # 1-3

# --- construction de dat (équivalent logique) ---
dat <- data.frame(
  cntry   = ess$cntry,
  w       = w,
  stflife = clean_010(ess$stflife),
  stringsAsFactors = FALSE
)

# ajoute les variables présentes (brutes en numérique)
for (v in vars_present) dat[[v]] <- to_num(ess[[v]])

# nettoyages/recode ciblés (comme ton script heatmap)
if ("hinctnta" %in% names(dat)) dat$hinctnta <- clean_110(dat$hinctnta)

if ("hincfel" %in% names(dat)) {
  dat$hincfel <- clean_14(dat$hincfel)
  dat$hincfel_ease <- ifelse(!is.na(dat$hincfel), 5 - dat$hincfel, NA_real_)
  dat$hincfel <- NULL
}

if ("sclmeet" %in% names(dat)) dat$sclmeet <- clean_17(dat$sclmeet)
if ("sclact"  %in% names(dat)) dat$sclact  <- clean_17(dat$sclact)

for (v in c("ppltrst","stfdem","trstprl","trstplt","trstlgl","stfeco")) {
  if (v %in% names(dat)) dat[[v]] <- clean_010(dat[[v]])
}

if ("health" %in% names(dat)) {
  dat$health <- clean_15(dat$health)
  dat$health_rev <- ifelse(!is.na(dat$health), 6 - dat$health, NA_real_)
  dat$health <- NULL
}

if ("hampered" %in% names(dat)) dat$hampered <- clean_13(dat$hampered)

if ("agea" %in% names(dat)) {
  dat$agea <- to_num(dat$agea)
  dat$agea[dat$agea >= 900] <- NA_real_
}

# supprime lignes sans cntry ou stflife
dat <- dat[!is.na(dat$cntry) & !is.na(dat$stflife), , drop = FALSE]

# --- prédicteurs finaux ---
predictors <- setdiff(names(dat), c("cntry", "w", "stflife"))

# --- z-scores pondérés par pays ---
w_mean <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
w_sd <- function(x, w) {
  m <- w_mean(x, w)
  sqrt(sum(w * (x - m)^2, na.rm = TRUE) / sum(w[!is.na(x)]))
}

std_one_country <- function(df, predictors) {
  zcols <- c("stflife", predictors)
  for (v in zcols) {
    s <- w_sd(df[[v]], df$w)
    m <- w_mean(df[[v]], df$w)
    df[[paste0(v, "_z")]] <- if (is.na(s) || s == 0) NA_real_ else (df[[v]] - m) / s
  }
  df
}

spl <- split(dat, dat$cntry)
dat_z <- do.call(rbind, lapply(spl, std_one_country, predictors = predictors))
row.names(dat_z) <- NULL

# --- régression multivariée par pays (pondérée), avec resampling optionnel ---
fit_one_country <- function(df, predictors, min_n, min_n_floor, do_resample) {
  y <- "stflife_z"
  X <- paste0(predictors, "_z")
  X <- X[X %in% names(df)]
  
  if (!(y %in% names(df)) || length(X) == 0) return(NULL)
  
  df2 <- df[, c("w", y, X), drop = FALSE]
  df2 <- df2[complete.cases(df2), , drop = FALSE]
  n0 <- nrow(df2)
  if (n0 < 2) return(NULL)
  
  if (n0 < min_n && do_resample) {
    if (n0 < min_n_floor) return(NULL)
    idx <- sample.int(n0, size = min_n, replace = TRUE)
    df2 <- df2[idx, , drop = FALSE]
  }
  
  f <- as.formula(paste(y, "~", paste(X, collapse = " + ")))
  m <- lm(f, data = df2, weights = df2$w)
  
  cc <- coef(m)
  cc <- cc[setdiff(names(cc), "(Intercept)")]
  if (length(cc) == 0) return(NULL)
  
  data.frame(term = names(cc), estimate = as.numeric(cc), stringsAsFactors = FALSE)
}

coef_list <- lapply(split(dat_z, dat_z$cntry),
                    fit_one_country,
                    predictors = predictors,
                    min_n = min_n,
                    min_n_floor = min_n_floor,
                    do_resample = do_resample)

coef_all <- do.call(rbind, Map(function(cn, res) {
  if (is.null(res) || nrow(res) == 0) return(NULL)
  cbind(cntry = cn, res)
}, names(coef_list), coef_list))

if (is.null(coef_all) || nrow(coef_all) == 0) {
  stop("Aucun pays n'a produit de coefficients. Baisse min_n, baisse min_n_floor, ou mets do_resample=FALSE.")
}

# --- labels lisibles (identiques au script heatmap) ---
label_map <- c(
  agea_z          = "Âge",
  health_rev_z    = "Santé",
  eisced_z        = "Études",
  uempla_z        = "Chômage",
  hinctnta_z      = "Décile de revenu du ménage",
  hincfel_ease_z  = "Aisance ressentie",
  hampered_z      = "Limitation santé",
  sclmeet_z       = "Sociabilité et rencontres",
  sclact_z        = "Activités sociales",
  ppltrst_z       = "Confiance interpersonnelle",
  stfdem_z        = "Satisfaction démocratie",
  trstprl_z       = "Confiance parlement",
  trstplt_z       = "Confiance politiciens",
  trstlgl_z       = "Confiance justice",
  stfeco_z        = "Satisfaction économie"
)

coef_all$var <- unname(label_map[coef_all$term])
coef_all <- coef_all[!is.na(coef_all$var), , drop = FALSE]

# --- ordre pays : moyenne pondérée de stflife (comme le script) ---
cntry_levels <- names(sort(tapply(seq_len(nrow(dat)), dat$cntry, function(ii) w_mean(dat$stflife[ii], dat$w[ii])),
                           decreasing = TRUE))

var_levels <- unname(label_map)

coef_all$cntry <- factor(coef_all$cntry, levels = cntry_levels)
coef_all$var   <- factor(coef_all$var,   levels = var_levels)

# --- heatmap ---
p_heat <- ggplot(coef_all, aes(x = var, y = cntry, fill = estimate)) +
  geom_tile() +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0,
                       name = "β standardisé") +
  labs(
    title = "Heatmap — Effets standardisés sur la satisfaction de vie, par pays",
    subtitle = "Chaque cellule = coefficient β d'une régression par pays",
    x = NULL,
    y = "Pays (code ESS)"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 7)
  )

print(p_heat)


# ==============================================================================
# 6. PARTIE 3 : INDICATEUR OBJECTIF (Figure 6)
# ==============================================================================

# Fonction Binned Points
binned_points <- function(x, y, K = 50, x_max = 9.5) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if (length(x) == 0) return(list(x = numeric(0), y = numeric(0)))
  rnk <- rank(x, ties.method = "random")
  bin <- floor((rnk - 1) / length(x) * min(K, length(x))) + 1
  list(x = tapply(x, bin, mean, na.rm = TRUE), y = tapply(y, bin, mean, na.rm = TRUE))
}

# Figure 6
ok <- complete.cases(data_ess$SWB_10, data_ess$Mean4_10)
pts0 <- binned_points(data_ess$SWB_10[ok], data_ess$Mean4_10[ok], K = 50, x_max = 9.5)

plot(
  pts0$x, pts0$y, pch = 16,
  xlab = "Moyenne entre bonheur et satisfaction dans la vie (0–10)",
  ylab = "Indicateur construit (Mean4_10)",
  main = "Bonheur subjectif en fonction de l'indicateur"
)
grid()
fit0 <- lm(pts0$y ~ pts0$x)
abline(fit0, lwd = 2)
legend("topleft", legend = c(paste0("corr = ", round(cor(pts0$x, pts0$y), 3)), paste0("R² = ", round(summary(fit0)$r.squared, 3))), bty = "n")

# ==============================================================================
# 7. PARTIE 3.3 : COURBES INDICATEURS PAR DÉCILE (Figure 7)
# ==============================================================================

df_fr <- data_ess[data_ess$cntry == "FR", ]
series_cols <- c("Health_10","Social_10","Safety_10","Trust_10","Mean4_10","SWB_10")
means <- list(); ns <- list()

for (nm in series_cols) {
  means[[nm]] <- tapply(df_fr[[nm]], df_fr$hinctnta, mean, na.rm = TRUE)
  ns[[nm]] <- tapply(df_fr[[nm]], df_fr$hinctnta, function(x) sum(!is.na(x)))
}

cols <- c("steelblue4","firebrick3","darkgreen","darkorange3","purple4","black")
names(cols) <- series_cols
ltys <- c(1,1,1,1,2,2); pchv <- c(16,16,16,16,17,15); names(ltys) <- series_cols; names(pchv) <- series_cols

plot(1:10, 1:10, type = "n", ylim = c(3, 9), xlab = "Décile de revenu", ylab = "Moyenne (0–10)", main = "Moyenne des indicateurs (0–10) par décile — France")
grid()

for (nm in series_cols) {
  y <- means[[nm]]; x <- as.numeric(names(y))
  lines(x, y, type = "b", col = cols[nm], lty = ltys[nm], lwd = 2, pch = pchv[nm])
}

legend("topleft", legend = c("Santé","Social","Sécurité","Confiance","Moyenne des 4","Bien-être subjectif"),
       col = cols[series_cols], lty = ltys[series_cols], lwd = 2, pch = pchv[series_cols], bty = "o", bg = "white")

# Fin du script