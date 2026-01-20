# NOTE:
# All results reported in the main paper are based on complete-case data (N = 486).
# The imputation, scaling, and train/test split code included below is exploratory
# and provided for transparency and potential future extension only.
## ---- 0) Reset (avoid leftover objects) 
#clearing the environment so that results are reproducible
rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

## ---- 1) Load ----
# set the dataset path
data_path <- "billboard_24years_lyrics_spotify.csv"
bb0 <- readr::read_csv(data_path, show_col_types = FALSE)

# Make safe, lower_snake names without janitor:
#this removes special character and spaces, replacing them with underscores
safe_names <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("_+", "_", x))
}
names(bb0) <- safe_names(names(bb0))

# Quick peek
glimpse(bb0)

## ---- 2) Minimal cleaning / standardisation ----
#  dataset has: ranking, song, band_singer, year, lyrics, and spotify features.
# Create consistent fields we’ll use.
bb <- bb0 %>%
  mutate(
    # Ranking is year-end rank; lower = better
    rank = as.numeric(ranking),
    # Title/artist
    title = song,
    artist = band_singer,
    # keep year, ensure numeric
    year  = as.integer(year)
  ) %>%
  # Assignment scope
  filter(!is.na(year), dplyr::between(year, 2000, 2023)) %>%
  # Basic validity: need title, artist, rank, year
  filter(!is.na(title), !is.na(artist), !is.na(rank))

# Build an ID for per-track summaries
bb <- bb %>%
  mutate(song_id = tolower(paste0(trimws(title), " — ", trimws(artist))))

# Identifying which audio/meta features are present in  file
possible_feats <- c(
  "danceability","energy","valence","acousticness","instrumentalness",
  "liveness","speechiness","tempo","loudness","duration_ms","popularity",
  "key","mode","time_signature"
)
feat_cols <- intersect(possible_feats, names(bb))

## 3) Light outlier capping for numeric features 
cap_iqr <- function(x) {
  if (!is.numeric(x)) return(x)
  qs <- stats::quantile(x, c(.25,.75), na.rm = TRUE)
  iqr <- qs[2] - qs[1]
  lo <- qs[1] - 3*iqr; hi <- qs[2] + 3*iqr
  pmin(pmax(x, lo), hi)
}
if (length(feat_cols) > 0) {
  bb <- bb %>% mutate(across(all_of(feat_cols), cap_iqr))
}

##  tables  that can be used in the in the report

# 4a) Table 1: Summary stats of available audio features
tbl_features_summary <- if (length(feat_cols) > 0) {
  bb %>%
    summarise(across(all_of(feat_cols),
                     list(n = ~sum(!is.na(.)),
                          mean = ~mean(., na.rm=TRUE),
                          sd = ~sd(., na.rm=TRUE),
                          p25 = ~quantile(., .25, na.rm=TRUE),
                          median = ~median(., na.rm=TRUE),
                          p75 = ~quantile(., .75, na.rm=TRUE))))
} else {
  tibble(note = "No Spotify feature columns found.")
}

# 4b) Yearly number of unique songs (new entries per year in  year-end list)
tbl_yearly_new <- bb %>%
  group_by(year) %>%
  summarise(unique_songs = n_distinct(song_id), .groups = "drop")

# 4c) Top recurring artists  across 2000–202)
tbl_top_artists <- bb %>%
  count(artist, sort = TRUE) %>%
  slice_head(n = 20)

# Saving tables
readr::write_csv(tbl_features_summary, "table_features_summary.csv")
readr::write_csv(tbl_yearly_new, "table_yearly_unique_songs.csv")
readr::write_csv(tbl_top_artists, "table_top20_artists.csv")

## ---- 5) Figures (saved as PNGs) ----
dir.create("figs", showWarnings = FALSE)

# Fig 1: Yearly unique songs (trend)
ggplot(tbl_yearly_new, aes(year, unique_songs)) +
  geom_line() + geom_point() +
  labs(title = "Unique Songs in Billboard Year-End List (2000–2023)",
       x = "Year", y = "Count") +
  theme_minimal()
ggsave("figs/fig1_yearly_unique_songs.png", width = 8, height = 5, dpi = 300)

# Fig 2: Feature trends 
show_feats <- intersect(c("danceability","energy","valence"), feat_cols)
if (length(show_feats) > 0) {
  yr_feats <- bb %>%
    group_by(year) %>%
    summarise(across(all_of(show_feats), ~mean(., na.rm=TRUE)), .groups = "drop") %>%
    tidyr::pivot_longer(all_of(show_feats), names_to = "feature", values_to = "value")
  
  ggplot(yr_feats, aes(year, value, colour = feature)) +
    geom_line() +
    labs(title = "Yearly Mean Audio Features (2000–2023)",
         x = "Year", y = "Mean value", colour = "Feature") +
    theme_minimal()
  ggsave("figs/fig2_feature_trends.png", width = 8, height = 5, dpi = 300)
}

# Fig 3: Correlation heatmap 
num_feats <- bb %>% select(all_of(feat_cols)) %>% select(where(is.numeric))
if (ncol(num_feats) >= 4) {
  corr_m <- cor(num_feats, use = "pairwise.complete.obs")
  corr_df <- as.data.frame(corr_m) %>%
    tibble::rownames_to_column("x") %>%
    tidyr::pivot_longer(-x, names_to = "y", values_to = "r")
  
  ggplot(corr_df, aes(x, y, fill = r)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1,1)) +
    coord_equal() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix of Audio Features", x = NULL, y = NULL, fill = "r")
  ggsave("figs/fig3_correlation_heatmap.png", width = 7, height = 6, dpi = 300)
}

## ---- 6) Simple statistical analysis 
# Your dataset has a *year-end* ranking (1..100). Define a success score:
bb <- bb %>% mutate(success_score = 101 - rank)  # higher = better (rank 1 -> 100)

# Linear model: success_score ~ audio features 
lm_feats <- setdiff(intersect(feat_cols, names(bb)), c("duration_ms","popularity"))
if (length(lm_feats) >= 2) {
  form <- as.formula(paste("success_score ~", paste(lm_feats, collapse = " + ")))
  fit <- lm(form, data = bb %>% drop_na(all_of(c("success_score", lm_feats))))
  mod_glance <- broom::glance(fit)
  mod_tidy   <- broom::tidy(fit) %>% arrange(p.value)
  
  readr::write_csv(mod_glance, "table_model_glance.csv")
  readr::write_csv(mod_tidy,   "table_model_coefficients.csv")
}

## ---- 7) Exporting a clean analysis-ready CSV ----
readr::write_csv(bb, "billboard_clean_2000_2023.csv")

## ---- 8) Tiny console checks ----
cat("\nRows:", nrow(bb),
    "\nYears:", min(bb$year, na.rm=TRUE), "to", max(bb$year, na.rm=TRUE),
    "\n#Features used:", length(feat_cols), "\n")


# =======================
# PATCHES for clean plots
# =======================
library(tidyverse)

# -- Fig 2 (feature trends) : drop NA-year rows cleanly
show_feats <- intersect(c("danceability","energy","valence"), feat_cols)

if (length(show_feats) > 0) {
  yr_feats <- bb %>%
    group_by(year) %>%
    summarise(across(all_of(show_feats), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    drop_na() %>%                                  # ⬅️ remove year rows with all-NA means
    pivot_longer(all_of(show_feats), names_to = "feature", values_to = "value")
  
  ggplot(yr_feats, aes(year, value, colour = feature)) +
    geom_line(na.rm = TRUE) +                       # ⬅️ ignore any stray NAs
    labs(title = "Yearly Mean Audio Features (2000–2023)",
         x = "Year", y = "Mean value", colour = "Feature") +
    theme_minimal()
  ggsave("figs/fig2_feature_trends.png", width = 8, height = 5, dpi = 300)
}

# -- Fig 3 (correlation): removing zero-variance features first
num_feats <- bb %>% select(all_of(feat_cols)) %>% select(where(is.numeric))

if (ncol(num_feats) > 0) {
  sds <- sapply(num_feats, function(x) sd(x, na.rm = TRUE))
  keep <- names(sds[sds > 0 & !is.na(sds)])         # ⬅️ drop constant/NA-only features
  num_feats2 <- num_feats %>% select(all_of(keep))
}

if (ncol(num_feats2) >= 4) {
  corr_m <- cor(num_feats2, use = "pairwise.complete.obs")
  corr_df <- as.data.frame(corr_m) %>%
    tibble::rownames_to_column("x") %>%
    pivot_longer(-x, names_to = "y", values_to = "r")
  
  ggplot(corr_df, aes(x, y, fill = r)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1,1)) +
    coord_equal() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix of Audio Features", x = NULL, y = NULL, fill = "r")
  ggsave("figs/fig3_correlation_heatmap.png", width = 7, height = 6, dpi = 300)
}

# -- Fig 4 : Top 20 artists by appearances
tbl_top_artists <- bb %>%
  count(artist, sort = TRUE) %>%
  slice_head(n = 20) %>%
  arrange(n)

ggplot(tbl_top_artists, aes(n, reorder(artist, n))) +
  geom_col() +
  labs(title = "Top 20 Artists by Appearances (2000–2023)",
       x = "Count of entries", y = NULL) +
  theme_minimal()
ggsave("figs/fig4_top20_artists.png", width = 8, height = 6, dpi = 300)

# Quick file check
print(list.files("figs", full.names = TRUE))

getwd()


library(tidyverse)

bb <- bb %>%
  mutate(
    success_score = 101 - rank,
    top10 = as.integer(rank <= 10)
  )


set.seed(123)
#selecting features only those present in datases
features <- intersect(
  c("danceability","energy","valence","speechiness","acousticness",
    "instrumentalness","liveness","tempo","loudness","key","mode","time_signature"),
  feat_cols
)
#building modelling dataset
dat0 <- bb %>%
  mutate(success_score = 101 - rank,
         top10 = as.integer(rank <= 10)) %>%
  select(success_score, top10, all_of(features))

keep_cols <- names(dat0)[colSums(!is.na(dat0)) > 0]
dat0 <- dat0[, keep_cols, drop = FALSE]

idx_top  <- which(dat0$top10 == 1)
idx_rest <- which(dat0$top10 == 0)
pick <- function(ix, p=0.8) sample(ix, size = floor(p*length(ix)))
train_idx <- c(pick(idx_top), pick(idx_rest))
test_idx  <- setdiff(seq_len(nrow(dat0)), train_idx)

train <- dat0[train_idx, , drop = FALSE]
test  <- dat0[test_idx,  , drop = FALSE]
# median imputation for missing feature values
med <- sapply(train[, -c(1,2)], function(x) median(x, na.rm = TRUE))
for (nm in names(med)) {
  train[[nm]][is.na(train[[nm]])] <- med[[nm]]
  test[[nm]][is.na(test[[nm]])]  <- med[[nm]]
}

mu  <- sapply(train[, -c(1,2)], mean)
sdv <- sapply(train[, -c(1,2)], sd)
sdv[sdv == 0 | is.na(sdv)] <- 1
for (nm in names(mu)) {
  train[[nm]] <- (train[[nm]] - mu[[nm]]) / sdv[[nm]]
  test[[nm]]  <- (test[[nm]]  - mu[[nm]]) / sdv[[nm]]
}

cat("# train rows:", nrow(train), "  # test rows:", nrow(test),
    "  # features:", length(features), "\n")
# regression model evaluation
form_reg <- as.formula(paste("success_score ~", paste(names(mu), collapse=" + ")))
fit_reg  <- lm(form_reg, data=train)
pred_reg <- predict(fit_reg, newdata=test)
rmse <- sqrt(mean((test$success_score - pred_reg)^2))
r2   <- cor(test$success_score, pred_reg)^2

form_clf <- as.formula(paste("top10 ~", paste(names(mu), collapse=" + ")))
fit_clf  <- glm(form_clf, data=train, family=binomial())
prob <- plogis(predict(fit_clf, newdata=test))
pred <- as.integer(prob >= 0.5)
accuracy <- mean(pred == test$top10)
precision<- sum(pred==1 & test$top10==1) / max(sum(pred==1),1)
recall   <- sum(pred==1 & test$top10==1) / max(sum(test$top10==1),1)
# ---- Save logistic regression performance table ----

baseline <- max(prop.table(table(test$top10)))

tbl_logistic_performance <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Baseline accuracy"),
  Value  = round(c(accuracy, precision, recall, baseline), 3)
)

readr::write_csv(
  tbl_logistic_performance,
  "table_logistic_performance.csv"
)


cat(sprintf("\nRMSE=%.2f  R2=%.3f  Accuracy=%.3f  Precision=%.3f  Recall=%.3f\n",
            rmse, r2, accuracy, precision, recall))

set.seed(123)


# ---------- B1: Robust PCA + k-means (fixed) ----------
library(tidyverse)

# 1) pick numeric features that actually exist
features <- intersect(
  c("danceability","energy","valence","speechiness","acousticness",
    "instrumentalness","liveness","tempo","loudness","key","mode","time_signature"),
  feat_cols
)

# build numeric matrix, drop NAs
x_raw <- bb %>%
  dplyr::select(dplyr::all_of(features)) %>%
  dplyr::select(where(is.numeric))

# remove columns that are all NA or constant
nz_cols <- names(x_raw)[colSums(!is.na(x_raw)) > 0]
x_raw <- x_raw[, nz_cols, drop = FALSE]
if (ncol(x_raw) < 2L) stop("Not enough usable numeric features for PCA.")

# drop rows with any NA (PCA needs complete cases)
x_cc <- x_raw[stats::complete.cases(x_raw), , drop = FALSE]
if (nrow(x_cc) < 10L) stop("Too few complete rows after NA removal for PCA.")

# remove zero-variance columns (causes singularities)
sds <- apply(x_cc, 2, sd)
keep <- names(sds)[is.finite(sds) & sds > 0]
x_cc <- x_cc[, keep, drop = FALSE]
if (ncol(x_cc) < 2L) stop("After removing zero-variance columns, <2 features remain.")

# 2) PCA (center & scale are TRUE because raw Spotify features are on different scales)
pca <- prcomp(x_cc, center = TRUE, scale. = TRUE)

# 3) Choose K via elbow on kmeans over scaled data
set.seed(123)
ks <- 2:8
wss <- sapply(ks, function(k) kmeans(scale(x_cc), centers = k, nstart = 20)$tot.withinss)
elbow <- tibble(k = ks, tot_withinss = wss)
readr::write_csv(elbow, "table_kmeans_elbow.csv")

# 4) Fit kmeans with K=4 (change if elbow suggests otherwise)
set.seed(123)
km4 <- kmeans(scale(x_cc), centers = 4, nstart = 25)
centroids <- as_tibble(km4$centers, .name_repair = "unique") %>%
  mutate(cluster = row_number())
readr::write_csv(centroids, "table_cluster_centroids_scaled.csv")

# 5) Figures
dir.create("figs", showWarnings = FALSE)

# Fig5: elbow
ggplot(elbow, aes(k, tot_withinss)) +
  geom_line() + geom_point() +
  labs(title = "K-means Elbow Curve", x = "K (clusters)", y = "Total within-cluster SS") +
  theme_minimal()
ggsave("figs/fig5_elbow.png", width = 7, height = 4, dpi = 300)

# Fig6: PCA scatter coloured by cluster (first 2 PCs)
pca_df <- as_tibble(pca$x[, 1:2]) %>%
  mutate(cluster = factor(km4$cluster))
ggplot(pca_df, aes(PC1, PC2, colour = cluster)) +
  geom_point(alpha = 0.4) +
  labs(title = "PCA (PC1 vs PC2) coloured by K-means clusters") +
  theme_minimal()
ggsave("figs/fig6_pca_clusters.png", width = 7, height = 5, dpi = 300)

# helpful console diagnostics
cat("# rows used in PCA:", nrow(x_cc),
    " | # features used:", ncol(x_cc), "\n",
    "First 5 features kept:", paste(head(colnames(x_cc), 5), collapse=", "), "\n")

library(tidyverse)

# features actually used in PCA/kmeans
features_used <- colnames(x_cc)          # from the PCA block you just ran

# rows in bb that are complete for those features
row_ok <- stats::complete.cases(bb[, features_used, drop = FALSE])

# add cluster labels (NA where features were incomplete)
bb_cl <- bb
bb_cl$cluster <- NA_integer_
bb_cl$cluster[row_ok] <- km4$cluster     # uses km4 from your k-means fit


# Top-10 success by cluster
tbl_cluster_top10 <- bb_cl %>%
  mutate(top10 = rank <= 10) %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            top10_rate = mean(top10),
            .groups = "drop")

# Yearly share of clusters
tbl_cluster_year <- bb_cl %>%
  filter(!is.na(cluster)) %>%
  count(year, cluster) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Cluster means of audio features (on original scale, easier to interpret)
features <- intersect(
  c("danceability","energy","valence","speechiness","acousticness",
    "instrumentalness","liveness","tempo","loudness","key","mode","time_signature"),
  names(bb_cl)
)

tbl_cluster_means <- bb_cl %>%
  filter(!is.na(cluster)) %>%
  summarise(across(all_of(features), ~ mean(.x, na.rm = TRUE)), .by = cluster)

# Save
readr::write_csv(tbl_cluster_top10,  "table_cluster_top10_rate.csv")
readr::write_csv(tbl_cluster_year,   "table_cluster_year_share.csv")
readr::write_csv(tbl_cluster_means,  "table_cluster_feature_means.csv")


# check
exists("bb_cl"); exists("km4")

library(dplyr)

# Rebuild table safely in case it's missing
tbl_cluster_top10 <- bb_cl %>%
  mutate(top10 = rank <= 10) %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            top10_rate = mean(top10),
            .groups = "drop")

ggplot(tbl_cluster_top10, aes(x = factor(cluster), y = top10_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Top-10 share by cluster",
       x = "Cluster", y = "Top-10 (%)") +
  theme_minimal()

ggsave("figs/fig7_cluster_top10_rate.png", width = 6, height = 4, dpi = 300)

# Rebuild table safely in case it's missing
tbl_cluster_year <- bb_cl %>%
  filter(!is.na(cluster)) %>%
  count(year, cluster) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(tbl_cluster_year, aes(x = year, y = pct, fill = factor(cluster))) +
  geom_area(alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Yearly share of clusters",
       x = "Year", y = "Share of year-end list", fill = "Cluster") +
  theme_minimal()

ggsave("figs/fig8_cluster_share_over_time.png", width = 8, height = 5, dpi = 300)

features <- intersect(
  c("danceability","energy","valence","speechiness","acousticness",
    "instrumentalness","liveness","tempo","loudness","key","mode","time_signature"),
  names(bb_cl)
)

tbl_cluster_means <- bb_cl %>%
  filter(!is.na(cluster)) %>%
  summarise(across(all_of(features), ~ mean(.x, na.rm = TRUE)), .by = cluster)

tbl_heat <- tbl_cluster_means %>%
  pivot_longer(-cluster, names_to = "feature", values_to = "mean_value")

ggplot(tbl_heat, aes(feature, factor(cluster), fill = mean_value)) +
  geom_tile() +
  coord_flip() +
  labs(title = "Cluster profiles (feature means)",
       x = "Feature", y = "Cluster", fill = "Mean") +
  theme_minimal()

ggsave("figs/fig9_cluster_feature_heatmap.png", width = 7, height = 5, dpi = 300)

list.files("figs", pattern = "fig7|fig8|fig9", full.names = TRUE)
browseURL("figs/fig7_cluster_top10_rate.png")


