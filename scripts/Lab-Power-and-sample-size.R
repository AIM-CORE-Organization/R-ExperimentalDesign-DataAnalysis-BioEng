######################################################################
# Modified from Design and Analysis of Experiments -- Douglas Montgomery
# dae.tex
# (c) Christophe Lalanne, 2009
######################################################################


######################################################################
## Power and Sample size (One-way ANOVA)
######################################################################

# Group means (example)
grp.means <- c(575, 600, 650, 675)

# Single power calculation (sanity check)
power.anova.test(
  groups     = 4,
  between.var = var(grp.means),
  within.var  = 25^2,
  sig.level   = 0.01,
  power       = 0.90
)

# Sequences to explore
sigma_seq <- seq(20, 80, by = 2)   # within-group SD values
nn        <- seq(4, 20, by = 2)    # per-group sample sizes

# Matrix of powers: rows = sigma, cols = n
pow_mat <- matrix(NA_real_, nrow = length(sigma_seq), ncol = length(nn))

# Safer computation in case vectorization changes across versions
for (i in seq_along(sigma_seq)) {
  this_sd <- sigma_seq[i]
  pow_vec <- sapply(nn, function(n_per_grp) {
    power.anova.test(
      groups      = 4,
      n           = n_per_grp,
      between.var = var(grp.means),
      within.var  = this_sd^2,
      sig.level   = 0.01
    )$power
  })
  pow_mat[i, ] <- pow_vec
}

colnames(pow_mat) <- nn
rownames(pow_mat) <- sigma_seq

# Plot
opar <- par(las = 1, cex = 0.8)
matplot(
  x = sigma_seq, y = pow_mat, type = "l",
  xlab = expression(sigma),
  ylab = expression(1 - beta),   # 'power' = 1 - beta
  col = 1, lty = 1
)
grid()

# Label each curve at the right edge with its n value
text(
  x = rep(max(sigma_seq), length(nn)),
  y = pow_mat[nrow(pow_mat), ],
  labels = as.character(nn),
  pos = 3
)

title("Operating Characteristic Curves\nfor a = 4 treatment means")
par(opar)

