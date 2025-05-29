# Required libraries
library(tidyverse)
library(scales)
library(ggthemes)

# 1) Parameters
d_target  <- 0.20       # the Cohen's d we care about
m_outcomes<- 3          # number of tests/outcomes
alpha     <- 0.05
power     <- 0.95

# 2) Bonferroni‐corrected alpha
alpha_corr <- alpha / m_outcomes

# 3) z‐scores
z_a2_corr <- qnorm(1 - alpha_corr/2)
z_b       <- qnorm(power)

# 4) Closed‐form solution for total N
#    N = 4 * ((z_{α'/2} + z_β) / d )^2
N_req <- ceiling( 4 * ( (z_a2_corr + z_b) / d_target )^2 )

cat(sprintf("For d = %.2f, α′ = %.4f (Bonferroni), 1–β = %.2f → N ≥ %d total\n",
            d_target, alpha_corr, power, N_req))

# 5) Optional: MDE curve with corrected alpha
mde_calc_corr <- function(N) (z_a2_corr + z_b) * sqrt(4 / N)

Ns    <- seq(200, 5000, by = 50)
mde_df<- tibble(N = Ns, MDE = mde_calc_corr(Ns))

ggplot(mde_df, aes(x = N, y = MDE)) +
  geom_line(size = 1.2, color = "#2C3E50") +
  geom_hline(yintercept = d_target,
             linetype = "dashed", color = "#E74C3C", size = 1) +
  geom_vline(xintercept = N_req,
             linetype = "dotted", color = "#3498DB", size = 1) +
  scale_x_continuous("Total Sample Size (N)", labels = comma) +
  scale_y_continuous("Minimum Detectable Cohen’s d",
                     labels = comma_format(accuracy = 0.01)) +
  labs(
    title    = "",
    subtitle = sprintf("Target d = %.2f; α′ = %.4f; 95%% power", d_target, alpha_corr),
    caption  = sprintf("MDE Curve with Bonferroni‐Corrected α\nRequired N = %d (dotted red); target d (dashed red)", N_req)
  ) +
  theme_few(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 11, margin = margin(b = 6)),
    plot.caption    = element_text(size = 8, hjust = 0),
    axis.title      = element_text(face = "bold"),
    axis.text       = element_text(color = "grey20"),
    panel.grid.minor= element_blank()
  )
