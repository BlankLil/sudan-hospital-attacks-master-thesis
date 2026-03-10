#robustness and statistical checks 

#multicollinearity ----
library(performance)


#collinearity check for ZINB base model ----
vif_res <- check_collinearity(
  glm(
    hospital_attacks ~ conflict_intensity +
      territorial_events_lag1 + n_territorial_events +
      log_population,
    data = panel_master_lag,
    family = poisson()
  )
)

vif_df <- as.data.frame(vif_res)

names(vif_df)
vif_df
vif_df <- vif_df %>% rename(Parameter = Term)
vif_df
#plot for multi collinearity 
multicollinearity_plot <- ggplot(vif_df, aes(x = reorder(Parameter, VIF), y = VIF)) +
  geom_point(size = 3, color = "#1F4E79") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 10, linetype = "dotted", color = "grey50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Variance Inflation Factor (VIF)",
    title = "Assessment of multicollinearity",
    subtitle = "All VIF values are well below conventional thresholds"
  ) +
  theme_minimal(base_family = "Times New Roman")

print(multicollinearity_plot)

ggsave(
  filename = "multicollinearity_plot.png",
  plot = multicollinearity_plot,    
  width = 18,
  height = 12,
  units = "cm", dpi = 300
)

#collinearity check for ZINB RSF model  -----
vif_RSF <- check_collinearity(
  glm(
    hospital_attacks_RSF ~
      conflict_intensity +
      territorial_losses_RSF +
      territorial_losses_RSF_lag1 +
      log_population,
    data = panel_master_lag_actors,
    family = poisson()
  )
)

vif_RSF_df <- as.data.frame(vif_RSF) %>%
  rename(Parameter = Term)

ggplot(vif_RSF_df, aes(x = reorder(Parameter, VIF), y = VIF)) +
  geom_point(size = 3, color = "#8B0000") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 10, linetype = "dotted", color = "grey50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Variance Inflation Factor (VIF)",
    title = "Assessment of multicollinearity (RSF model)",
    subtitle = "No evidence of problematic multicollinearity"
  ) +
  theme_minimal(base_family = "Times New Roman")

#collinearity check for ZINB SAF model  -----
vif_SAF <- check_collinearity(
  glm(
    hospital_attacks_SAF ~
      conflict_intensity +
      territorial_losses_SAF +
      log_population,
    data = panel_master_lag_actors,
    family = poisson()
  )
)

vif_SAF_df <- as.data.frame(vif_SAF) %>%
  rename(Parameter = Term)

ggplot(vif_SAF_df, aes(x = reorder(Parameter, VIF), y = VIF)) +
  geom_point(size = 3, color = "#1F4E79") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 10, linetype = "dotted", color = "grey50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Variance Inflation Factor (VIF)",
    title = "Assessment of multicollinearity (SAF model)",
    subtitle = "All VIF values are below conventional thresholds"
  ) +
  theme_minimal(base_family = "Times New Roman")



#predict comparable models ----
# Poisson ----
pois_RSF <- glm(
  hospital_attacks_RSF ~
    conflict_intensity +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population,
  data = panel_master_lag_actors,
  family = poisson()
)

summary(pois_RSF)
#deviance check, not good measure: deviance-based dispersion breaks down with extreme sparsity
deviance(pois_RSF) / df.residual(pois_RSF)
#so pearson-based dispersion
sum(residuals(pois_RSF, type = "pearson")^2) / df.residual(pois_RSF)
#3.016

pois_SAF <- glm(
  hospital_attacks_SAF ~
    conflict_intensity +
    territorial_losses_SAF +
    log_population,
  data = panel_master_lag_actors,
  family = poisson()
)

summary(pois_RSF)


#check overdisperion
performance::check_overdispersion(pois_RSF)
performance::check_overdispersion(pois_SAF)

# Negative Binomial----
library(MASS)
nb_RSF <- glm.nb(
  hospital_attacks_RSF ~
    conflict_intensity +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population,
  data = panel_master_lag_actors
)

summary(nb_RSF)

nb_SAF <- glm.nb(
  hospital_attacks_SAF ~
    conflict_intensity +
    territorial_losses_SAF +
    log_population,
  data = panel_master_lag_actors
)

summary(nb_SAF)

#compare AIC (possion, negative binomial, ZINB)
AIC(pois_RSF, nb_RSF, zinb_RSF_t)
AIC(pois_SAF, nb_SAF, zinb_SAF_t)

#test overdispersion ----
library(lmtest)
lrtest(pois_RSF, nb_RSF)
lrtest(pois_SAF, nb_SAF)
#vuong test 
vuong(nb_RSF, zinb_RSF_t)
vuong(nb_SAF, zinb_SAF_t)
#check overdispersion ----
zinb_RSF_t$theta
zinb_SAF_t$theta
# - overdispersion is a thing - ZINB is valid!

#check residuals ----
plot_residuals_zinb_RSF <- plot(residuals(zinb_RSF_t, type = "pearson"))
ggsave(
  filename = "plot_residuals_zinb_RSF.pdf",
  plot = plot_residuals_zinb_RSF,    
  width = 18,
  height = 12,
  units = "cm"
)

print(plot_residuals_zinb_RSF)

plot_resdiuals_zinb_SAF <- plot(residuals(zinb_SAF_t, type = "pearson"))
ggsave(
  filename = "plot_residuals_zinb_SAF.pdf",
  plot = plot_residuals_zinb_SAF,    
  width = 18,
  height = 12,
  units = "cm"
)

print(plot_resdiuals_zinb_SAF)

#sparsity checks ----
#for territorial losses SAF lagged 
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(territorial_losses_SAF_lag1 != 0, na.rm = TRUE),
    share_nonzero = mean(territorial_losses_SAF_lag1 != 0, na.rm = TRUE)
  )
#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(territorial_losses_SAF_lag1 > 0,
           hospital_attacks_SAF > 0))

#territorial losses SAF 
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(territorial_losses_SAF != 0, na.rm = TRUE),
    share_nonzero = mean(territorial_losses_SAF != 0, na.rm = TRUE)
  )

#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(territorial_losses_SAF > 0,
           hospital_attacks_SAF > 0))

#territorial losses RSF lagged
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(territorial_losses_RSF_lag1 != 0, na.rm = TRUE),
    share_nonzero = mean(territorial_losses_RSF_lag1 != 0, na.rm = TRUE)
  )
#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(territorial_losses_RSF_lag1 > 0,
           hospital_attacks_RSF > 0))

#territorial losses RSF 
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(territorial_losses_RSF != 0, na.rm = TRUE),
    share_nonzero = mean(territorial_losses_RSF != 0, na.rm = TRUE)
  )
#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(territorial_losses_RSF > 0,
           hospital_attacks_RSF > 0))

#conflict Intensity 
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(conflict_intensity != 0, na.rm = TRUE),
    share_nonzero = mean(conflict_intensity != 0, na.rm = TRUE)
  )
#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(conflict_intensity > 0,
           hospital_attacks_RSF > 0))

#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(conflict_intensity > 0,
           hospital_attacks_SAF > 0))


#conflict intensity lagged
panel_master_lag_actors %>%
  summarise(
    n = n(),
    n_nonzero = sum(conflict_intensity_lag1 != 0, na.rm = TRUE),
    share_nonzero = mean(conflict_intensity_lag1 != 0, na.rm = TRUE)
  )
#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(conflict_intensity_lag1 > 0,
           hospital_attacks_RSF > 0))

#variable ever switch on when attacks occur?
with(panel_master_lag_actors,
     table(conflict_intensity_lag1 > 0,
           hospital_attacks_SAF > 0))


#separation check ----
with(panel_master_lag_actors,
     table(territorial_losses_SAF_lag1 > 0,
           hospital_attacks_SAF == 0))

#alternative check to sparsity check----
panel_master_lag_actors %>%
  filter(hospital_attacks_SAF > 0) %>%
  summarise(
    n_attacks = n(),
    share_loss_now = mean(territorial_losses_SAF != 0),
    share_loss_lag = mean(territorial_losses_SAF_lag1 != 0)
  )
#contingency table
with(panel_master_lag_actors,
     table(
       loss = territorial_losses_SAF_lag1 > 0,
       attack = hospital_attacks_SAF > 0
     ))

panel_master_lag_actors %>%
  filter(hospital_attacks_RSF > 0) %>%
  summarise(
    n_attacks = n(),
    share_loss_now = mean(territorial_losses_RSF != 0),
    share_loss_lag = mean(territorial_losses_RSF_lag1 != 0)
  )




