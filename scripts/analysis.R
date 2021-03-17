explanatory <- c("sex", "age", "thickness", "ulcer")

#Demographic table using finalfit package
table1 <- melanoma %>% 
  mutate(age = ff_label(age, "Age (years)")) %>% 
  mutate(sex = ff_label(sex, "Sex")) %>% 
  mutate(thickness = ff_label(thickness, "Tumor thickness (mm)")) %>% 
  mutate(ulcer = ff_label(ulcer, "Ulcerated tumor")) %>% 
  mutate(status = ff_label(status, "Status")) %>% 
  mutate(t_stage = ff_label(t_stage, "T-stage")) %>% 
  summary_factorlist("status", explanatory, add_dependent_label = TRUE, na_include = TRUE, p = TRUE)
table1
save(table1, file = here("data", "table1.rda"))

#Logistic regression (5 year mortality)
dependent <- "dead_melanoma_5yr"
explanatory <- c("ulcer", "age", "sex", "t_stage")
table_LR <- melanoma %>% 
  mutate(dead_melanoma_5yr = ff_label(dead_melanoma_5yr, "5-year survival")) %>% 
  mutate(age = ff_label(age, "Age (years)")) %>% 
  mutate(sex = ff_label(sex, "Sex")) %>% 
  mutate(t_stage = ff_label(t_stage, "T-stage")) %>% 
  mutate(ulcer = ff_label(ulcer, "Ulcerated tumor")) %>% 
  finalfit(dependent, explanatory)
table_LR
save(table_LR, file = here("data", "table_LR.rda"))

melanoma %>% 
  mutate(dead_melanoma_5yr = ff_label(dead_melanoma_5yr, "5-year survival")) %>% 
  mutate(age = ff_label(age, "Age (years)")) %>% 
  mutate(sex = ff_label(sex, "Sex")) %>% 
  mutate(t_stage = ff_label(t_stage, "T-stage")) %>% 
  mutate(ulcer = ff_label(ulcer, "Ulcerated tumor")) %>% 
  or_plot("dead_melanoma_5yr", explanatory, 
          breaks = c(0.5, 1, 5, 10, 20, 30),
          table_text_size = 3.5)

ggsave(here("figures", "odds_ratio.jpg")) #saving the figure

#Survival Analysis
survival_object <- melanoma %$% 
  Surv(time/365.25, status_os)

fit <- survfit(survival_object ~ 1, data = melanoma)
os_all <- ggsurvplot(fit, risk.table = TRUE)
os_all$plot
save(os_all, file = here("data", "os_all.rda"))
ggsave(here("figures", "os_all.jpg")) #saving the figure

fit_ulcer <- survfit(survival_object ~ ulcer, data = melanoma)
os_ulcer <- ggsurvplot(fit_ulcer, risk.table = TRUE, pval = TRUE, palette = c("#E7B800", "#2E9FDF"), risk.table.col = "strata", 
           risk.table.y.text.col = TRUE, conf.int = TRUE)
os_ulcer$plot
save(os_ulcer, file = here("data", "os_ulcer.rda"))
ggsave(here("figures", "os_ulcer.jpg")) #saving the figure

survival_object_dss <- melanoma %$% 
  Surv(time/365.25, status_dss)

fit_dss <- survfit(survival_object_dss ~ 1, data = melanoma)
dss_all <- ggsurvplot(fit_dss, risk.table = TRUE)
dss_all$plot
save(dss_all, file = here("data", "dss_all.rda"))
ggsave(here("figures", "dss_all.jpg")) #saving the figure

fit_ulcer_dss <- survfit(survival_object_dss ~ ulcer, data = melanoma)
dss_ulcer <- ggsurvplot(fit_ulcer_dss, risk.table = TRUE, pval = TRUE, palette = c("#E7B800", "#2E9FDF"), risk.table.col = "strata", 
           risk.table.y.text.col = TRUE, conf.int = TRUE)
dss_ulcer$plot
save(dss_ulcer, file = here("data", "dss_ulcer.rda"))
ggsave(here("figures", "dss_ulcer.jpg")) #saving the figure

#Cox proportional hazard
coxph(Surv(time, status_os) ~ age + sex + thickness + ulcer, data = melanoma) %>% 
  tidy(conf.int = TRUE, exp = TRUE)

dependent_os  <- "Surv(time, status_os)"
dependent_dss <- "Surv(time, status_dss)"
dependent_crr <- "Surv(time, status_crr)"
explanatory   <- c("age", "sex", "thickness", "ulcer")

table_cphm <- melanoma %>% 
  mutate(age = ff_label(age, "Age (years)")) %>% 
  mutate(sex = ff_label(sex, "Sex")) %>% 
  mutate(thickness = ff_label(thickness, "Thickness")) %>% 
  mutate(ulcer = ff_label(ulcer, "Ulcerated tumor")) %>% 
  finalfit(dependent_os, explanatory, add_dependent_label = FALSE) %>% 
  rename("Overall survival" = label) %>% 
  rename(" " = levels) %>% 
  rename("  " = all)
table_cphm

save(table_cphm, file = here("data", "table_cphm.rda"))

melanoma %>% 
  hr_plot(dependent_os, explanatory) #hazard ratio plot

ggsave(here("figures", "hazard_ratio.jpg")) #saving the figure

#Testing for proportional hazard
explanatory <- c("age", "sex", "thickness", "ulcer", "year")
melanoma 
  coxphmulti(dependent_os, explanatory) %>% 
  cox.zph() %>% 
  {zph_result <<- .} %>% 
  plot(var=5) #change 4 to 1, 2 or 3 to see the other variables

explanatory <- c("age", "sex", "ulcer", "thickness", 
                 "strata(year)")
melanoma %>% 
  finalfit(dependent_os, explanatory)

#Bootstrap confidence intervals using rsample package
coxph_intervals <- reg_intervals(Surv(time, status_os) ~ age + sex + thickness + ulcer, 
                                 data = melanoma, 
                                 type = "percentile", 
                                 keep_reps = TRUE, 
                                 model_fn = "coxph")

save(coxph_intervals, file = here("data", "coxph_intervals.rda"))

coxph_bootstrap_intervals <- coxph_intervals %>%
  mutate(term = fct_reorder(term, exp(.estimate))) %>%
  ggplot(aes(exp(.estimate), term)) +
  geom_vline(xintercept = 1, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = exp(.lower), xmax = exp(.upper)),
                 size = 1.5, alpha = 0.5, color = "midnightblue") +
  geom_point(size = 3, color = "midnightblue") +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_minimal()

save(coxph_bootstrap_intervals, file = here("data", "coxph_bootstrap_intervals.rda"))

coxph_bootstrap_distributions <- coxph_intervals %>%
  mutate(term = fct_reorder(term, exp(.estimate))) %>%
  unnest(.replicates) %>%
  ggplot(aes(exp(estimate), fill = term)) +
  geom_vline(xintercept = 1, size = 1.2, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE, binwidth = 0.05) +
  facet_wrap(vars(term)) + 
  labs(x = "Hazard ratio") + 
  theme_minimal()

save(coxph_bootstrap_distributions, file = here("data", "coxph_bootstrap_distributions.rda"))


#Competing risk regression
explanatory   <- c("age", "sex", "thickness", "ulcer")
dependent_dss <- "Surv(time, status_dss)"
dependent_cr <- "Surv(time, status_cr)"

table_competingrisk <- melanoma %>% 
  mutate(age = ff_label(age, "Age (years)")) %>% 
  mutate(sex = ff_label(sex, "Sex")) %>% 
  mutate(thickness = ff_label(thickness, "Thickness")) %>% 
  mutate(ulcer = ff_label(ulcer, "Ulcerated tumor")) %>% 
  # Summary table
  summary_factorlist(dependent_dss, explanatory, 
                     column = TRUE, fit_id = TRUE) %>%
  # CPH univariable
  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH univariable)")
  ) %>%
  # CPH multivariable
  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH multivariable)")
  ) %>%
  # Fine and Gray competing risks regression
  ff_merge(
    melanoma %>%
      crrmulti(dependent_cr, explanatory) %>%
      fit2df(estimate_suffix = " (competing risks multivariable)")
  ) %>%
  select(-fit_id, -index) %>%
  dependent_label(melanoma, "Survival")
table_competingrisk

save(table_competingrisk, file = here("data", "table_competingrisk.rda"))
