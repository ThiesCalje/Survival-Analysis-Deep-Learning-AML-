library(survival, lib.loc = "C:/Program Files/R/R-4.1.1/library")
library(survminer)
library(dplyr)
library(knitr)
library(data.table)
library(cmprsk)
library(sm)
library(condSURV)
library(remotes)
library(condSURV)
Surv(lung$time, lung$status)
f1 <- survfit(Surv(time, status) ~ 1, data = lung)
names(f1)
plot(survfit(Surv(time, status) ~ 1, data = lung), 
     xlab = "Days", 
     ylab = "Overall survival probability")
ggsurvplot(
  fit = survfit(Surv(time, status) ~ 1, data = lung), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  censor = FALSE)
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)
survfit(Surv(time, status) ~ 1, data = lung)
lung %>% 
  filter(status == 2) %>% 
  summarize(median_surv = median(time))
survdiff(Surv(time, status) ~ sex, data = lung)
sd <- survdiff(Surv(time, status) ~ sex, data = lung)
1 - pchisq(sd$chisq, length(sd$n) - 1)

coxph(Surv(time, status) ~ sex, data = lung)
broom::tidy(
  coxph(Surv(time, status) ~ sex, data = lung), 
  exp = TRUE
) %>% 
  kable()
coxph(Surv(time, status) ~ sex, data = lung) %>% 
  gtsummary::tbl_regression(exp = TRUE) 
data(BMT, package = "SemiCompRisks")
lm_dat <- 
  BMT %>% 
  filter(T1 >= 90) 
lm_dat <- 
  lm_dat %>% 
  mutate(
    lm_T1 = T1 - 90
  )

lm_fit <- survfit(Surv(lm_T1, delta1) ~ deltaA, data = lm_dat)
coxph(
  Surv(T1, delta1) ~ deltaA, 
  subset = T1 >= 90, 
  data = BMT
) %>% 
gtsummary::tbl_regression(exp = TRUE)
bmt <- setDT(BMT, keep.rownames = TRUE)[]
td_dat <- 
  tmerge(
    data1 = bmt %>% select(rn, T1, delta1), 
    data2 = bmt %>% select(rn, T1, delta1, TA, deltaA), 
    id = rn, 
    death = event(T1, delta1),
    agvhd = tdc(TA)
  )
coxph(
  Surv(time = tstart, time2 = tstop, event = death) ~ agvhd, 
  data = td_dat
) %>% 
  gtsummary::tbl_regression(exp = TRUE)
data(Melanoma, package = "MASS")
cuminc(Melanoma$time, Melanoma$status, cencode = 2)
ci_fit <- 
  cuminc(
    ftime = Melanoma$time, 
    fstatus = Melanoma$status, 
    cencode = 2
  )
plot(ci_fit)
ggcompetingrisks(ci_fit)
ci_ulcer <- 
  cuminc(
    ftime = Melanoma$time, 
    fstatus = Melanoma$status, 
    group = Melanoma$ulcer,
    cencode = 2
  )

ci_ulcer[["Tests"]]
ggcompetingrisks(
  fit = ci_ulcer, 
  multiple_panels = FALSE,
  xlab = "Days",
  ylab = "Cumulative incidence of event",
  title = "Death by ulceration",
  ylim = c(0, 1)
)
shr_fit <- 
  crr(
    ftime = Melanoma$time,
    fstatus = Melanoma$status,
    cov1 = Melanoma[, c("sex", "age")],
    cencode = 2
  )

shr_fit
chardat <- 
  Melanoma %>% 
  mutate(
    sex_char = ifelse(sex == 0, "Male", "Female")
  )
covs1 <- model.matrix(~ sex_char + age, data = chardat)[, -1]
crr(
  ftime = chardat$time,
  fstatus = chardat$status,
  cov1 = covs1,
  cencode = 2
)
chr_fit <- 
  coxph(
    Surv(time, ifelse(status == 1, 1, 0)) ~ sex + age, 
    data = Melanoma
  )

broom::tidy(chr_fit, exp = TRUE) %>% 
  kable()
gtsummary::tbl_regression(chr_fit, exp = TRUE)
mv_fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
cz <- cox.zph(mv_fit)
print(cz)
plot(cz)
sm.options(
  list(
    xlab = "Age (years)",
    ylab = "Time to death (years)")
)

sm.survival(
  x = lung$age,
  y = lung$time,
  status = lung$status,
  h = sd(lung$age) / nrow(lung)^(-1/4)
)
sm.survival(
  x = lung$age,
  y = lung$time,
  status = lung$status,
  h = (1/4) * sd(lung$age) / nrow(lung)^(-1/4)
)
fit1 <- survfit(Surv(time, status) ~ 1, data = lung)

prob_times <- seq(365.25, 182.625 * 5, 182.625)
remotes::install_github("zabore/condsurv")
purrr::map_df(
  prob_times, 
  ~conditional_surv_est(
    basekm = fit1, 
    t1 = 182.625, 
    t2 = .x) 
) %>% 
  mutate(months = round(prob_times / 30.4)) %>% 
  select(months, everything()) %>% 
  kable()
