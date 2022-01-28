library(tidyverse)
theme_set(theme_light())

trial <-
    read_csv("ZnSe COVIDvax clinical trial.csv") %>%
    janitor::clean_names() %>%
    mutate(across(.cols = c("zn_se", "symptoms", "vax_type", "vax_number"),
                  .fns = ~ factor(.x)
                  )
            ) %>%
    filter(across(.cols = everything(), .fns = ~ !is.na(.x)))

trial %>%
    distinct(patient) %>%
    count()

trial %>%
    ggplot +
    aes(x = zn_se, y = symptoms, color = vax_number) + 
    geom_jitter(width = .2, height = .2) + 
    labs(x = "Zn/Se treatment",
         y = "Symptoms",
         color = "Vaccine\nshot order")

ggsave("results/initial-overview.png", width = 6, height = 6)


Xsqr <-
    janitor::tabyl(trial, zn_se, symptoms) %>%
    janitor::chisq.test() %>%
    broom::tidy()

N <- nrow(trial)

effectsize <- sqrt(Xsqr$statistic / N)
pwr::pwr.chisq.test(w = effectsize, N = N, df = Xsqr$parameter) %>% broom::tidy()

?pwr::pwr.chisq.test
