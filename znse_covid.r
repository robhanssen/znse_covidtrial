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