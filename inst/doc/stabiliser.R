## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("roberthyde/stabiliser")

## ----warning=FALSE------------------------------------------------------------
library(stabiliser)
data("stabiliser_example")

## ----message=FALSE, warning=FALSE, error=FALSE--------------------------------
set.seed(8141)
stable_enet <- stabilise(data = stabiliser_example,
                         outcome = "y")

## -----------------------------------------------------------------------------
stable_enet$enet$stability

## -----------------------------------------------------------------------------
stable_enet$enet$perm_thresh

## ----message=FALSE, warning=FALSE, error=FALSE--------------------------------
set.seed(8141)
stable_combi <- stabilise(data = stabiliser_example,
                         outcome = "y",
                         models = c("enet",
                                    "mbic",
                                    "mcp"))

## -----------------------------------------------------------------------------
triangulated <- triangulate(stable_combi)
triangulated

## ----eval=FALSE---------------------------------------------------------------
#  stab_plot(stabiliser_object = triangulated)

## ----stab_plot, echo=FALSE, message=FALSE, error=FALSE, warning=FALSE---------
library(ggplot2)
library(dplyr)

triangulated$combi$stability %>%
    filter(!is.na(bootstrap_p)) %>%
    mutate(causal = case_when(grepl("causal", variable) ~ "Causal",
                              TRUE ~ "Junk")) %>%
    ggplot(aes(x = stability, y = bootstrap_p, colour = causal)) +
    geom_jitter(height = 0.05, width = 1) +
    geom_vline(xintercept = triangulated$combi$perm_thresh) +
    labs(
      x = "Stability (%)",
      y = "Bootstrap-p",
      colour = "Variable"
    ) +
    scale_y_reverse()+
    theme_minimal()

