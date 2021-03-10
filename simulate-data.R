library(tidyverse)
library(RxODE)
library(here)

Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

# convert pk parameters --------------------------------------------------------
convert_2cmp_pk_rate = function(CL, Vc, Q, Vp){
  data.frame(
    k = CL/Vc,
    k12 = Q/Vc,
    k21 = Q/Vp
  )
}

# ADA 2 compartment model ------------------------------------------------------

ada_model <- RxODE({
  kel = (k + kt*max(0, t - time0))
  d/dt(centr) = -(k12 + kel)*centr + k21 * peri
  d/dt(peri) = k12*centr - k21*peri
})

# pk parameters ----------------------------------------------------------------

pk_parameters <- tribble(
  ~id, ~dose, ~CL, ~Vc, ~Q, ~Vp, ~time0, ~kt,
  1, 170, 0.10, 0.37, 0.10, 0.22, 12.37, 0.59,
  2, 160, 0.10, 0.37, 0.10, 0.22, 12.37, 0.59,
  3, 150, 0.36, 0.46, 1.57, 0.98, 12.97, 0.88,
  4, 170, 0.36, 0.46, 1.57, 0.98, 12.97, 0.88,
  5, 160, 0.14, 0.67, 0.06, 0.05, 12.40, 3.61,
  6, 170, 0.14, 0.67, 0.06, 0.05, 12.40, 3.61
) %>% 
  mutate(
    converted_params = pmap(list(CL, Vc, Q, Vp), convert_2cmp_pk_rate)
  ) %>% 
  unnest(cols = converted_params)

# generate data ----------------------------------------------------------------

pred_time <- c(seq(0, 7, by = 0.5), seq(8, 28, by = 2))

predict_concentrations <- function(ID) {
  
  df <- filter(pk_parameters, id == ID)
  
  dose <- df[["dose"]]
  
  # ada model parameters
  theta <- df %>% select(Vc, k, k12, k21, time0, kt)
  
  init <- c(centr = dose/theta[["Vc"]], peri = 0)
  
  ev <- eventTable(time.units = "days")
  
  ev$add.sampling(pred_time)
  
  res = as.data.frame(ada_model$solve(theta, event = ev, inits = init))
  
  tibble(
    id = ID,
    dose = dose,
    time = pred_time,
    conc_2cmptADA = res$centr
  )
}

fake_data <- map_dfr(1:6, predict_concentrations) 

fake_data %>% 
  ggplot(aes(x = time, y = conc_2cmptADA)) +
  geom_line() +
  facet_wrap(vars(id)) +
  scale_y_log10()

monolix_data <- fake_data %>% 
  mutate(
    AMT = if_else(time == 0, as.character(dose), "."),
    DV = if_else(time == 0, ".", as.character(conc_2cmptADA))
  ) %>% 
  select(
    ID = id,
    TIME = time,
    AMT,
    DV 
  )

write_csv(monolix_data, here("data", "monolix-fake-data.csv"))
