
# código de  procesamiento -----------------------------------------------------------



# paquetes a utilizar  ----------------------------------------------------

pacman::p_load(tidyverse, magrittr) 


# datos a utilizar ----------------------------------------------------------------


movid <- haven::read_dta(url("https://movid-impact.netlify.app/input/data/MOVID-IMPACT.dta"))

movid_proc <- movid %>%
  group_by(id_encuesta) %>%  #Para calcular tamano de hogar
  mutate(tamanohogar = n()) %>%
  ungroup() %>%
  filter(entrevistado == 1) %>% #Filtrar solo a entrevistados
  select(sexo, edad, trabaja = g1,ingreso = g47_monto, tingreso= g48, tamanohogar, starts_with("c2"), fatiga = f5_3) %>% #Seleccionar variabes
  mutate_at(vars(-tingreso), ~car::recode(.,"c(8,9) = NA")) %>%
  mutate(tingreso = car::recode(tingreso,"c(98,99) = NA")) %>%
  mutate_at(vars(sexo, trabaja, starts_with("c2"), tingreso, -fatiga), ~ as_factor(.))


saveRDS(movid_proc, file = "output/movid_proc.RDS")
