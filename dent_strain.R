library(tidyverse)
library(here)


dentseg <- readxl::read_xlsx(here("line_16in.xlsx"), skip = 5) %>%
  janitor::clean_names() %>%
  filter(str_detect(event, "dent")) %>% 
  mutate(R0 = od_in/2,
         R1 = -(4*(od_reduction_percent/100 * od_in) ^ 2 + (width_in*0.80)^2) / (8*od_reduction_percent/100*od_in),
         R2 = (4*(od_reduction_percent/100 * od_in) ^ 2 + (length_in*0.80)^2) / (8*od_reduction_percent/100*od_in),
         eps1 = wall_thickness_in / 2 * (1 / R0 - 1 / R1), 
         eps2 = -wall_thickness_in / (2 * R2), 
         eps3 = 1 / 2 * (od_reduction_percent/100 * od_in / (length_in*0.80)) ^ 2, 
         eps_i = 2 / sqrt(3) * sqrt(eps1^2 + eps1 * (eps2+eps3) + (eps2 + eps3) ^ 2),
         eps_o = 2 / sqrt(3) * sqrt(eps1^2 - eps1 * (-eps2+eps3) + (-eps2 + eps3) ^ 2)
         )

write_csv(dentseg, "dent_strain.csv")
