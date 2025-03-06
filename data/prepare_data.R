# header ------------------------------------------------------------------

# Script which uses the jmpwashdata R data package and transforms the 
# jmp_wld_sanitation data set from wide to long format. The script also
# adds the variable names and labels from the codebook to the data set.

# load packages -----------------------------------------------------------

library(tidyverse)
library(jmpwashdata)

# import data -------------------------------------------------------------

codebook_long <- read_csv("https://raw.githubusercontent.com/WASHNote/jmpwashdata/main/data-raw/codebook/jmp_codebook_estimate_long.csv")
codebook_wide <- read_csv("https://raw.githubusercontent.com/WASHNote/jmpwashdata/main/data-raw/codebook/jmp_codebook_estimate_wide.csv")

# transform data ----------------------------------------------------------

## define levels for varname_short

levels_varname_short <- c("san_sm", "san_bas", "san_lim", "san_unimp", "san_od")  
levels_varname_long <- c("safely managed sanitation services", 
                         "basic sanitation services", 
                         "limited sanitation services", 
                         "unimproved sanitation facilities", 
                         "no sanitation facilities")
## transform data

jmp_wld_sanitation_long <- jmp_wld_sanitation |> 
  select(name:year,
         region_sdg,
         starts_with("san_bas"),
         starts_with("san_lim"),
         starts_with("san_unimp"),
         starts_with("san_od"),
         starts_with("san_sm")) |> 
  pivot_longer(cols = san_bas_n:san_sm_u,
               names_to = "varname_short",
               values_to = "percent") |> 
  mutate(residence = case_when(
    str_detect(varname_short, "_n") ~ "national",
    str_detect(varname_short, "_r") ~ "rural",
    str_detect(varname_short, "_u") ~ "urban",
  )) |> 
  mutate(varname_short = str_remove(varname_short, "(?<!n)_u$|_n|_r")) |> 
  left_join(codebook_long, join_by("varname_short")) |> 
  mutate(varname_long = str_remove(object, " \\(.*\\)")) |> 
  relocate(varname_long, residence, .after = varname_short) |> 
  select(name:percent) |> 
  mutate(varname_long = factor(varname_long, levels = levels_varname_long),
         varname_short = factor(varname_short, levels = levels_varname_short),
         residence = factor(residence, levels = c("national", "rural", "urban"))
         )

# export data -------------------------------------------------------------

write_rds(jmp_wld_sanitation_long, "data/jmp_wld_sanitation_long.rds")
write_csv(jmp_wld_sanitation_long, "data/jmp_wld_sanitation_long.csv")

# explore -----------------------------------------------------------------

# In codebook long, but not in wide
setdiff(codebook_long$varname_short, codebook_wide$varname_short)

codebook_long |> 
  anti_join(codebook_wide, join_by("varname_short"))

# In codebook wide, but not in long
setdiff(codebook_wide$varname_short, codebook_long$varname_short)

codebook_wide |> 
  anti_join(codebook_long, join_by("varname_short"))


