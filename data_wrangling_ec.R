precinct_pop <- read_csv("precinct_pop.csv")

precincts <- read_sf("nypp.shx") %>%
  mutate(Precinct = as.double(Precinct))

sqf17 <-read_csv("sqf-2017.csv") %>%
  mutate(year = 2017) %>%
  filter(STOP_LOCATION_PRECINCT != "#NULL!") %>%
  mutate(STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT)) 

sqf18 <-read_csv("sqf-2018.csv") %>%
  mutate(year = 2018) %>%
  filter(STOP_LOCATION_PRECINCT != "#NULL!") %>%
  mutate(STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT))

sqf19 <-read_csv("sqf-2019.csv") %>%
  mutate(year = 2019) %>%
  filter(STOP_LOCATION_PRECINCT != "#NULL!") %>%
  mutate(STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT))

precinct_pop <- precinct_pop %>%
  mutate(black_pc = black/total_pop,
         hispanic_pc = hisp_latino/total_pop,
         white_pc = white/total_pop,
         other_pc = other/total_pop)

sqf17_1 <- sqf17 %>%
  select(YEAR2, SUSPECT_RACE_DESCRIPTION, STOP_LOCATION_PRECINCT) %>%
  mutate(SUSPECT_RACE_DESCRIPTION = case_when(
    SUSPECT_RACE_DESCRIPTION %in% c("AMER IND", "ASIAN/PAC.ISL") ~ "OTHER",
    SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC" ~ "BLACK",
    TRUE ~ as.character(SUSPECT_RACE_DESCRIPTION)
  )) %>%
  filter(SUSPECT_RACE_DESCRIPTION != "(null)",
         SUSPECT_RACE_DESCRIPTION != "MALE")

sqf18_1 <- sqf18 %>%
  select(YEAR2, SUSPECT_RACE_DESCRIPTION, STOP_LOCATION_PRECINCT) %>%
  mutate(SUSPECT_RACE_DESCRIPTION = case_when(
    SUSPECT_RACE_DESCRIPTION %in% 
      c("AMERICAN INDIAN/ALASKAN NATIVE", "ASIAN / PACIFIC ISLANDER") ~ "OTHER",
    SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC" ~ "BLACK",
    TRUE ~ as.character(SUSPECT_RACE_DESCRIPTION)
  )) %>%
  filter(SUSPECT_RACE_DESCRIPTION != "(null)")

sqf19_1 <- sqf19 %>%
  select(YEAR2, SUSPECT_RACE_DESCRIPTION, STOP_LOCATION_PRECINCT) %>%
  mutate(SUSPECT_RACE_DESCRIPTION = case_when(
    SUSPECT_RACE_DESCRIPTION %in% 
      c("AMERICAN INDIAN/ALASKAN N", "ASIAN / PACIFIC ISLANDER") ~ "OTHER",
    SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC" ~ "BLACK",
    TRUE ~ as.character(SUSPECT_RACE_DESCRIPTION)
  )) %>%
  filter(SUSPECT_RACE_DESCRIPTION != "(null)")

master_1 <- rbind(sqf17_1, sqf18_1, sqf19_1) %>%
  filter(STOP_LOCATION_PRECINCT != "#NULL!") %>%
  mutate(STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT))

master_1_group <- master_1 %>%
  count(STOP_LOCATION_PRECINCT, SUSPECT_RACE_DESCRIPTION, YEAR2) %>%
  group_by(STOP_LOCATION_PRECINCT, YEAR2) %>%
  mutate(prop = n/sum(n)) %>%
  filter(SUSPECT_RACE_DESCRIPTION != "OTHER") %>%
  ungroup()

rows <- cbind(STOP_LOCATION_PRECINCT=c(32,43,47,75,101,113, 113,73), SUSPECT_RACE_DESCRIPTION=c(rep("WHITE", 6),"WHITE HISPANIC","WHITE"), YEAR2=c(rep(2017, 7),2018), n=rep(0, 8), prop=rep(0,8))

master_1_group <- rbind(master_1_group, rows) %>%
  mutate(across(c(1,3:5), as.double))
precinct_pop_1 <- precinct_pop %>%
  select(precinct_2020, black_pc, hispanic_pc, white_pc)

master_1_group <- master_1_group %>%
  left_join(y = precinct_pop_1, 
            by = c("STOP_LOCATION_PRECINCT" = "precinct_2020")) %>%
  mutate(diff_pc = case_when(
    SUSPECT_RACE_DESCRIPTION == "BLACK" ~ (black_pc-prop),
    SUSPECT_RACE_DESCRIPTION == "WHITE HISPANIC" ~ (hispanic_pc-prop),
    SUSPECT_RACE_DESCRIPTION == "WHITE" ~ (white_pc-prop)
  )) %>%
  filter(STOP_LOCATION_PRECINCT != 208760) %>%
  left_join(precincts, by = c("STOP_LOCATION_PRECINCT" = "Precinct"))