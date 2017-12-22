# ******************************************************
# LTBI screening
# N Green
# Sept 2017
#
# combine decision tree and competing risk model output


if (!exists("aTB_CE_stats")) load(choose.files()) #load(pastef(diroutput, "aTB_CE_stats.RData"))
if (!exists("dectree_res")) load(choose.files()) #dectree_res <- readRDS(paste0("Q:/R/cluster--LTBI-decision-tree/", cluster_output_filename))

# if (!exists("scenario_parameter_p")) scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_fullfactorial_QFT-plus.xlsx", sheet = "p")
if (!exists("scenario_parameter_p")) scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_fullfactorial_QFT-GIT.xlsx", sheet = "p")
# if (!exists("scenario_parameter_p")) scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_fullfactorial_TSPOT.xlsx", sheet = "p")

popscale <- 1#00000


# create BCEA dataframe ---------------------------------------------------

###############
## active TB ##
###############

scenario.names <-
  c(0, seq_len(length(dectree_res))) %>%
  as.character(.)

# BCEA format
# with status-quo

aTB_cost.df <-
  do.call(cbind,
          aTB_CE_stats$cost_incur_person) %>%
  data.frame(0, ., row.names = NULL) %>%
  set_names(nm = scenario.names)

aTB_QALYgain.df <-
  do.call(cbind,
          aTB_CE_stats$QALYgain_person) %>%
  data.frame(0, ., row.names = NULL) %>%
  set_names(nm = scenario.names)


####################
## LTBI screening ##
####################

LTBI_cost_melt <- do.call(cbind.data.frame,
                          purrr::map(dectree_res, "mc_cost"))

LTBI_QALYloss_melt <- do.call(cbind.data.frame,
                              purrr::map(dectree_res, "mc_health"))

## BCEA format
LTBI_cost.df <- data.frame('0' = 0, LTBI_cost_melt, check.names = FALSE)
LTBI_QALYgain.df <- data.frame('0' = 0, -LTBI_QALYloss_melt, check.names = FALSE)

# screen_discount <- 0.9

# discount due to delay to screening
LTBI_cost.df <- LTBI_cost.df * screen_discount
LTBI_QALYgain.df <- LTBI_QALYgain.df * screen_discount


c.total <- as.matrix(LTBI_cost.df + aTB_cost.df) * popscale
e.total <- as.matrix(LTBI_QALYgain.df + aTB_QALYgain.df) * popscale


# create design matrix ----------------------------------------------------

# convert to percentages
scenario_numbers <- scenario_parameter_p$scenario

design_matrix <-
  (scenario_parameter_p*100) %>%
  dplyr::mutate(scenario = scenario_numbers)

# convert to discrete levels
# design_matrix <- apply(scenario_parameter_p, 2, as.factor)

# discounting due to delay to screening
dectree_res_mc_health <- lapply(purrr::map(dectree_res, "mc_health"), `*`, screen_discount)
dectree_res_mc_cost <- lapply(purrr::map(dectree_res, "mc_cost"), `*`, screen_discount)

# combine decision tree and pop model output
e_screened <- purrr::map2(aTB_CE_stats$QALY.screened_person,
                          dectree_res_mc_health, `-`)

c_screened <- purrr::map2(aTB_CE_stats$cost.screened_person,
                          dectree_res_mc_cost, `+`)

e_statusquo <- aTB_CE_stats$QALY.statusquo_person
c_statusquo <- aTB_CE_stats$cost.statusquo_person

wtp_seq <- seq(10000, 30000, by = 500)

# net monetary benefit by wtp
nmb_long <-
  lapply(wtp_seq,
         FUN = function(wtp) nmb(e_statusquo, c_statusquo,
                                 e_screened, c_screened,
                                 wtp)) %>%
  do.call(what = rbind, args = .)

# simplify names
names(design_matrix) <- gsub(pattern =  " Treatment", replacement = "", names(design_matrix))
names(design_matrix) <- gsub(pattern =  " to Screen", replacement = "", names(design_matrix))

sim_matrix <- merge(x = design_matrix,
                    y = nmb_long,
                    by = "scenario")
# set baseline level
sim_matrix <- within(sim_matrix,
                     policy <- factor(policy, levels = c("statusquo", "screened")))

