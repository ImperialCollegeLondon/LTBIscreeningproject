
#' create_and_save_policies
#'
#' Given the input argument creates a sort of grid array version of an environment
#' which behaves like a list object.
#'
#' @param incidence_list WHO incidence in country of origin group to target screening
#' @param endpoints when to stop using costs and QALYs at time of exit or death
#' @param LTBI_test TSPOT or QFT
#' @param treatment 6 months or 3 months
#'
#' @return list of policies as inputs for the model.
#' @export
#'
#' @examples
#'
create_and_save_policies <- function(incidence_list,
                                     endpoints,
                                     LTBI_test,
                                     treatment) {
  policies <- data.frame()
  policies_ls <- vector(mode = "character")
  num_policy <- 1

  for (min_screen_length_of_stay in 0) {
    for (incidence in seq_along(incidence_list)) {
      for (endpoint_QALY in seq_along(endpoints)) {
        for (endpoint_cost in endpoint_QALY:length(endpoints)) {
          for (test in LTBI_test) {
            for (treat in treatment) {

              policies <- rbind(policies,
                                cbind(policy = formatC(num_policy, width = 3, format = "d", flag = "0"),
                                      min_screen_length_of_stay,
                                      incid_grps = as.character(incidence_list[incidence]),
                                      treatment = treat,
                                      LTBI_test = test,
                                      endpoint_cost = endpoints[endpoint_cost],
                                      endpoint_QALY = endpoints[endpoint_QALY]))

              environ_name <- sprintf("policy_%s",
                                      formatC(num_policy, width = 3, format = "d", flag = "0"))

              assign(x = environ_name, value = new.env())
              assign(x = "min_screen_length_of_stay", value = min_screen_length_of_stay, envir = eval(parse(text = environ_name)))
              assign(x = "incidence_grps_screen", value = incidence_list[[incidence]], envir = eval(parse(text = environ_name)))
              assign(x = "ENDPOINT_cost", value = endpoints[endpoint_cost], envir = eval(parse(text = environ_name)))
              assign(x = "ENDPOINT_QALY", value = endpoints[endpoint_QALY], envir = eval(parse(text = environ_name)))
              assign(x = "treatment", value = treat, envir = eval(parse(text = environ_name)))
              assign(x = "LTBI_test", value = test, envir = eval(parse(text = environ_name)))

              policies_ls <- c(policies_ls, environ_name)
              num_policy <- num_policy + 1
            }
          }
        }
      }
    }
  }

  global_params <-
    policies %>%
    split(seq(nrow(.))) %>%
    lapply(as.list)


  # save --------------------------------------------------------------------

  save(global_params, file = here::here("data/global_params.RData"))
  save(policies_ls, file = here::here("data/policies_ls.RData"))
  save(list = policies_ls, file = here::here("data/policies.RData"))
  write.csv(policies, file = here::here("data/policies-inputs.csv"))
}

