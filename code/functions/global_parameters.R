global_parameters <- function() {
  ## ---------------------------------------------
  ## Global parameters stored as YAML
  assign(x = "param",
         value = yaml::read_yaml("./data/data-config/global_parameters.yml"),
         envir = globalenv())
}