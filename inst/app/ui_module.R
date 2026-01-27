# Home module ----
home_mod <- function(id){
  ns <- NS(id)
  homepage_ui()
  }

# LCA module ----
ctt_mod <- function(id, project){
  ns <- NS(id)
  ctt_ui(project)
}

# LPA module ----
contentval_mod <- function(id, project){
  ns <- NS(id)
  contentval_ui(project)
}
# LPT module ----
lta_mod <- function(id, project){
  ns <- NS(id)
  lta_ui(project)
}
# EFA module ----
efa_mod <- function(id, project){
  ns <- NS(id)
  efa_ui(project)
}
# CFA module ----
cfa_mod <- function(id, project){
  ns <- NS(id)
  cfa_ui(project)
}
