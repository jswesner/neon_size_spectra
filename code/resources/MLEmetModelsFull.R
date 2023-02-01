rm(list = ls())
source("./code/resources/01_load-packages.R")
theme_set(theme_minimal())
#ARIK
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/ARIK_metModel.R",
  name = "ARIK metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#BIGC
rstudioapi::jobRunScript(
  path = here::here("code/resources/metModelsMLE/BIGC_metModel.R"),
  name = "BIGC metMM",
  workingDir = here::here(),
  importEnv = FALSE,
  exportEnv = FALSE
)
# BLDE
rstudioapi::jobRunScript(
  path = here::here("code/resources/metModelsMLE/BLDE_metModel.R"),
  name = "BLDE metMM",
  workingDir = here::here(),
  importEnv = FALSE,
  exportEnv = FALSE
)


# BLUE
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/BLUE_metModel.R",
  name = "BLUE metMM",
  workingDir = here::here(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#CARI
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/CARI_metModel.R",
  name = "CARI metMM",
  workingDir = here::here(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#COMO
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/COMO_metModel.R",
  name = "COMO metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
# CUPE
rstudioapi::jobRunScript(
  path = here::here("code/resources/metModelsMLE/CUPE_metModel.R"),
  name = "CUPE metMM",
  workingDir = here::here(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#GUIL
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/GUIL_metModel.R",
  name = "GUIL metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/HOPB_metModel.R",
  name = "HOPB metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/KING_metModel.R",
  name = "KING metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/LECO_metModel.R",
  name = "LECO metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
# LEWI
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/LEWI_metModel.R",
  name = "LEWI metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#MART
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/MART_metModel.R",
  name = "MART metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#MAYF
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/MAYF_metModel.R",
  name = "MAYF metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
# MCDI
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/MCDI_metModel.R",
  name = "MCDI metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#MCRA
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/MCRA_metModel.R",
  name = "MCRA metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#OKSR
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/OKSR_metModel.R",
  name = "OKSR metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#POSE
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/POSE_metModel.R",
  name = "POSE metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#PRIN
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/PRIN_metModel.R",
  name = "PRIN metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#REDB
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/REDB_metModel.R",
  name = "REDB metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
# SYCA
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/SYCA_metModel.R",
  name = "SYCA metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#TECR
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/TECR_metModel.R",
  name = "TECR metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#WALK
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/WALK_metModel.R",
  name = "WALK metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
#WLOU
rstudioapi::jobRunScript(
  path = "./code/resources/metModelsMLE/WLOU_metModel.R",
  name = "WLOU metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)