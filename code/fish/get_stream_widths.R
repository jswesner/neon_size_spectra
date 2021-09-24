library(neonUtilities)

#stream sites
siteID=c("HOPB", "LEWI", "POSE", "CUPE",
         "GUIL", "KING", "MCDI", "LECO",
         "WALK", "MAYF", "ARIK", "BLUE",
         "PRIN", "BLDE", "COMO", "WLOU", 
         "SYCA", "REDB", "MART", "MCRA",
         "BIGC", "TECR", "OKSR", "CARI")

# Pull reaeration data from NEON API into R environment. Keep only the table with stream widths
reaDP<-neonUtilities::loadByProduct(dpID="DP1.20190.001", siteID, 
                                    nCores = 4,
                                    tabl = "rea_widthFieldData",
                                    check.size = F, token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJqZWZmd2VzbmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTc4NjU1MjkxMiwiaWF0IjoxNjI4ODcyOTEyLCJlbWFpbCI6ImplZmZ3ZXNuZXJAZ21haWwuY29tIn0.VnIZyX8yUCBfQyLOtS2hxr_tB4JW2CBzD46QezlxnIKCc1biYv9BbVZvl72obmKP1uXu4iK_c2pzDmBFW_S9oA")

