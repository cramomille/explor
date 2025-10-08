
library(sf)
library(mapsf)
library(asf)


ix1 <- asf_mar("iris_xxxx", "iris_f")  # O
ix2 <- asf_mar("iris_xxxx", "iris_r2") # O
ix3 <- asf_mar("iris_xxxx", "iris_r5") # O
ix4 <- asf_mar("iris_xxxx", "com_f")   # O
ix5 <- asf_mar("iris_xxxx", "com_r2")  # O
ix6 <- asf_mar("iris_xxxx", "com_r5")  # O

ii1 <- asf_mar("iris_2023", "iris_r2") # O
ii2 <- asf_mar("iris_2023", "iris_r5") # O
ii3 <- asf_mar("iris_2023", "com_f")   # O
ii4 <- asf_mar("iris_2023", "com_r2")  # O
ii5 <- asf_mar("iris_2023", "com_r5")  # O

cx1 <- asf_mar("com_xxxx", "com_f")    # O
cx2 <- asf_mar("com_xxxx", "com_r2")   # O
cx3 <- asf_mar("com_xxxx", "com_r5")   # O

cc1 <- asf_mar("com_2023", "com_r2")   # O
cc2 <- asf_mar("com_2023", "com_r5")   # O

fond <- asf_mar(geom = TRUE)


# Conversions IRIS_xxxx
ix1 <- asf_fond(fond, ix1, by = "IRISF_CODE", maille = "IRISF_CODE")    # 49 310
ix2 <- asf_fond(fond, ix2, by = "IRISF_CODE", maille = "IRISrD_CODE")   # 21 243
ix3 <- asf_fond(fond, ix3, by = "IRISF_CODE", maille = "IRISr5_CODE")   #  9 363 V
ix4 <- asf_fond(fond, ix4, by = "COMF_CODE", maille = "COMF_CODE")      # 34 721 V
ix5 <- asf_fond(fond, ix5, by = "COMF_CODE", maille = "COMR2_CODE")     # 10 547 V
ix6 <- asf_fond(fond, ix6, by = "COMF_CODE", maille = "COMR5_CODE")     #  5 683 V

# Conversions IRIS_2023
ii1 <- asf_fond(fond, ii1, by = "IRISF_CODE", maille = "IRISrS_CODE")   # 21 375 (132 de +)
ii2 <- asf_fond(fond, ii2, by = "IRISF_CODE", maille = "IRISr5_CODE")   #  9 363 V
ii3 <- asf_fond(fond, ii3, by = "COMF_CODE", maille = "COMF_CODE")      # 34 721 V
ii4 <- asf_fond(fond, ii4, by = "COMF_CODE", maille = "COMR2_CODE")     # 10 547 V
ii5 <- asf_fond(fond, ii5, by = "COMF_CODE", maille = "COMR5_CODE")     #  5 683 V

# Conversions COM_xxxx
cx1 <- asf_fond(fond, cx1, by = "COMF_CODE", maille = "COMF_CODE")      # 34 721 V
cx2 <- asf_fond(fond, cx2, by = "COMF_CODE", maille = "COMR2_CODE")     # 10 547 V
cx3 <- asf_fond(fond, cx3, by = "COMF_CODE", maille = "COMR5_CODE")     #  5 683 V

# Conversions COM_2023
cc1 <- asf_fond(fond, cc1, by = "COMF_CODE", maille = "COMR2_CODE")     # 10 547 V
cc2 <- asf_fond(fond, cc2, by = "COMF_CODE", maille = "COMR5_CODE")     #  5 683 V





