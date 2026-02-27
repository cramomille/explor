
#                           EXPLORATION DES RESULTATS DE NOS PARTIES DE FARAWAY
#
#                                                                antoine beroud
#                                                                     aout 2025

invisible(sapply(list.files("script/faraway/function/", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))


# EXPLORATIONS ----------------------------------------------------------------
rank_games(dir = "script/faraway/games_results/")
rank_games(dir = "script/faraway/games_results/", n = 10)

rank_games(dir = "script/faraway/games_results/", n = 10, j = "rose")
rank_games(dir = "script/faraway/games_results/", n = 10, j = "antoine")

analyze_cards(dir = "script/faraway/games_results/")
analyze_cards(dir = "script/faraway/games_results/", n = 10)

analyze_cards(dir = "script/faraway/games_results/", j = "rose")
analyze_cards(dir = "script/faraway/games_results/", n = 10, j = "rose")

analyze_cards(dir = "script/faraway/games_results/", j = "antoine") 
analyze_cards(dir = "script/faraway/games_results/", n = 10, j = "antoine") 



# ENREGISTREMENT DES PARTIES --------------------------------------------------
# save_game(
# list(
#   c("antoine", 0, 0, 9, 0, 2, 0, 24, 19, 7),
#   c("rose", 6, 6, 8, 10, 4, 12, 10, 12, 7),
#   c("franck", 1, 4, 4, 6, 8, 8, 8, 18, 7)
# )
# )
# save_game(
# list(
#   c("gabriel", 0, 0, 6, 7, 6, 2, 20, 14, 11), 
#   c("alain", 0, 16, 4, 0, 12, 3, 4, 5, 16),
#   c("franck", 0, 9, 4, 0, 12, 12, 15, 15, 5)
# )
# )
# save_game(
# list(
#   c("rose", 2, 0, 9, 6, 3, 0, 4, 14, 7), 
#   c("antoine", 4, 10, 8, 0, 18, 12, 10, 6, 14)
# )
# )
# save_game(
# list(
#   c("baltazar", 6, 0, 0, 15, 12, 9, 8, 12, 14),
#   c("gabriel", 4, 2, 10, 4, 2, 4, 14, 16, 5),
#   c("franck", 0, 0, 0, 10, 8, 2, 8, 0, 19),
#   c("rose", 8, 0, 0, 0, 0, 0, 12, 20, 11)
# )
# )
# save_game(
# list(
#   c("rose", 0, 0, 2, 0, 10, 15, 8, 13, 9),
#   c("franck", 0, 0, 4, 6, 12, 0, 12, 19, 3),
#   c("valentine", 0, 0, 4, 0, 9, 0, 16, 9, 9)
# )
# )
# save_game(
# list(
#   c("franck", 4, 0, 2, 9, 0, 6, 14, 0, 5),
#   c("rose", 0, 4, 4, 6, 12, 12, 10, 15, 8),
#   c("valentine", 0, 7, 0, 9, 6, 6, 18, 8, 7)
# )
# )
# save_game(
# list(
#   c("rose", 6, 8, 3, 10, 0, 18, 0, 16, 4),
#   c("franck", 10, 6, 6, 2, 9, 0, 3, 8, 24),
#   c("valentine", 0, 2, 0, 9, 9, 13, 6, 19, 7)
# )
# )
# save_game(
# list(
#   c("valentine", 0, 0, 2, 0, 2, 12, 9, 9, 17),
#   c("rose", 0, 0, 7, 9, 4, 8, 0, 6, 16),
#   c("franck", 0, 3, 0, 13, 15, 12, 9, 4, 9)
# )
# )
# save_game(
# list(
#   c("baltazar", 19, 6, 0, 16, 12, 12, 9, 7, 21), 
#   c("gabriel", 6, 2, 4, 10, 6, 6, 18, 9, 14),
#   c("franck", 2, 10, 9, 7, 0, 10, 13, 10, 11),
#   c("rose", 1, 0, 20, 14, 21, 8, 6, 4, 18)
# )
# )
# save_game(
# list(
#   c("valentine", 0, 18, 0, 0, 6, 9, 4, 0, 18), 
#   c("rose", 3, 0, 0, 0, 5, 2, 6, 20, 0)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 4, 8, 5, 15, 0, 4, 8, 4),
#   c("valentine", 0, 3, 0, 10, 1, 6, 9, 21, 11),
#   c("franck", 0, 10, 2, 0, 13, 9, 0, 16, 13),
#   c("rose", 0, 3, 6, 12, 9, 12, 18, 12, 6)
# )
# )
# save_game(
# list(
#   c("antoine", 12, 0, 4, 6, 20, 8, 0, 18, 12), 
#   c("rose", 0, 5, 0, 8, 6, 10, 6, 0, 12)
# )
# )
# save_game(
# list(
#   c("rose", 8, 0, 0, 6, 12, 4, 10, 18, 0),
#   c("eve", 3, 2, 16, 10, 3, 0, 10, 14, 18),
#   c("antoine", 13, 9, 12, 6, 0, 12, 7, 17, 9),
#   c("valentine", 4, 2, 0, 0, 20, 12, 4, 19, 3)
# )
# )
# save_game(
# list(
#   c("valentine", 8, 6, 2, 6, 9, 8, 8, 10, 17),
#   c("rose", 10, 10, 9, 9, 7, 12, 0, 16, 5)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 3, 0, 0, 0, 0, 3, 12, 15),
#   c("rose", 0, 0, 12, 10, 8, 3, 9, 9, 7)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 9, 6, 9, 9, 3, 8, 5, 12), 
#   c("rose", 0, 12, 8, 9, 10, 0, 0, 20, 10)
# )
# )
# save_game(
# list(
#   c("rose", 6, 0, 0, 6, 9, 9, 8, 8, 12),
#   c("antoine", 0, 20, 7, 0, 9, 16, 0, 8, 10)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 5, 3, 0, 0, 18, 16, 12, 12),
#   c("valentine", 0, 0, 8, 10, 10, 0, 0, 20, 3), 
#   c("eve", 0, 3, 4, 6, 0, 19, 0, 12, 13), 
#   c("rose", 0, 2, 6, 6, 0, 10, 17, 14, 7)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 12, 0, 0, 0, 4, 0, 20, 6),
#   c("rose", 0, 6, 7, 12, 9, 8, 10, 15, 5)
# )
# )
# save_game(
# list(
#   c("antoine", 2, 15, 0, 9, 7, 12, 8, 15, 7),
#   c("rose", 0, 18, 0, 12, 0, 12, 9, 15, 14)
# )
# )
# save_game(
# list(
#   c("antoine", 9, 0, 3, 8, 15, 0, 3, 9, 10),
#   c("rose", 0, 18, 4, 10, 8, 9, 8, 10, 5)
# )
# )
# save_game(
# list(
#   c("antoine", 2, 0, 0, 12, 0, 0, 5, 10, 25),
#   c("rose", 6, 0, 0, 14, 0, 9, 4, 10, 13)
# )
# )
# save_game(
# list(
#   c("mateo", 8, 0, 9, 14, 4, 16, 10, 3, 11),
#   c("rose", 0, 7, 8, 6, 9, 12, 8, 20, 7),
#   c("antoine", 20, 0, 0, 15, 8, 4, 3, 2, 16)
# )
# )
# save_game(
# list(
#   c("rose", 9, 13, 16, 9, 6, 2, 12, 12, 3),
#   c("franck", 0, 3, 10, 0, 0, 9, 20, 10, 18),
#   c("antoine", 3, 7, 4, 8, 0, 24, 14, 12, 11)
# )
# )
# save_game(
# list(
#   c("franck", 3, 0, 0, 0, 0, 8, 20, 8, 8), 
#   c("mateo", 0, 2, 0, 2, 0, 12, 14, 24, 4),
#   c("rose", 18, 6, 4, 19, 15, 6, 8, 4, 21),
#   c("antoine", 0, 0, 9, 6, 7, 6, 17, 16, 19)
# )
# )
# save_game(
# list(
#   c("rose", 0, 0, 6, 19, 15, 20, 20, 8, 8),
#   c("antoine", 0, 0, 18, 0, 0, 20, 9, 20, 25)
# )
# )
# save_game(
# list(
#   c("rose", 0, 8, 4, 15, 16, 0, 7, 18, 17),
#   c("franck", 4, 0, 6, 5, 4, 19, 6, 20, 6),
#   c("antoine", 0, 0, 9, 8, 10, 12, 10, 0, 13)
# )
# )
# save_game(
# list(
#   c("franck", 2, 0, 0, 0, 10, 8, 18, 10, 12),
#   c("antoine", 3, 0, 0, 20, 15, 12, 0, 5, 11),
#   c("rose", 2, 19, 3, 0, 9, 0, 10, 24, 3)
# )
# )
# save_game(
# list(
#   c("rose", 15, 0, 13, 0, 6, 9, 7, 6, 19),
#   c("antoine", 12, 10, 0, 0, 17, 10, 4, 10, 15)
# )
# )
# save_game(
# list(
#   c("franck", 7, 4, 3, 8, 12, 10, 18, 15, 12),
#   c("rose", 2, 0, 0, 6, 4, 0, 0, 6, 15),
#   c("antoine", 0, 0, 10, 0, 12, 8, 0, 24, 0)
# )
# )
# save_game(
# list(
#   c("franck", 0, 8, 0, 10, 0, 16, 12, 6, 8),
#   c("antoine", 0, 0, 10, 0, 8, 12, 0, 6, 19),
#   c("rose", 4, 4, 6, 4, 6, 15, 16, 9, 7)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 0, 0, 0, 10, 0, 18, 20, 15),
#   c("rose", 15, 4, 16, 0, 4, 18, 12, 6, 9)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 0, 0, 15, 0, 12, 6, 8, 15),
#   c("rose", 4, 7, 12, 6, 19, 15, 9, 12, 0)
# )
# )
# save_game(
# list(
#   c("antoine", 0, 2, 10, 10, 12, 10, 24, 10, 7),
#   c("rose", 0, 0, 15, 7, 10, 0, 20, 15, 13)
# )
# )
# save_game(
# list(
#   c("franck", 0, 0, 4, 0, 17, 21, 15, 21, 6),
#   c("rose", 2, 0, 7, 16, 4, 12, 10, 8, 8), 
#   c("antoine", 6, 9, 6, 6, 18, 14, 3, 16, 22)
# )
# )
# save_game(
# list(
#   c("samuel", 2, 0, 0, 6, 6, 4, 0, 7, 19),
#   c("stephane", 0, 0, 2, 10, 2, 8, 4, 16, 11),
#   c("christelle", 18, 9, 0, 20, 24, 10, 5, 0, 6),
#   c("oceane", 0, 0, 6, 6, 0, 12, 6, 20, 1),
#   c("antoine", 0, 0, 0, 0, 2, 19, 12, 6, 15)
# )
# )
# save_game(
# list(
#   c("antoine", 9, 3, 5, 6, 0, 14, 8, 2, 20),
#   c("rose", 18, 0, 6, 2, 6, 0, 0, 8, 6)
# )
# )