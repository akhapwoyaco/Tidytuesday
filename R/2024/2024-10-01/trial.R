#
chess <- read.csv(
  file = "D:/2024/October/October_5/TidyTuesday/R/2024/2024-10-01/chess.csv"
) |> distinct()
dim(chess)
View(chess)
#
chess |> 
  select(turns, victory_status) |> 
  group_by(victory_status) |>
  summarise(
    n = n()
  ) |> 
  ggplot(aes(x = n, y = reorder(victory_status, -n))) + 
  geom_col() + 
  theme_bw() + 
  labs(x = 'Turns', y = 'Vistory Status', 
       title = 'Total Turns by Victory Status') +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
#
chess |> 
  select(turns, victory_status) |> 
  group_by(victory_status) |>
  summarise(
    n = n(),
    average = mean(turns)
  ) |> 
  ggplot(aes(x = average, y = reorder(victory_status, -average))) + 
  geom_col() + 
  theme_bw() + 
  labs(x = 'Turns', y = 'Vistory Status', 
       title = 'Total Turns by Victory Status') +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
#
#
#
#
#
chess |> 
  select(opening_name) |> 
  group_by( opening_name) |>
  summarise(
    n = n()
  ) |> View()
ggplot(aes(x = n, y = reorder(victory_status, -n))) + 
  geom_col() + 
  theme_bw() + 
  labs(x = 'Turns', y = 'Vistory Status', 
       title = 'Total Turns by Victory Status') +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )



#

# start by removing the duplicates of game ids
# table(chess$game_id)
moves_victory <- chess[c("moves", "victory_status")] #|> 
# mutate(
#   moves = paste(moves, NA, sep  = ' ')
# )
moves_victory |> 
  View()
#
# moves <- as.vector(moves_victory$moves) |> 
#   unique()
# (moves) 
# #
# #
# #
# 
# #
# moves_victory |> 
#   separate_wider_delim(
#     cols = moves, delim = ' ', 
#     names = 'moves') |> 
#   view()
# #
#
# moves_victory |> 
#   mutate(
#     moves_2 =
#       moves |> (
#         \(x) strsplit(x, split = ' |-')[[1]])()
#       ) |> View()
#     # rowwise() |>
#     extract(
#       moves_2, 
#       into = str_c("move", 1:349), regex = "(.*)"
#       #, regex = '([[:alnum:]]+)'
#     ) |> View()
#     #
# moves_victory_1 = within(
#   moves_victory, 
#   move <- data.frame(
#     do.call(
#       'rbind', 
#       strsplit(as.character(moves), split = ' |-', fixed = T)
#     )
#   )
# )
# View(moves_victory_1)
#
#' #
#' library(dplyr)
#' #
#' move <- data.frame(
#'   do.call(
#'     'bind_rows', #
#'     #'rbind', 
#'     c(
#'       strsplit(as.character(moves_victory$moves), split = ' |-|NA', fixed = !TRUE),
#'       .id = seq(1)
#'       #deparse.level = 2)
#'     )
#'   ))
#' View(move)
#
#
bind_func <- function(x){
  val = strsplit(as.character(x), split = ' |-|NA', fixed = !TRUE)
  # print(val)
  len_val = length(val[[1]])
  val = set_names(x = val[[1]], nm = paste0('move', 1:len_val))
  val
}
bind_func(moves_victory$moves[1])
#
move <- data.frame(NULL)
for (i in moves_victory$moves){
  move = bind_rows(
    move, bind_func(i)
  )
}
View(move)
#

# move <- data.frame(
#   do.call(
#     'bind_rows',
#     # strsplit(as.character(moves_victory$moves), split = ' |-|NA', fixed = !TRUE)
#     bind_func(moves_victory$moves[1:2])
#   )
# )
# View(move)
# #
# data.frame(
#   x = letters[1:8],
#   y = 1:8,
#   colour = c('black', 'white')
# ) |> 
#   ggplot(
#     aes(x = x, y = y)
#   ) + 
#   geom_tile(aes(fill = colour)) +
#   scale_fill_identity(guide = "legend")
# #scale_continuous_identity()
#
board = expand.grid(
  X = 1:8,
  Y = 1:8
)
#
board = board |> 
  mutate(
    color = ifelse((X-Y) %% 2 == 0, 'black', 'white')
  ) |> 
  mutate_all(factor)
#
board |> 
  ggplot(
    aes(x = X, y = Y, fill = color)
  ) + 
  geom_tile() + 
  scale_fill_manual(
    values = c(
      'white' = '#FFFFFF', 'black' = '#000000'
    )) +
  scale_x_discrete(labels = letters[1:8], name = '') +
  scale_y_discrete(name = '') + 
  theme_minimal() +
  theme(
    legend.position = 'none'
  )
#
