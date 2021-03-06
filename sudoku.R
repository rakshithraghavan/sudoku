sudoku_board <- rbind(c(9, NA, 6,8,NA, NA, 3, NA, NA),
                      c(NA, NA, NA, NA, 7, NA, NA, NA, NA),
                      c(NA, NA, 1, NA, NA, NA, 6, NA, 2),
                      c(NA, NA, NA, NA, 6, NA, 1, NA, 5),
                      c(NA, 7, NA, NA, 5, NA, NA, 3, NA),
                      c(6, NA, 3, NA, 8, NA, NA, NA, NA),
                      c(4, NA, 2, NA, NA, NA, 5, NA, NA),
                      c(NA, NA, NA, NA, 1, NA, NA, NA, NA),
                      c(NA, NA, 5, NA, NA, 4, 9, NA, 1)
)

possible_choices <- function(board, i, j){
  # Get the list of possible numbers at a tile, given filled in numbers
  possible_answers <- rep(TRUE,9)
  selected_num <- unique(c(board[i,], board[,j], board[3*((i-1) %/% 3) + 1:3,3*((j-1) %/% 3) + 1:3]))
  selected_num <- na.omit(selected_num)
  possible_answers[selected_num] <- FALSE
  return(possible_answers)
}


solve_sudoku <- function(board, progress=81){
  if(progress==0){
    # Successfully filled in the board
    return(board)
  }
  # Get the i,j coordinates
  i <- ((progress-1)%%9) +1
  j <- ((progress-1)%/%9) +1
  if(is.na(board[i,j])){
    choices <- which(possible_choices(board, i, j))
  }else{
    choices <- c(board[i,j])
  }
  for(k in choices){
    board[i,j] <- k
    answer <- solve_sudoku(board, progress-1)
    if(!is.null(answer)){
      return(answer)
    }
  }
  return(NULL)
}
solve_sudoku(sudoku_board)
