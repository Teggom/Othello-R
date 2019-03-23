source("~/../Desktop/Othello/Functions.R")

# This is a simple version that lets you play against the easiest bot possible
# This bot has a handmade heuristic and plays the optimal move on that heuristic
# It does NOT look ahead!

# This simple AI will be ran against itself many times 
# The result will provide a more robust heuristic, one that will help
#  give a good move for both which tiles fipped are important
#  and which tiles played are important

Computer = "W"
Player = "B"
while(!isBoardFull(Board)){
  correct = F
  while(!(correct && hasValidMoves(Board, "B"))){
    printBoard(Board)
    Moves = getValidMoves(Board, "B")
    cat("Valid Moves:\n")
    for(each in Moves){
      cat("\t(", each[1],",",each[2], ")\n",sep="")
    }
    row = as.numeric(readline(prompt = "Which Row would you like to move in: "))
    col = as.numeric(readline(prompt = "Which Column would you like to move in: "))
    if(row == -1 || col == -1){
      stop("Default stop.")
    }
    for(each in Moves){
      if(!(F %in% c(each==c(row,col)))){
        correct = T
      }
    }
    if(!correct){
      print("BAD MOVE TRY AGAIN")
    }
  } 
  if(hasValidMoves(Board, "B")){
    Board = makeMove(Board, "B", row,col)
  } else {
    cat("Player has no valid moves!\n")
  }
    
  #Computer Moves
  cat("Computer is now moving.\n")
  if(hasValidMoves(Board, "W")){
    Move = GetBestMove(Board,"W")[[1]]
    Board = makeMove(Board, "W", Move[1],Move[2])
  } else {
    cat("Computer has no valid moves!\n")
  }
}
