source("~/Desktop/R-othello /Functions.R")
Computer = "W"
Player = "B"
while(T){
  correct = F
  while(!correct){
    printBoard(Board)
    Moves = getValidMoves(Board, "B")
    for(each in Moves){
      cat("(", each[1],",",each[2], ")\n",sep="")
    }
    row = as.numeric(readline(prompt = "Which Row would you like to move in: "))
    col = as.numeric(readline(prompt = "Which Column would you like to move in: "))
    for(each in Moves){
      if(!(F %in% c(each==c(row,col)))){
        correct = T
      }
    }
    if(!correct){
      print("BAD MOVE TRY AGAIN")
    }
  }
  Board = makeMove(Board, "B", row,col)
  
  #Computer Moves
  cat("Computer is now moving.\n")
  Move = GetBestMove(Board,"W")[[1]]
  print("8*8")
  print(Move)
  Board = makeMove(Board, "W", Move[1],Move[2])
}