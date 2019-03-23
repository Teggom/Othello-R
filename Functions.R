# Setup
Board = matrix(rep(x = c(" "), times = 64), nrow=8, ncol=8)
colnames(Board) = LETTERS[1:8]
rownames(Board) = letters[1:8]
Board[4,5] = "B"
Board[5,4] = "B"
Board[4,4] = "W"
Board[5,5] = "W"

Heuristic = matrix(rep(x=0,size=64),nrow=8)

# A weak heuristic function I defined by hand
easyHeuristic <- function(){
  # setup
  Heuristic = matrix(rep(x = 10,size = 64),nrow = 8, ncol = 8)
  # Edges
  Heuristic[2:7,c(1,8)] = 3
  Heuristic[c(1,8),2:7] = 3
  # Middle
  Heuristic[2:7,2:7] = 1
  Heuristic[3:6,3:6] = .45
  Heuristic[4:5,4:5] = .2
  #Touchup
  Heuristic[c(1,1,2,2,7,7,8,8),c(2,7,1,8,1,8,2,7)] = .2
  Heuristic[c(2,2,7,7),c(2,7,2,7)] = 0
  #Counter
  Heuristic[c(1,1,8,8),c(1,8,1,8)] = 40
}  
  
Heuristic <- easyHeuristic()

# Given a board, heuristic, and color: calculate the best move
GetBestMove = function(board,Color){
  Moves = getValidMoves(board,Color)
  Values = rep(x=0,length(Moves))
  for(each in 1:length(Moves)){
    #print(each)
    
    Values[each] = CalculateHeuristicState(makeMove(Board,Color,Moves[[each]][1],Moves[[each]][2]),Color)
  }
  return(Moves[which.max(Values)])
}

## Given a Heuristic, calcualate how optimal the current board is
CalculateHeuristicState = function(Board,Color){
  GoodBoard = Board
  if(Color == "W"){
    NotColor = "B"
  } else {
    NotColor = "W"
  }
  GoodBoard[GoodBoard==Color] = 1
  GoodBoard[GoodBoard==NotColor] = -1
  GoodBoard[GoodBoard==" "] = 0
  GoodBoard = matrix(as.numeric(GoodBoard),nrow=8,ncol=8)
  
  return(sum(GoodBoard * Heuristic))
}

## Preferred way to print the board
printBoard = function(board){
  cat("  \033[4m",LETTERS[1:8],"\033[24m\n", sep = " ")
  for(row in 1:8){
    cat((1:8)[row], "\033[4m|\033[24m")
    for(col in 1:8){
      if(board[row,col]==" "){
        cat("\033[4m |\033[24m")
      } else if(board[row,col]=="B"){
        cat("\033[4m\033[1m@\033[22m|\033[24m")
      } else {
        cat("\033[4mO|\033[24m")
      }
    }
    cat("\n")
  }
}

## Returns a board with the updated move and tiles flipped. 
makeMove = function(board, Color, row, col){
  print(Color)
  print(row)
  print(col)
  print("Heh")
  if(board[row,col]!=" "){
    print("Hah-")
    warning("ERROR, BAD TILE ENTERED!")
    return(board)
  }
  if(Color == "W"){
    NotColor = "B"
  } else {
    NotColor = "W"
  }
  
  #Flip Up
  offset = 1
  if(row > 1 && board[row-1,col]==NotColor){
    while(row-offset > 0 && board[row-offset,col]==NotColor){
      offset = offset + 1
    }
    if(row-offset > 0 && board[row-offset,col]==Color){
      board[row:(row-offset),col] = Color
    }
  }
  
  #Flip Right
  offset = 1
  if(col < 8  && board[row,col+1]==NotColor){
    while(col+offset <9 && board[row,col+offset]==NotColor){
      offset = offset + 1
    }
    if(col+offset < 9 && board[row,col+offset]==Color){
      board[row,col:(col+offset)] = Color
    }
  }
  
  #Flip Left
  offset = 1
  if(col > 1 && board[row,col-1]==NotColor){
    while(col-offset > 0 && board[row,col-offset]==NotColor){
      offset = offset + 1
    }
    if(col-offset > 0 && board[row,col-offset] == Color){
      board[row,col:(col-offset)] = Color
    }
  }
  
  #Flip Down
  offset = 1
  if(row<8 && board[row+1,col]==NotColor){
    while(row+offset < 9 && board[row+offset,col]==NotColor){
      offset = offset + 1
    }
    if(row+offset <9 && board[row+offset,col]==Color){
      board[row:(row+offset),col] = Color
    }
  }
  
  #Flip UpRight
  offset = 1
  if(row > 1 && col < 8 && board[row-1,col+1]==NotColor){
    while(row-offset > 0 && col + offset < 9 && board[row-offset,col+offset]==NotColor){
      offset = offset + 1
    }
    if(row-offset > 0 && col+offset < 9 && board[row-offset,col+offset]==Color){
      for(each in 0:offset){
        board[row-each,col+each] = Color
      }
    }
  }
  
  #Flip UpLeft
  offset = 1
  if(row > 1 && col > 1 && board[row-1, col-1]==NotColor){
    while(row-offset > 0 && col - offset > 0 && board[row-offset,col-offset]==NotColor){
      offset = offset + 1
    }
    if(row-offset > 0 && col - offset > 0 && board[row-offset,col-offset]==Color){
      for(each in 0:offset){
        board[row-each,col-each] = Color
      }
    }
  }

  #Flip DownRight
  offset = 1
  if(row < 8 && col < 8 && board[row+1,col+1] == NotColor){
    while(row+offset < 9 && col+offset < 9 && board[row+offset,col+offset]==NotColor){
      offset = offset + 1
    }
    if(row+offset < 9 && col + offset < 9 && board[row+offset,col+offset]==Color){
      for(each in 0:offset){
        board[row+each,col+each] = Color
      }
    }
  }
  
  #Flip DownLeft
  offset = 1
  if(row< 8 && col > 1 && board[row+1,col-1] == NotColor){
    while(row+offset < 9 && col-offset > 0 && board[row+offset,col-offset]==NotColor){
      offset = offset + 1
    }
    if(row+offset < 9 && col - offset > 0 && board[row+offset,col-offset]==Color){
      for(each in 0:offset){
        board[row+each,col-each] = Color
      }
    }
  }
  
  return(board)  
}

## Returns a list of vectors with each vector being the c(row, col) 
## of a valid move
getValidMoves = function(board, Color){
  Combos = list()
  if(Color =="W"){
    NotColor = "B"
  } else {
    NotColor = "W"
  }
  for(row in 1:8){
    for(col in 1:8){
      isValid = F
      if(board[row,col]==" "){
        #Check up
        if(row>1){
          if(board[row-1,col]==NotColor){
            offset = 2
            while(row-offset>0 && board[row-offset,col]!=" "){
              if(board[row-offset,col]==Color){
                isValid = T
              }
              offset = offset+1
            }
          }
        }
        
        #Check left
        if(col>1 && !isValid){
          if(board[row,col-1] == NotColor){
            offset = 2
            while(col-offset>0 && board[row,col-offset]!=" "){
              if(board[row,col-offset]==Color){
                isValid = T
              }
              offset = offset+1
            }
          }
        }
        
        #Check down
        if(row<8 && !isValid){
          if(board[row+1,col] == NotColor){
            offset = 2
            while(row+offset<9 && board[row+offset,col]!=" "){
              if(board[row+offset, col]==Color){
                isValid = T
              }
              offset = offset+1
            }
          }
        }
        
        #Check right
        if(col<8 && !isValid){
          if(board[row,col+1] == NotColor){
            offset = 2
            while(col+offset<9 && board[row,col+offset]!=" "){
              if(board[row,col+offset]==Color){
                isValid = T
              }
              offset = offset + 1
            }
          }
        }
        
        #Check downleft
        if(col>1 && row<8 && !isValid){
          if(board[row+1,col-1] == NotColor){
            offset = 2
            while(col-offset>0 && row+offset<9 && board[row+offset, col-offset]!= " "){
              if(board[row+offset, col-offset]==Color){
                isValid = T
              }
              offset = offset + 1
            }
          }
        }
        
        #Check downright
        if(col<8 && row<8 && !isValid){
          if(board[row+1,col+1]==NotColor){
            offset = 2
            while(col+offset<9 && row+offset<9 && board[row+offset, col+offset]!= " "){
              if(board[row+offset, col+offset]==Color){
                isValid = T
              }
              offset = offset + 1
            }
          }
        }
        
        #Check upleft
        if(col>1 && row>1 && !isValid){
          if(board[row-1,col-1]==NotColor){
            offset = 2
            while(col-offset>0&&row-offset>0 && board[row-offset,col-offset]!= " "){
              if(board[row-offset,col-offset]==Color){
                isValid = T
              }
              offset = offset + 1
            }
          }
        }
        
        #Check upright
        if(col<8 && row>1 && !isValid){
          if(board[row-1,col+1]==NotColor){
            offset = 2
            while(col+offset<9 && row-offset>0 && board[row-offset,col+offset]!=" "){
              if(board[row-offset,col+offset]==Color){
                isValid = T
              }
              offset = offset + 1
            }
          }
        }
      }
      if(isValid){
        Combos[[length(Combos)+1]] = c(row,col)
      }
    }
  }
  return(Combos)
}

## Returns a true or false value if the board is full
# Easier to use and read than the return statement
isBoardFull <- function(GameBoard){
  return(!(" " %in% Board))
}

## Returns a true or false value if the specified player has valid moves
# Redundant since you could just check if the list
# returned from getValidMoves() has length(list)==0
# This function is here to simply streamline that process
hasValidMoves <- function(Board, Player){
  return(length(getValidMoves(board = Board, Color = Player))!=0)
}

## Returns color of winning player
## Returns "" in the event of a tie
getWinner <- function(Board){
  NumBlack = length(Board[Board=="B"])
  if(NumBlack==32){
    return("")
  }
  if(NumBlack < 32){
    return("W")
  } 
  return("B")
}
