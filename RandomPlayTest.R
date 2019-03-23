Board = matrix(rep(x = c(" "), times = 64), nrow=8, ncol=8)
colnames(Board) = LETTERS[1:8]
rownames(Board) = letters[1:8]
Board[4,5] = "B"
Board[5,4] = "B"
Board[4,4] = "W"
Board[5,5] = "W"

Player = "W"
for(each in 1:20){
  print(Board)
  if(Player == "W"){
    Player = "B"
  } else {
    Player = "W"
  }
  Moves = getValidMoves(Board,Player)
  point = sample(x = 1:length(Moves), 1)
  Board = makeMove(Board,Player,Moves[[point]][1],Moves[[point]][2])
  q = readline(prompt = "Press <Enter>: ")
}