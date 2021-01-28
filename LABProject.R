Coordinates <- setRefClass("Coordinates",
                           fields = list(
                             col = "numeric",
                             row = "numeric",
                             #  isValid is true if - and only if - the position
                             #  (col,row) is a valid coordinate in the chessboard
                             isValid = "logical"
                           ),
                           methods = list(
                             initialize = function(newCol, newRow) {
                               col <<- newCol
                               row <<- newRow
                               #  We need to check that col and row are valid
                               #  coordinates for the chessboard (so >= 1 and <= 8)
                               if ((col >= 1) && (col <= 8) && 
                                   (row >= 1) && (row <= 8)) {
                                 isValid <<- TRUE
                               } else {
                                 isValid <<- FALSE
                               }
                             }
                           ))

Pawn <- setRefClass("Pawn",
                    fields = list(
                      color = "character", # w - white, b - black
                      isPiece = "logical"
                    ),
                    methods = list(
                      initialize = function(newColor) {
                        #  We want to check if the newColor passed as parameter 
                        #  is 'w' or 'b'; If the value is not valid, the 
                        #  execution of the software is stopped
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isPiece <<- FALSE
                        }},
                      checkTrajectory = function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #  defines the move this piece can do
                      checkMove = function(source, destination, cb, currentColor) {
                        if (currentColor == "b") {
                          if (source$row==7) { 
                            if (((destination$row-source$row) == -2) && (destination$col == source$col)) {
                              return(TRUE)
                            }}
                          if ((destination$row-source$row) == -1) {
                            if (abs(destination$col-source$col) <= 1) {
                              return(TRUE)
                            } #default move
                          }}
                        else if (currentColor == "w") {
                          if (source$row == 2) { 
                            if (((destination$row-source$row) == 2 || (destination$col == source$col))) {
                              return(TRUE)
                            }}
                          if ((destination$row-source$row) == 1) {
                            if (abs(destination$col-source$col) <= 1) {
                              return(TRUE)
                            } #default move
                          }}
                        return(FALSE)
                      }
                    ))

King <- setRefClass("King",
                    fields = list(
                      color = "character", #  w - white, p - black
                      isKing = "logical"
                    ),
                    methods = list(
                      initialize = function(newColor) {
                        #  We want to check if the newColor passed as parameter 
                        #  is 'w' or 'b'; If the value is not valid, the 
                        #  execution of the software is stopped
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isKing <<- FALSE
                        }
                      },
                      checkTrajectory = function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #  defines the move this piece can do
                      checkMove = function(source,destination, cb, currentColor) {
                        if (abs(destination$row-source$row) <= 1) {
                          #  default move of the king
                          if (abs(destination$col-source$col) <= 1) {
                            # valid move
                            return(TRUE)
                          }
                        }
                        #  invalid move
                        return(FALSE)
                      }
                    ))

Rook <- setRefClass("Rook",
                    fields=list(
                      color = "character", #  w - white, b - black
                      isRook = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        #  We want to check if the newColor passed as parameter 
                        #  is 'w' or 'b'; If the value is not valid, the 
                        #  execution of the software is stopped
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isRook <<- FALSE
                        }
                      },
                      checkTrajectory = function(src, dest, currentColor, cb) {
                        cat("Here I am, in checkTrajectory!")
                        swapped<-FALSE
                        if ((src$col == dest$col) && (abs(src$row-dest$row)>1))  {
                          if (src$row > dest$row) {
                            temp <- src$row
                            src$row <- dest$row
                            dest$row <- temp
                            swapped <- TRUE
                          }
                          for (i in seq (src$row+1, dest$row-1)) {
                            if (!(is.null(cb[i, src$col][[1]]))) {
                              cat("Trajectory 1\n")
                              return(FALSE)}}
                          if(swapped) { 
                            #  re-swapping
                            temp <- src$row
                            src$row <- dest$row
                            dest$row <- temp
                          }}
                        else if ((src$row==dest$row) && (abs(src$col-dest$col)>1)) { 
                          if (src$col > dest$col) {
                            temp <- src$col
                            src$col <- dest$col
                            dest$col <- temp
                            swapped <- TRUE
                          }
                          for (i in seq (src$col + 1, dest$col - 1)) {
                            if (!(is.null(cb[src$row, i][[1]]))) {
                              cat("Trajectory 2\n")
                              return(FALSE)}}
                          if(swapped) { 
                            #   re-swapping
                            temp <- src$col
                            src$col <- dest$col
                            dest$col <- temp
                          }
                        }
                        cat("checktrajectory is true")
                        cat("a: ", src$row)
                        cat("b: ", src$col)
                        cat("c: ", dest$row)
                        cat("d: ", dest$col)
                        return (TRUE)
                      },
                      #  defines the move this piece can do
                      checkMove=function(source,destination, cb, currentColor) {
                        cat("Here I am, in checkMove!")
                        if (((abs(destination$row-source$row) <= 8) && (destination$col == source$col)) || 
                            (((destination$row == source$row)) && ((abs(destination$col-source$col) <= 8 )))) {
                          #  valid move
                          return(TRUE)
                        }
                        #  invalid move
                        cat("Move not allowed\n")
                        return(FALSE)
                      }
                    ))
=======
Bishop <- setRefClass("Bishop",
                      fields = list(
                        color = "character", # w - white, b - black
                        isBishop = "logical"
                      ),
                      methods = list(
                        initialize = function(newColor) {
                          #  We want to check if the newColor passed as parameter 
                          #  is 'w' or 'b'; If the value is not valid, the 
                          #  execution of the software is stopped
                          if ((newColor != "w") && (newColor != "b")) {
                            stop("invalid parameters to the ctor (", newColor, 
                                 " but only 'b' and 'w' are valid)")
                          } else {
                            color <<- newColor
                            isBishop <<- FALSE
                          }
                        },
                        checkTrajectory = function(source, destination, currentColor, cb) {
                          absDist <- as.integer((abs(destination$row-source$row)))
                          if (absDist > 1) {
                            if ((source$row<destination$row) && (source$col<destination$col))  {
                              for (i in seq (1, absDist - 1)) {
                                if (!(is.null(cb[source$row + i, source$col + i][[1]]))) {
                                  cat("Trajectory 1 wrong\n")
                                  return(FALSE)}}
                            }
                            else if ((source$row<destination$row) && (source$col>destination$col))  {
                              for (i in seq (1, absDist - 1)) {
                                if (!(is.null(cb[source$row + i, source$col - i][[1]]))) {
                                  cat("Trajectory 2 wrong\n")
                                  return(FALSE)}}
                            }
                            else if ((source$row > destination$row) && (source$col < destination$col))  {
                              temp <- source$row
                              source$row <- destination$row
                              destination$row <- temp
                              temp1 <- source$col
                              source$col <- destination$col
                              destination$col <- temp1
                              for (i in seq (1, absDist - 1)) {
                                if (!(is.null(cb[source$row + i, source$col - i][[1]]))) {
                                  cat("Trajectory 2 wrong\n")
                                  return(FALSE)}}
                              temp <- source$row
                              source$row <- destination$row
                              destination$row <- temp
                              temp1 <- source$col
                              source$col <- destination$col
                              destination$col <- temp1
                              
                            }
                            else if ((source$row>destination$row) && (source$col>destination$col))  {
                              temp <- source$row
                              source$row <- destination$row
                              destination$row <- temp
                              temp1 <- source$col
                              source$col <- destination$col
                              destination$col <- temp1
                              for (i in seq (1, absDist - 1)) {
                                cat("\nrow", source$row+i)
                                cat("\nncol", source$col+i)
                                cat("\n i: ", i)
                                if (!(is.null(cb[source$col+i, source$row+i][[1]]))) {
                                  cat("Trajectory 1 wrong\n")
                                  return(FALSE)}}
                              temp <- source$row
                              source$row <- destination$row
                              destination$row <- temp
                              temp1 <- source$col
                              source$col <- destination$col
                              destination$col <- temp1
                            }}
                          return(TRUE)},   
                        #  defines the move this piece can do
                        checkMove=function(source, destination, cb, currentColor) {
                          if ((abs(destination$col-source$col)) == (abs(destination$row-source$row))) {
                            #  valid move
                            return(TRUE)
                          }
                          # invalid move
                          return(FALSE)
                        }
                      ))

Knight <- setRefClass("Knight",
                      fields = list(
                        color = "character", # w - white, b - black
                        isKnight = "logical"
                      ),
                      methods = list(
                        initialize = function(newColor) {
                          # We want to check if the newColor passed as parameter 
                          # is 'w' or 'b'; If the value is not valid, the 
                          #  execution of the software is stopped
                          if ((newColor != "w") && (newColor != "b")) {
                            stop("invalid parameters to the ctor (", newColor, 
                                 " but only 'b' and 'w' are valid)")
                          } else {
                            color <<- newColor
                            isKnight <<- FALSE
                          }
                        },
                        checkTrajectory = function(source, destination, currentColor) {
                          return (TRUE)
                        },
                        #  defines the move this piece can do
                        checkMove = function(source, destination, cb, currentColor) {
                          if ((((((abs(destination$row-source$row)) == 2) && (abs(destination$col-source$col)) == 1)) || 
                               (((abs(destination$col-source$col)) == 1) && (abs(destination$row-source$row)) == 2))) {
                            #  valid move
                            return(TRUE)
                          }
                          #  invalid move
                          return(FALSE)
                        }
                      ))

Queen <- setRefClass("Queen",
                     fields=list(
                       color = "character", # w - white, b - black
                       isQueen = "logical"
                     ),
                     methods=list(
                       initialize = function(newColor) {
                         # We want to check if the newColor passed as parameter 
                         # is 'w' or 'b'; If the value is not valid, the 
                         # execution of the software is stopped
                         if ((newColor != "w") && (newColor != "b")) {
                           stop("invalid parameters to the ctor (", newColor, 
                                " but only 'b' and 'w' are valid)")
                         } else {
                           color <<- newColor
                           isQueen <<- FALSE
                         }
                       },
                       checkTrajectory=function(source, destination, currentColor) {
                         return(TRUE)},
                       #  defines the move this piece can do
                       checkMove=function(source, destination, cb, currentColor) {
                         if (((abs(destination$row-source$row)<=8) && ((abs(destination$col-source$col))==0 )) || ((abs(destination$col-source$col)==0) && ((abs(destination$row-source$row))<=8 ) ) || ((abs(destination$col-source$col)) == (abs(destination$row-source$row)))) {
                           # valid move
                           return(TRUE)
                         }
                         # invalid move
                         return(FALSE)
                       }
                     ))

#  Chessboard is the heart of this game; It is responsible for the representation
#  of the chessboard and supervises the game, verifying that the constraints 
#  and the rules of the game are followed.
Chessboard <- setRefClass("Chessboard",
                          fields  = list(
                            chessboard = "matrix"
                          ),
                          methods = list(
                            initialize = function() {
                              #  We initialize a 8x8 matrix and we fill it with empty lists.
                              chessboard <<- matrix(list(NULL), nrow = 8, ncol = 8)
                              #  Now we want to initialize and set the chessboard to its initial configuration,
                              #  populating the matrix with Pieces(). In R Matrices have no idea how to store a 
                              #  ReferenceClass but they know how to store a list() and lists know how to
                              #  store a ReferenceClass.
                              #  This means that each cell of the matrix contains an empty list or
                              #  a list with an object inside and we are going to use the operator [[n]]
                              #  to access the n-th element of this list (in our case, there is only one element so, always [[1]])
                              #
                              #  If empty,      chessboard[y,x][[1]] = NULL
                              #  If not empty,  chessboard[y,x][[1]] = object of type piece
                              #
                              for (col in seq(1,8, by=1)) {
                                chessboard[2,col] <<- list(Pawn("w"))
                                chessboard[7,col] <<- list(Pawn("b"))
                                chessboard[1,4] <<- list(King("w"))
                                chessboard[8,5] <<- list(King("b"))
                                chessboard[1,1] <<- list(Rook("w"))
                                chessboard[1,8] <<- list(Rook("w"))
                                chessboard[8,1] <<- list(Rook("b"))
                                chessboard[8,8] <<- list(Rook("b"))
                                chessboard[1,2] <<- list(Knight("w"))
                                chessboard[1,7] <<- list(Knight("w"))
                                chessboard[8,2] <<- list(Knight("b"))
                                chessboard[8,7] <<- list(Knight("b"))
                                chessboard[1,3] <<- list(Bishop("w"))
                                chessboard[1,6] <<- list(Bishop("w"))
                                chessboard[8,3] <<- list(Bishop("b"))
                                chessboard[8,6] <<- list(Bishop("b"))
                                chessboard[1,5] <<- list(Queen("w"))
                                chessboard[8,4] <<- list(Queen("b"))
                              }},
                            exit = function() {
                              .Internal(.invokeRestart(list(NULL, NULL), NULL))
                            },
                            draw = function() {
                              cat("    ")
                              for (header in seq(1, 8)) {
                                cat(header, "   ", sep="")
                              }
                              cat("\n")
                              for (row in seq(1, 8)) {
                                cat(row, " ", sep="")
                                for (col in seq(1, 8)) {
                                  if (is(chessboard[row, col][[1]], "Pawn")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","p", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "P", 
                                          " ", sep="")
                                    }}
                                  else if (is(chessboard[row, col][[1]], "King")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","k", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "K", 
                                          " ", sep="")
                                    }}
                                  else if (is(chessboard[row, col][[1]], "Rook")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","r", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "R", 
                                          " ", sep="")
                                    }}
                                  else if (is(chessboard[row, col][[1]], "Queen")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","q", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "Q", 
                                          " ", sep="")
                                    }}
                                  else if (is(chessboard[row, col][[1]], "Bishop")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","b", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "B", 
                                          " ", sep="")
                                    }}
                                  else if (is(chessboard[row, col][[1]], "Knight")) {
                                    if ((chessboard[row, col][[1]]$color) == "w") {
                                      cat("| ","c", 
                                          " ", sep="")
                                    } 
                                    else{
                                      cat("| ", "C", 
                                          " ", sep="")
                                    }}
                                  else {
                                    cat("|   ")
                                  }
                                }
                                cat("|\n  ")
                                for (col in seq(1, 8)) {
                                  cat("----")
                                }
                                cat("-\n")
                              }},
                            
                            checkOverlapPieces=function(source,destination, currentColor){
                              cat("11:", source$row)
                              cat("12:", source$col)
                              cat("21:", destination$row)
                              cat("22:", destination$col)
                              if (((is(chessboard[destination$row, destination$col][[1]],  "Pawn")   == TRUE)  || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "King")   == TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Rook")   == TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Knight") == TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Queen")  == TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Bishop") == TRUE))) &&  
                                  (chessboard[destination$row, destination$col][[1]]$color == currentColor )) 
                              {
                                cat("Overlapping\n")
                                return(FALSE)
                              }
                              return(TRUE)
                            },
                            checkMovablePiece=function(source,destination, currentColor){
                              if (((is(chessboard[source$row, source$col][[1]], "Pawn")    == TRUE)   || 
                                   ((is(chessboard[source$row, source$col][[1]], "King")   == TRUE))  || 
                                   ((is(chessboard[source$row, source$col][[1]], "Rook")   == TRUE))  || 
                                   ((is(chessboard[source$row, source$col][[1]], "Knight") == TRUE))  || 
                                   ((is(chessboard[source$row, source$col][[1]], "Queen")  == TRUE))  || 
                                   ((is(chessboard[source$row, source$col][[1]], "Bishop") == TRUE))) &&  
                                  ((chessboard[source$row, source$col][[1]]$color == currentColor))) {
                                return(TRUE)
                              }
                              cat("Movable\n")
                              return(FALSE)
                            },
                            checkEatablePieces=function(source,destination, currentColor){
                              # evitare di auto-mangiarmi
                              if (((is(chessboard[destination$row, destination$col][[1]], "Pawn") ==TRUE) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "King") ==TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Rook") ==TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Knight") ==TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Queen") ==TRUE)) || 
                                   ((is(chessboard[destination$row, destination$col][[1]], "Bishop") ==TRUE))) &&  
                                  (chessboard[destination$row, destination$col][[1]]$color != currentColor)) 
                              {
                                return(TRUE)
                                cat("Eatable!\n")
                              }
                              
                              return(FALSE)
                            },
                            move=function(source,destination,currentColor){
                              #  firstly, you check if the cell is occupied and where the move is allowed   
                              if (checkOverlapPieces(source, destination, currentColor)) {cat("/n1 true")}
                              if (movendum$checkMove(source,destination, cb, currentColor)) {cat("/n2 true")}
                              if (checkMovablePiece(source,destination, currentColor)) {cat("/n3 true")}
                              if (movendum$checkTrajectory(source,destination, currentColor, chessboard)) {cat("/n4 true")}
                              if (((checkOverlapPieces(source, destination, currentColor)) && 
                                   (movendum$checkMove(source,destination, cb, currentColor)) &&  
                                   (checkMovablePiece(source,destination, currentColor)) && 
                                   (movendum$checkTrajectory(source,destination, currentColor, chessboard)))) {
                                # se il movendum appartiene ad una classe diversa rispetto a quella di destinazione 
                                cat("1:", source$row)
                                cat("2:", source$col)
                                cat("3:", destination$row)
                                cat("4:", destination$col)
                                if (checkEatablePieces(source, destination, currentColor)) { 
                                  cat("checkeatablepieces")
                                  if (is(chessboard[destination$row, destination$col][[1]], "King")) {
                                    cat(paste(currentPlayer, "is the winner!\n"))
                                    cat("Type wow to exit the game")
                                    txt <- readline("Type: ")
                                    if (txt == "wow") {
                                      exit()
                                    }}
                                  
                                  chessboard[destination$row, destination$col] <<- list(NULL) 
                                  chessboard[destination$row, destination$col] <<- chessboard[source$row, source$col]         
                                  chessboard[source$row, source$col] <<- list(NULL)
                                  return(TRUE)}
                                else if (!(checkEatablePieces(source, destination, currentColor))) {
                                  chessboard[destination$row, destination$col] <<- chessboard[source$row, source$col]
                                  chessboard[source$row, source$col] <<- list(NULL)
                                  return(TRUE)}
                                else {
                                  cat("Uncorrect move, try again!\n")
                                  return(FALSE)}
                              }
                            },
                            switch=function(source,destination, currentColor) {
                              for (col in seq(1,8, by=1)) {
                                if ((is(chessboard[1, col][[1]], "Pawn")) && ((chessboard[1, col][[1]])$color == "b")) {
                                  chessboard[1, col] <<- list(Queen("b"))
                                  return(TRUE)
                                }
                                else if ((is(chessboard[8, col][[1]], "Pawn")) && ((chessboard[1, col][[1]])$color == "w")) {
                                  chessboard[8, col] <<- list(Queen("w"))
                                  return(TRUE)
                                }}
                              
                              return(FALSE)
                            }
                          ))

chessboard <- Chessboard()
turn <- 0
playerColors <- c("w", "b")
playerNames <- c("WHITE", "BLACK")
while (TRUE) {
  # turn is 0  (0/2) = 0 (0%%2) = 0
  # turn is 1  (1/2) = 0 (1%%2) = 1
  # turn is 2  (2/2) = 1 (2%%2) = 0
  # turn is 3  (3/2) = 1 (3%%2) = 1
  currentColor <- playerColors[(turn %% 2) + 1]
  currentPlayer <- playerNames[(turn %% 2) + 1]
  chessboard$draw()
  cat("Current turn: ", (turn + 1), "\n", sep="")
  cat("Player ", currentPlayer, " moves", sep="")
  pcType <- readline("Which piece would like to move? ")
  srcCol <- readline("piece col? ")
  if (srcCol == "q") {
    break;
  }
  srcRow <- readline("piece row? ")
  if (srcRow == "q") {
    break;
  }
  dstCol <- readline("moves to col? ")
  if (dstCol == "q") {
    break;
  }
  dstRow <- readline("moves to row? ")
  if (dstRow == "q") {
    break;
  }
  
  if (pcType == "q") {
    break;
  }
  if (pcType == "pawn") {
    movendum <- Piece(as.character(type))}
  if (pcType == "queen") {
    type <<- "b"
    movendum <- Queen(as.character(type))}
  if (pcType == "knight") {
    type <<- "b"
    movendum <- Knight(as.character(type))} 
  if (pcType == "bishop") {
    type <<- "b"
    movendum <- Bishop(as.character(type))} 
  if (pcType == "king") {
    type <<- "b"
    movendum <- King(as.character(type))}
  if (pcType == "rook") {
    type <<- "b"
    movendum <- Rook(as.character(type))}
  source <- Coordinates(as.integer(srcCol), as.integer(srcRow))
  destination <- Coordinates(as.integer(dstCol), as.integer(dstRow))
  if (source$isValid) {
    if (destination$isValid) {
      # We want to give the source and destination coordinates to the method
      # move() of the chessboard class. If the method returns a TRUE 
      # (so the movement source->destination was legit), then we can 
      # move to the next turn.
      if (chessboard$move(source, destination, currentColor)) {
        turn <- turn + 1
        cat("Both coordinates are valid\n")
        if (chessboard$switch(source,destination, currentColor))
        {cat("Your pawn made it! You have one more Queen now") }
      }
    } 
    else {
      cat("The destination coordinate is not valid\n")
    }
  } else {
    cat("The source coordinate is not valid\n")
  }
  
}
