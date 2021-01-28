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