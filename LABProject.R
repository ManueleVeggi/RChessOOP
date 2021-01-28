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