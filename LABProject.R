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