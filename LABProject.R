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