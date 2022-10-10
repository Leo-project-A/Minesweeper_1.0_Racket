;--build the required board--
;builds an empty board colXrow
(define (BuildBoard col row)
  (cond
    ((= row 0) '())
    (else
     (cons (BuildRows col)(BuildBoard col (- row 1))))))
;build a empty row size of col
(define (BuildRows col)
  (cond
    ((= col 0) '())
    (else
     (cons '_ (BuildRows (- col 1))))))

;----------------------------------------------------------------
;creates board by players input
(define (Costume col row)
  (cond
    ((or (not(number? col))(not(number? row))) (YourBoard))
    ((< col 3)(Costume 3 row))
    ((> col 30)(Costume 30 row))
    ((< row 3)(Costume col 3))
    ((< row 3)(Costume col 30))
    (else
     (BuildBoard (+ 2 col) (+ 2 row)))))

;----------------------------------------------------------------
;--places a sign in the required location--
;constracts the new row to the rest of the board
(define (place-sign board X Y sign)
  (cond
    ((= Y 0) (cons (place-sign-help (first board) X sign) (rest board)))
    (else
     (cons (first board) (place-sign (rest board) X (- Y 1) sign)))))
;constracts a new row with the required sign in the required location
(define (place-sign-help board X sign)
  (cond
    ((= X 0) (cons sign (rest board)))
    (else
     (cons (first board) (place-sign-help (rest board) (- X 1) sign)))))

;----------------------------------------------------------------
;--find what sign is in the required location--
(define (check-location board X Y)
 (list-ref (list-ref board Y) X))

;----------------------------------------------------------------
;--places the mines on the board randomly--
(define (place-bombs bombNum board Width Height)
  (cond
    ((> bombNum (* Width Height)) (place-bombs (* Width Height) board Width Height))
    ((= bombNum 0) board)
    (else
     (place-bombs-help bombNum board (+ 1 (random Width)) (+ 1 (random Height)) Width Height))))
;palces a mine in a random location
(define (place-bombs-help bombNum board X Y Width Height)
  (cond
    ((not(equal? (check-location board X Y) '_)) (place-bombs bombNum board Width Height))
    (else
     (place-bombs (- bombNum 1) (place-sign board X Y 'b) Width Height))))

;----------------------------------------------------------------
;--checks all the board, and places a number in each location based on number on mines around it--
;goes through all the rows
(define (place-numbers board X Y)
  (cond
    ((= Y 0) board)
    (else
     (place-numbers (place-numbers-help board  X Y) X (- Y 1)))))
;in each location in row, checks the number on mines around it and places that number
(define (place-numbers-help board X Y)
  (cond
    ((= X 0) board)
    ((equal? (check-location board X Y) 'b)(place-numbers-help board (- X 1) Y))
    (else 
     (place-numbers-help (place-sign board X Y (numOfBombs board X Y)) (- X 1) Y))))
;sums the number of mines around the selected location
(define (numOfBombs board X Y)
  (+
    (numOfBombs-help board (+ 1 X) Y)
    (numOfBombs-help board (- X 1) Y)
    (numOfBombs-help board X (- Y 1))
    (numOfBombs-help board X (+ 1 Y))
    (numOfBombs-help board (+ 1 X) (+ 1 Y))
    (numOfBombs-help board (- X 1) (- Y 1))
    (numOfBombs-help board (+ 1 X) (- Y 1))
    (numOfBombs-help board (- X 1) (+ 1 Y))))
;checks if the location contains a mine, if so return 1
(define (numOfBombs-help board X Y)
  (cond
    ((equal? (check-location board X Y) 'b) 1)
    (else 0)))

;----------------------------------------------------------------
;--gets a location, and places the matching  sign on Player's board--
;places on the player's board a number acording the the game's board
(define (compare-boards YourBoard GameBoard X Y directions)
  (cond 
    ((equal? (check-location GameBoard X Y) '0) (open-board YourBoard GameBoard X Y directions #f))
    (else (place-sign YourBoard X Y (check-location GameBoard X Y)))))

;if the selected location is a 0, opens all the 0 near by
(define (open-board YourBoard GameBoard X Y directions same)
  (cond 
    ((empty? directions) (place-sign YourBoard X Y '0))
    ((equal? (check-location YourBoard X Y) '0) YourBoard)
    ((and (not same) (equal? (check-location YourBoard X Y) 'X)) YourBoard)
    (else 
     (open-board (compare-boards (place-sign YourBoard X Y 'X) GameBoard (+ X (first (first directions)))(+ Y (second (first directions))) Directions) GameBoard X Y (rest directions) #t))))

;all the directions that need to be checked
(define Directions '((0 +1)(-1 +1)(-1 -1)(+1 -1)(+1 +1)(+1 0)(-1 0)(0 -1)))

;----------------------------------------------------------------
;--checks if the board is full,if yes returns #t, if not #f--
;checks that every row is full
(define (full? board)
  (cond
    ((empty? (rest board)) #t)
    ((equal? (full?-help (rest (first board))) #f) #f)
    (else
     (full? (rest board)))))
;checks that a row is full
(define (full?-help board)
  (cond
    ((empty? (rest board)) #t)
    ((equal? (first board) '_) #f)
    (else
     (full?-help (rest board)))))

;----------------------------------------------------------------
;--prints the players board without the frame--
(define (printTheBoard board)
  (cond
    ((empty? (rest board)) '())
    (else
     (printTheBoard-help board))))
;prints every row in a line
(define (printTheBoard-help board)
  (newline)
  (print (printTheBoard-help2 (rest (first board))))
  (printTheBoard (rest board)))
;makes a row without the frame
(define (printTheBoard-help2 board)
  (cond
    ((empty? (rest board)) '())
    (else
     (cons (first board)(printTheBoard-help2 (rest board))))))

;----------------------------------------------------------------
;--starts the game--
(define (main)
  (print '(lets START!!))
  (printTheBoard (rest YourBoard))
  (newline)
  (checkKeys YourBoard GameBoard (GetX) (GetY) (GetSign)))

;----------------------------------------------------------------
;--checks if input is valid--
(define (checkKeys yourBoard gameBoard X Y sign)
  (cond
    ((or (< X 1)(> X Columns)(< Y 1)(> Y Rows)(and (not(equal? sign 'bomb))(not(equal? sign 'open))))(invalid-input yourBoard gameBoard))
    (else
     (Turn yourBoard gameBoard X Y sign))))

;----------------------------------------------------------------
;--plays a turn in the game. checks if player win/lose, player makes hes move, continue to next turn--
(define (Turn yourBoard gameBoard X Y sign)
  (printTheBoard (rest (compare-boards yourBoard gameBoard X Y Directions)))
  (newline)
  (cond
    ((and (equal? (checkMines yourBoard gameBoard) #t) (full? (rest (compare-boards yourBoard gameBoard X Y Directions)))) 'WINNER!!)
    ((and (not (equal? sign 'bomb))(equal? (check-location gameBoard X Y) 'b) ) 'LOSER!!)
    (else
     (checkKeys (compare-boards yourBoard gameBoard X Y Directions) gameBoard (GetX) (GetY) (GetSign)))))

;----------------------------------------------------------------
;--checks if all the flags on the YourBoard are mines on the GameBoard--
;checks that every row is correct
(define (checkMines yourBoard gameBoard)
  (cond
    ((empty? yourBoard) #t)
    ((equal? (checkMines-help (first yourBoard) (first gameBoard)) #f) #f)
    (else
     (checkMines (rest yourBoard) (rest gameBoard)))))
;checks that in a row all the flags are on mines
(define (checkMines-help yourBoard gameBoard)
  (cond
    ((empty? yourBoard) #t)
    ((not (equal? (first yourBoard) (first gameBoard))) #f)
    (else
     (checkMines-help (rest yourBoard) (rest gameBoard)))))

;----------------------------------------------------------------
;--if the input is invalid, prints the message, and gets new input--
(define (invalid-input yourBoard gameBoard)
  (print '(invalid input))
  (newline)
  (checkKeys yourBoard gameBoard (GetX) (GetY)(GetSign)))

;----------------------------------------------------------------
;--prints the message and gets the input--
;gets the X coordinate
(define (GetX)
  (print '(please enter X))
  (newline)
  (read))
;gets the Y coordinate
(define (GetY)
  (print '(please enter Y))
  (newline)
  (read))
;gets what the player wants to do
(define (GetSign)
  (print '(please enter what you want- bomb for flag, open to open))
  (newline)
  (read))
;gets the number of Columns for the game
(define (getCol)
  (print '(please enter the number of columns you would like))
  (newline)
  (read))
;gets the number of Rows for the game
(define (getRow)
  (print '(please enter the number of rows you would like))
  (newline)
  (read))
;gets the number of Mines for the game
(define (getMines)
  (print '(please enter the number of mines you would like))
  (newline)
  (read))

;----------------------------------------------------------------
;--gets the data for starting the game--
;gets all the data for the boards
(define Columns (getCol))
(define Rows (getRow))
(define Mines (getMines))

;creates the boards for the game
(define YourBoard (Costume Columns Rows))
(define GameBoard (place-numbers (place-bombs Mines (Costume Columns Rows) Columns Rows) Columns Rows))

;----------------------------------------------------------------
;---start the game---
(main)