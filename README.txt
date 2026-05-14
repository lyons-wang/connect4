BO1901849 - Evan diVittorio
B01891229 - Lyons Wang

• instructions for use, describing how a user would interact with your program 
(how would someone play your game against a friend? against the AI?):
    To use our program, the user must modify Referee.re. At the bottom of the file,
    there are two modules titled HumanvsAI and HumanVsHuman. The first player is always
    a human. To play, the user must comment out (highlight and then press 
    'ctrl//cmd' + '/') the version that they do not wish to play. Next they must run
    'npm run build' and 'node Referee.bs.js'. The REPL will appear in the terminal with
    an empty connect4 board. The user will then input an integer beetween 1 and 7,
    to place their token. The columns are ordered left to right in increasing order,
    starting at one. If playing against AI, they player will wait for the REPL to 
    prompt their next move (allowing AI to go). If playing against a human, the
    other player must now go. Play continues according to the rules of Connect4 
    until someone wins, or the game is a draw.



• an overview of how your program functions, including how all of the pieces fit 
together:
  Representations:
    players as P1 or P2; 
    the board as a list(list(option(P1//P2))) or, in context, as a list
        of columns;
    a status as Win(P1//P2) or Draw or Ongoing(P1//P2);
    a state as State(status, board)

  Game module function implementations:
   stringOfPlayer:
    Outputs a string of the player. very simple.
   stringOfState:
    The board associated with the state must be transposed to represent the rows
    of the board as rows of the matrix to allow for efficient conversion to a 
    string. This is done with 'transpose'. stringOfBoard calls the transposed 
    version of the column board representation and converts to a string. 
    StringOfState simply adds a top and bottom to the board created by 
    stringOfBoard.
   stringOfMove:
    Outputs a string representing the parametrized value of Move(x)
   initialState:
    This function initializes an empty board based on the dimensions provided 
    in the input string. It uses helper functions like parseBoardDims to 
    extract the dimensions, getBoardHeight and getBoardWidth to retrieve the 
    height and width, and boardBuilder to construct the board. Internally, 
    boardBuildHeight creates a single column with the specified height filled 
    with None values, representing empty slots. boardBuilder then replicates 
    this column for the desired number of columns to form the full matrix. The 
    resulting board is wrapped in a State object with the status set to 
    Ongoing and the starting player as P1.
   legalMoves:
    This function identifies all valid moves by examining the top row of the 
    board. It uses List.map to extract the topmost element (List.hd) of each 
    column and passes this row to the recursive helper function legal. legal 
    traverses the row, checking if each element is None (indicating the column 
    is not full). If the column is valid, it adds a Move(x) corresponding to 
    the column index to the list of legal moves. This function ensures that 
    only playable columns are included.
   gameStatus:
    Extracts and returns the status (Win, Draw, or Ongoing) from the current 
    state object. The helper logic embedded in the state ensures that the 
    status is always up to date based on moves applied through nextState.
   nextState:
    Updates the game state after a move. It calls the helper function newBoard, 
    which modifies the board by applying the move to the appropriate column 
    using addMoveToCol. If the column is full, addMoveToCol raises an error; 
    otherwise, it inserts the current player's token (Some(P1) or Some(P2)) in 
    the first available slot. Once the board is updated, nextState checks for 
    win conditions using isWin, which evaluates rows, columns, and diagonals 
    for four consecutive tokens. Within isWin, the helper dBoardResults 
    implements a curried version of fold_right and the helper function 
    'columnCheck' to assess the outputs of the  diagonal helper functions
    (diagonalrighthelp//diagonaldownhelp//mainDiagonal) and short circuit itself
    to minimize (at least a little) the runtime of the function. If no legal 
    moves remain, the status is set to Draw. Finally, it switches the current 
    player using otherPlayer. 
   currentPlayer:
    Determines the player whose turn it is by checking the state’s status. If 
    the status is Ongoing, it extracts the associated player. If the status is 
    Win or Draw, it raises an error when called, because the game has already 
    concluded. These cases should never be evaluated, but the cases exist to allow
    for exhaustive switch cases.
   moveOfString:
    Converts a user-provided string into a Move(x) type. First, it attempts to 
    parse the string into an integer using int_of_string. If the conversion 
    fails, it raises an error with a helpful message. Next, it verifies that 
    the resulting move is within the valid range (1–7 for Connect 4) and checks 
    if the move is included in the legal moves list, generated by legalMoves. 
    If the move is invalid or the column is full, it raises an error. Otherwise, 
    it returns the Move(x) object.
   estimateValue: estimateValue: when estimateValue is called from 
    nextMovehelp, we want to check the state and return a float. If the 
    state shows that P1 has won, the result would be infinity, as P1 wants 
    to maximize and it has completely maximized, but if the state shows that 
    P2 has won, the result would be neg_infinity, as P2 wants to minimize 
    and it has completely minimized. If the state shows that neither player 
    has won yet, we can take the values of all possible wins from columns, 
    rows, and diagonals. For this, we invocate anyvalue on the entire board 
    (columns), anyvalue on the transpose of the board (rows become columns), 
    and anyvalue on a procedure diagonalboard, which makes all diagonals 
    into columns. Anyvalue would keep innovating columnvalue on each value 
    of the column. columnvalue looks into a column, and assigns infinity or 
    negative infinity based on a group of 4, 1. or -1. if there is just 1 in 
    a row, 2. or -2. if there are 2 in a row, and 3. or -3. if there are 
    three in a row. We sum all of the columns, rows, and diagonals, and we 
    arrive at a final result of a float, that can be compared with all other 
    possible states in order to find the move most favorable.

  The AI Player: 
   When nextMove is called on a state, the AI will look at all 
     the possible legal moves at that state, and call a helper procedure 
     evaluateMoves. That procedure takes in that current state, the depth we want
     to go, and a list of moves, and the current move. It will take the best 
     move of all of those moves based on a helper procedure pickBest, which is 
     recursive. pickBest compares estimateValues from the next state if that 
     move is done, and calls upon nextMoveHelp to go down the depth. At the end, 
     pickBest will compare the current best value with the current value, and 
     choose the best option based on the player (P1 wants to maximize, P2 wants 
     to minimize). When nextMovehelp is called, it will take in the next state, 
     and with those legalMoves, take the head, keep invoking nextMovehelp 
     until the certain depth is reached or there are no more legal moves left 
     to do. Then, it will put that into evaluateMoveValues, which will take in 
     the current state, the depth, a list of moves, and the float (estimateValue) 
     of the previous head that was in the list of moves. It will keep comparing 
     estimateValues with each other, so that if the current state is with P1, we 
     would maximize, but if the current state is with P2, we would minimize. At 
     the end, nextMove would produce one move that is based on estimateValue of 
     numerous different states at all possible moves at a certain depth.

  Fitting together:
        The game progresses via state transitions handled by nextState. Players 
        (human or AI) input moves using moveOfString. The AI (nextMove) selects 
        the best move based on the current state, and a depth limit. At each 
        step, win/draw conditions are checked with isWin, dBoardResults, and 
        row/column/diagonal checks. The static evaluator (estimateValue) informs
        the AI how 'good' each state is to allow for decision-making.



• a description of any possible bugs or problems with your program

    The most likely shortcoming of our program lies in the implementation of 
    estimateValue. The function does not explicitly account for strategic 
    assumptions that a human might make such as central columns being better than
    those on the edge. 



• a list of the people with whom you collaborated
  
    N/A



• a description of any extra features you chose to implement
    
    As mentioned above, there are a few instances of curried functions because
    they can be more efficient. The diagonal helper functions all omit diagonals
    of length less than 4 because there could not be a match in a list of length
    under 4. Beyond that and the short-circuiting feature of dBoardHelp there is 
    nothing else 'extra' that we have implemented.
