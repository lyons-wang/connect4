open! CS17SetupGame;
open MyGameType;

module Connect4 = {
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* otherPlayer: whichPlayer => whichPlayer
        Input:  player, one of the two players
        Outpt:  the opposite player */
  let otherPlayer: whichPlayer => whichPlayer = player =>
    switch (player) {
    | P1 => P2
    | P2 => P1
    };

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /* the state of the game: the position, status, anything else associated
     with the game at a given turn */
  type state =
    | State(status, list(list(option(whichPlayer))));

  /* describes a move that a player can make */
  type move =
    | Move(int);

  /* printing functions */
  /* stringOfPlayer: whichPlayer => string 
        Input:  player, the current player as type whichPlayer
        Output: the same player, but represented as a string */
  let stringOfPlayer: whichPlayer => string =
    player =>
      switch (player) {
      | P1 => "P1"
      | P2 => "P2"
      };

  /* currentPlayer: state => whichPlayer
        Input:  state, the current state
        Output: the current player */
let currentPlayer: state => whichPlayer =
    inState =>
      switch (inState) {
      | State(Draw, _) => failwith("")
      | State(Ongoing(p), _)
      | State(Win(p), _) => p
      };

  /* list(list(option(whichPlayer))) => list(list(option(whichPlayer)))
        Input:  mat, a matrix representation of the board state
        Output: The result of reflecting the matrix across its main diagonal.
                This is equivalent to switching rows to columns and columns to
                rows. */
  let rec transpose:
    list(list(option(whichPlayer))) => list(list(option(whichPlayer))) =
    mat =>
      switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0-dimensional")
      | [[head], ...butt] => [[head, ...List.map(List.hd, butt)]]
      | [[head, ...tail], ...butt] => [
          [head, ...List.map(List.hd, butt)],
          ...transpose([tail, ...List.map(List.tl, butt)]),
        ]
      };
  
  /* stringOfBoard: list(list(option(whichPlayer))) => string
        Input:  board, a matrix representation of the board state, stored as a 
                  list of columns
        Output: A string representation of the the board as it would appear to a
                  player in the physical version of Connect4, but with rows and 
                  columns switched */
  let rec stringOfBoard: list(list(option(whichPlayer))) => string =
    board =>
      switch (board) {
      | [[Some(x), ...ctl], ...btl] =>
        stringOfPlayer(x) ++ " | " ++ stringOfBoard([ctl, ...btl])
      | [[None, ...ctl], ...btl] =>
        "--" ++ " | " ++ stringOfBoard([ctl, ...btl])
      | [[], ...btl] => "\n" ++ stringOfBoard(btl)
      | [] => ""
      };

  /* stringOfState: state => string
        Input:  State(status, board), a state
        Output: a string representation of board to be shown to the player*/
  let stringOfState: state => string =
    (State(status, board)) =>
         "----------------------------------"
      ++ "\n"
      ++ stringOfBoard(transpose(board))
      ++ "----------------------------------"
      ++ "\n";

  /* stringOfMove: move => string
        Input:  Move(x), a move
        Output: the string conversion of x*/
  let stringOfMove: move => string = (Move(x)) => string_of_int(x);


  /* the state of the game when it begins */
  //the first digit of the input string happens to be the board height
  /* initialState: string => state
        Input:  s, a string
        Output: an empty board state*/
  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      /* boardBuildHeight: int => list(option(whichPlayer))
        Input:  bHeight, the desired board height
        Output: one column of length bHeight*/
      let rec boardBuildHeight: int => list(option(whichPlayer)) =
        bHeight =>
          switch (bHeight) {
          | 0 => []
          | _nonzero => [None, ...boardBuildHeight(bHeight - 1)]
          };
      /* boardBuilder: (list(option(whichPlayer)), int) => list(list(option(whichPlayer)))
        Input:  bColumn, a built column
                bWidth, the desired board with
        Output: a list of length bWidth consisting of bColumns*/
      let rec boardBuilder:
        (list(option(whichPlayer)), int) =>
        list(list(option(whichPlayer))) =
        (bColumn, bWidth) =>
          switch (bWidth) {
          | 0 => []
          | _nonzero => [
              bColumn,
              ...boardBuilder(bColumn, bWidth - 1),
            ]
          };
      State(
        Ongoing(P1),
        boardBuilder(boardBuildHeight(boardHeight), boardWidth),
      );
    };

  /* legal: (list(option(whichPlayer)), int) => list(move)
        Input:  row, the top row of the board
                i, a the number of the element being checked, initial input must
                   be 1
        Output: a list of moves that are legal (based on whether the ith element
                  of row was None or Some(_))*/
  let rec legal: (list(option(whichPlayer)), int) => list(move) =
    (row, i) =>
      switch (row) {
      | [] => []
      | [None, ...tl] => [Move(i), ...legal(tl, i + 1)]
      | [Some(_player), ...tl] => legal(tl, i + 1)
      };

  /* produces the list of legal moves at a state */
  /* legalMoves: state => list(move)
        Input:  cState, the current game stats
        Output: a list of moves that are legal */
  let legalMoves: state => list(move) =
    cState =>
      switch (cState) {
      | State(_cStatus, cBoard) =>
        legal(List.map(List.hd, cBoard), 1) // 1 means start of the 1st column (index)
      };

  /* returns the status of the game at the given state */
  /* gameStatus: state => status
        Input:  state, the current board state
        Output: the current status */
  let gameStatus: state => status =
    state =>
      switch (state) {
      | State(cStatus, board) => cStatus
      };

  /* c representing the kth row of the board */
  /* columnCheck: (list(option(whichPlayer)), whichPlayer) => bool
        Input:  column, a list of options representing a line on the board
                p, the current player
        Output: the current status */
  let rec columnCheck: (list(option(whichPlayer)), whichPlayer) => bool =
    (column, p) =>
      switch (column) {
      | [x] => failwith("columnCheck should not have inputs under 4")
      | [x, y] => failwith("columnCheck should not have inputs under 4")
      | [x, y, z] => false
      | [Some(a), Some(b), Some(c), Some(d), ...tl] =>
          if (a == p && b == p && c == p && d == p) {true}
          else {columnCheck(List.tl(column), p)}
      | _ => columnCheck(List.tl(column), p)
      };

  /* columnsToRow: (list(list(option(whichPlayer))),int) => list(option(whichPlayer))
         Input:  board, the current board state
                 k, an integer that represents the index of the desired row.
                    1 is the top row.
         Output: a list representing the kth row of the board */
  let rec columnsToRow:
    (list(list(option(whichPlayer))), int) => list(option(whichPlayer)) =
    (board, k) =>
      if (k == 1) {
        List.map(List.hd, board);
      } else {
        columnsToRow(List.map(List.tl, board), k - 1);
      };

  /* rowCheck: (list(list(option(whichPlayer))), int,whichPlayer) => bool
         Input:  board, the current board state
                 k, an integer that represents the index of the desired row.
                    1 is the top row.
                 p, the player who most recently moved
         Output: a boolean: true if the most recent player has a sequence of 4 in
                             the kth row
                           false otherwise */
  let rowCheck: (list(list(option(whichPlayer))), int, whichPlayer) => bool =
    (board, k, p) => columnCheck(columnsToRow(board, k), p);

  /* kElement: (list(option(whichPlayer)), int) => option(whichPlayer)
        Input:  column, a list of option(whichPlayer)
                k, a counter to determine which element to extract from column
                    k must be a positive integer
        Output: the kth element of column */
  let rec kElement: (list(option(whichPlayer)), int) => option(whichPlayer) =
    (column, k) =>
      switch (column) {
      | [hd, ...tl] =>
        if (k == 1) {
          hd;
        } else {
          kElement(tl, k - 1);
        }
      };

  /* mainDiagonal: (list(list(option(whichPlayer))), int) => list(option(whichPlayer))
        Input:  board, the current board state
                k, a counter to determine which element to extract from each list
                  *k must equal 1 in the original call*
        Output: the main diagonal of the input board. Because we are only concerned with getting 4 in a row */
  let rec mainDiagonal:
    (list(list(option(whichPlayer))), int) => list(option(whichPlayer)) =
    (board, k) =>
      switch (board) {
      | [] => []
      | [c1, ...ctl] =>
        if (List.length(c1) >= k) {
          [kElement(c1, k), ...mainDiagonal(ctl, k + 1)];
        } else {
          [];
        }
      };

  /* diagonaldownhelp: list(list(option(whichPlayer))) => list(list(option(whichPlayer)))
        Input:  miniboard, the current board state
        Output: a list of the diagonals to the left/down from the main diagonal
                  with length 4 or greater. (a win won't be found in less than 4 moves)*/
let rec diagonaldownhelp:
    list(list(option(whichPlayer))) => list(list(option(whichPlayer))) =
    miniboard =>
      switch (miniboard) {
      | [[head], ..._] => []
      | [[head, ...tail], ..._] =>
        let taillist = List.map(List.tl, miniboard);
        if(List.length(List.hd(taillist)) < 4) {[]} else {
        [mainDiagonal(taillist, 1), ...diagonaldownhelp(taillist)]};
      };

  /* diagonalrighthelp: list(list(option(whichPlayer))) => list(list(option(whichPlayer)))
        Input:  miniboard, the current board state
        Output: a list of the diagonals to the right/up from the main diagonal
                  with length 4 or greater. (a win won't be found in less than 4 moves)*/

  let rec diagonalrighthelp:
    list(list(option(whichPlayer))) => list(list(option(whichPlayer))) =
    miniboard =>
      switch (miniboard) {
      | [] => []
      | [c1, ...ctl] => 
      if (List.length(ctl) < 4) {[]} else {[mainDiagonal(ctl, 1), ...diagonalrighthelp(ctl)]}
      };



  /* dBoardResults: (list(list(option(whichPlayer))), whichPlayer) => bool
        Input:  board, the board state with the most recent move added
                cPlayer, the player who made the most recent move
                callnum, the number of recursive calls that dBoardResults is 
                          allowed to make (value should always be 1). This allows 
                          the procedure to avoid running List.rev unless 
                          necessary (reducing run time)
        Output: a bool, true if the most recent player now has a winning state
                          (4 in a row)
                        false otherwise */  
  let rec dBoardResults: (list(list(option(whichPlayer))), whichPlayer, int) => bool =
  (board, cPlayer, callnum) => {
        /* foldHelp: list(list(option(whichPlayer))) => bool
        Input:  dhelper, a list of list of option(P1/P2/None) that is the output
                  of one of the helper procedures for diagonal wins
        Output: a bool, true if there is a list in dhelper with a sequence of 4,
                        false otherwise*/
        //cannot test because it is nested in function
          let foldHelp: list(list(option(whichPlayer))) => bool = (dhelper) =>
            List.fold_right((x, y) => columnCheck(x, cPlayer) || y, dhelper, false);
    foldHelp([mainDiagonal(board, 1)]) ||
    foldHelp(diagonalrighthelp(board)) ||
    foldHelp(diagonaldownhelp(board)) ||
    switch(callnum) {
      | 1 => dBoardResults(List.rev(board), cPlayer, callnum - 1)
      | 0 => false
      | _ => failwith("dBoardResults third input must be '1'")
    }; 
  }

  /* addMoveToCol: (list(option(whichPlayer)), whichPlayer) => list(option(whichPlayer))
        Input:  column, the column that cPlayer wants to move in
                cPlayer, the current player
        Output: column, updated with cPlayer's move and failing if the column 
                  is already full*/
  let rec addMoveToCol:
    (list(option(whichPlayer)), whichPlayer) => list(option(whichPlayer)) =
    (column, cPlayer) =>
      switch (column) {
      | [None, Some(x), ...tl] => [Some(cPlayer), Some(x), ...tl]
      | [None] => [Some(cPlayer)]
      | [Some(x), ... tl] => failwith("addMoveToCol -- column is full")
      | [x, ...tl] => [x, ...addMoveToCol(tl, cPlayer)]
      };

  /* newBoard: (list(list(option(whichPlayer))), list(list(option(whichPlayer))), int, whichPlayer) =>
               (list(list(option(whichPlayer))), list(option(whichPlayer)))
        Input:  cBoard, the current board state
                nBoard, the new board state (input should always be empty list)
                k, the number of the column being moved into by cPlayer
                cPlayer, the current player
        Output: an updated board state that represents cPlayer's most recent 
                move in the kth column*/
  let rec newBoard:
    (list(list(option(whichPlayer))), list(list(option(whichPlayer))), int, whichPlayer) =>
    (list(list(option(whichPlayer))), list(option(whichPlayer))) =
    (cBoard, nBoard, k, cPlayer) => 
    if (k > 7 || k < 1) 
      {failwith("input must be an integer between 1 and 7")}
    else
      switch (cBoard, k) {
      | ([c1, ...ctl], 1) =>
        let newc1 = addMoveToCol(c1, cPlayer);
        (List.append(nBoard, [newc1, ...ctl]), newc1);
      | ([c1, ...ctl], num) =>
        newBoard(ctl, nBoard @ [c1], num - 1, cPlayer)
      };

  /* findRowIndex: list(option(whichPlayer)) => int
        Input:  column, the column associated with the most recent move.
                  this column will have at least one non-None element
        Output: the row index of the most recent move  */
  let rec findRowIndex: list(option(whichPlayer)) => int =
    (column) =>
      switch (column) {
      | [Some(_), ...tl] => 1
      | _ => findRowIndex(List.tl(column)) + 1
      };

  /* isWin:(list(list(option(whichPlayer))), whichPlayer, list(option(whichPlayer))) => status               (list(list(option(whichPlayer))), list(option(whichPlayer)))
        Input:  board, the board state including the most recent move
                cPlayer, the current player
                col, the column that the cPlayer moved in
        Output: a status, Win(cPlayer) if there is a series of 4 on the board
                  Ongoing(the other player) otherwise*/
  let isWin:(list(list(option(whichPlayer))), whichPlayer, list(option(whichPlayer))) => status
    = (board, cPlayer, col) => 
      if (columnCheck(col, cPlayer) ||
          rowCheck(board, findRowIndex(col), cPlayer) ||           
          dBoardResults(board, cPlayer, 1))
          {Win(cPlayer)}
      else
          {Ongoing(otherPlayer(cPlayer))}

  /* given a state and a legal move, yields the next state */
  /* nextState: (state, move) => state
        Input:  cState, the current state
                move, a move made by the current player
        Output: an updated state based upon the addition of move*/
  let emptyBoard = [];
  let nextState: (state, move) => state =
    (cState, move) => {
      switch (cState, move) {
      | (State(Win(cPlayer), board), mv) => State(Win(cPlayer), board)
      | (State(Draw, board), mv) => State(Draw, board)
      | (State(Ongoing(cPlayer), board), Move(int)) => {
          let (newB, col) = newBoard(board, emptyBoard, int, cPlayer);
          if (legalMoves(State(Ongoing(cPlayer), newB)) == []) 
            {State(Draw, newB)} 
          else
          {State(isWin(newB, cPlayer, col), newB)}
      }
    }
  }
  /* moveOfString:(string, state) => move
        Input:  input, a string, taken from the REPL
                state, the current state of the board
        Output: either a failwith message indicating an invalid input or the 
                  move associated with the integer input */
  /* for transforming human player input into internal representation of move */
  let moveOfString: (string, state) => move = (input, state) =>
  { let inputString = try(int_of_string(input)) {
    | _ => failwith("input must be an integer between 1 and 7")}
    if (List.mem(Move(inputString), legalMoves(state))) 
      {Move(inputString)}
    else
      {failwith("input must be between 1 and 7 and in an open column")}}

/* columnvalue: list(option(whichPlayer)) => float
      Input: a column of an option of whichPlayer, column.
      Output: the value of that column, based on 3 in a rows, 2 in a rows, and 1 in
        a row, with P1 being greater than 0 and P2 being less than 0.*/
  let rec columnvalue: list(option(whichPlayer)) => float =
    column =>
      switch (column) {
      | [] => 0.
      | [Some(a)] =>
        if (a == P1) {
          1. +. columnvalue(List.tl(column));
        } else if (a == P2) {
          (-1.) +. columnvalue(List.tl(column));
        } else {
          columnvalue(List.tl(column));
        }
      | [Some(a), Some(b)] =>
        if (a == b && a == P1) {
          2. +. columnvalue(List.tl(column));
        } else if (a == b && a == P2) {
          (-2.) +. columnvalue(List.tl(column));
        } else if (a == P1) {
          1. +. columnvalue(List.tl(column));
        } else if (a == P2) {
          (-1.) +. columnvalue(List.tl(column));
        } else {
          columnvalue(List.tl(column));
        }
      | [Some(a), Some(b), Some(c)] =>
        if (a == b && b == c && a == P1) {
          3. +. columnvalue(List.tl(column));
        } else if (a == b && b == c && a == P2) {
          (-3.) +. columnvalue(List.tl(column));
        } else if (a == b && a == P1) {
          2. +. columnvalue(List.tl(column));
        } else if (a == b && a == P2) {
          (-2.) +. columnvalue(List.tl(column));
        } else if (a == P1) {
          1. +. columnvalue(List.tl(column));
        } else if (a == P2) {
          (-1.) +. columnvalue(List.tl(column));
        } else {
          columnvalue(List.tl(column));
        }
      | [Some(a), Some(b), Some(c), Some(d), ...tl] =>
        if (a == b && b == c && c == d && a == P1) {
          infinity;
        } else if (a == b && b == c && c == d && a == P2) {
          neg_infinity;
        } else if (a == b && b == c && a == P1) {
          3. +. columnvalue(List.tl(column));
        } else if (a == b && b == c && a == P2) {
          (-3.) +. columnvalue(List.tl(column));
        } else if (a == b && a == P1) {
          2. +. columnvalue(List.tl(column));
        } else if (a == b && a == P2) {
          (-2.) +. columnvalue(List.tl(column));
        } else if (a == P1) {
          1. +. columnvalue(List.tl(column));
        } else if (a == P2) {
          (-1.) +. columnvalue(List.tl(column));
        } else {
          columnvalue(List.tl(column));
        }
      | _ => columnvalue(List.tl(column))
      };

  /* anyvalue: list(list(option(whichPlayer))) => float
        Input:  board, a list of columns
        Output: the sum of the columnValue float for each column in board */
  let rec anyvalue: list(list(option(whichPlayer))) => float =
    board =>
      switch (board) {
      | [hd] => columnvalue(hd)
      | [hd, ...tl] => (columnvalue(hd) +. anyvalue(tl))
      };

  /* diagonalBoard:
      list(list(option(whichPlayer))) => list(list(option(whichPlayer)))
      Input: a list of lists of options of P1 or P2, board
      Output: every single possible diagonal win represented in columns instead of
      diagonals. */
  let diagonalBoard:
    list(list(option(whichPlayer))) => list(list(option(whichPlayer))) =
    board => {
      let reversedboard = List.rev(board);
      [mainDiagonal(board, 1)]
      @ diagonalrighthelp(board)
      @ diagonaldownhelp(board)
      @ [mainDiagonal(reversedboard, 1)]
      @ diagonalrighthelp(reversedboard)
      @ diagonaldownhelp(reversedboard);
    };

  /* estimates the value of a given state (static evaluation) */
  /* estimateValue: state => float
        Input: the state that value is going to be estimated, current
        Output: If P1 wins, infinity, if P2 wins, -infinity, else evaluate board based
        on how many possible 3 in a rows, 2 in a rows, or 1 in a rows there are. */
  let estimateValue: state => float =
    current =>
      switch (current) {
      | State(Win(P1), _) => infinity
      | State(Win(P2), _) => neg_infinity
      | State(Draw, _) => -10000.
      | State(_, board) =>
        anyvalue(board)
        +. anyvalue(transpose(board))
        +. anyvalue(diagonalBoard(board))
      };
};

module MyGame: Game = Connect4;
open Connect4;

////////////////////CHECK EXPECTS////////////////
let emptyBoard: list(list(option(whichPlayer))) = [
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None],
[None,     None,     None,     None,     None]]

//P1 1 move from winning
let board1: list(list(option(whichPlayer))) = [
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P1), Some(P1), Some(P1), Some(P2)],
[None,     None,     None,     None,     Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P2), Some(P2), Some(P1), Some(P2)],
[None,     None,     Some(P2), Some(P2), Some(P1)],
[Some(P1), Some(P1), Some(P2), Some(P2), Some(P2)]]

let transposedBoard1: list(list(option(whichPlayer))) = 
[[None,     None,     None,     None,     None,     None,     Some(P1)],
 [None,     Some(P1), None,     None,     Some(P2), None,     Some(P1)],
 [Some(P1), Some(P1), None,     Some(P1), Some(P2), Some(P2), Some(P2)],
 [Some(P2), Some(P1), None,     Some(P2), Some(P1), Some(P2), Some(P2)],
 [Some(P1), Some(P2), Some(P2), Some(P1), Some(P2), Some(P1), Some(P2)]]

let badStringBoard1: string =
"-- | -- | P1 | P2 | P1 | \n-- | P1 | P1 | P1 | P2 | \n-- | -- | -- | -- | P2 | \n-- | -- | P1 | P2 | P1 | \n-- | P2 | P2 | P1 | P2 | \n-- | -- | P2 | P2 | P1 | \nP1 | P1 | P2 | P2 | P2 | \n"

let stringBoard1: string =
"----------------------------------\n-- | -- | -- | -- | -- | -- | P1 | \n-- | P1 | -- | -- | P2 | -- | P1 | \nP1 | P1 | -- | P1 | P2 | P2 | P2 | \nP2 | P1 | -- | P2 | P1 | P2 | P2 | \nP1 | P2 | P2 | P1 | P2 | P1 | P2 | \n----------------------------------\n"

let state1: state = State(Ongoing(P1), board1)

let board1Row1 = [None, None, None, None, None, None, Some(P1)]
let board1Row2 = [None, Some(P1), None, None, Some(P2), None, Some(P1)]
let board1Row5 = [Some(P1), Some(P2), Some(P2), Some(P1), Some(P2), Some(P1), Some(P2)]
let board1MainDiagonal = [None, Some(P1), None, Some(P2), Some(P2)]
let board1Column5 = [None, Some(P2), Some(P2), Some(P1), Some(P2)]

let board1LegalMoves: list(move) = [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6)]

//draw state
let fullBoard: list(list(option(whichPlayer))) = [
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)]]
//nextState will be a draw
let almostFullBoard: list(list(option(whichPlayer))) = [
  [None,     Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)],
  [Some(P2), Some(P2), Some(P2), Some(P1), Some(P1)],
  [Some(P1), Some(P1), Some(P1), Some(P2), Some(P2)]]

//P1 win on column
let board2: list(list(option(whichPlayer))) = [
[None,     None,     Some(P1), Some(P2), Some(P1)],
[Some(P1), Some(P1), Some(P1), Some(P1), Some(P2)],
[None,     None,     None,     None,     Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P2), Some(P2), Some(P1), Some(P2)],
[None,     None,     Some(P2), Some(P2), Some(P1)],
[Some(P1), Some(P1), Some(P2), Some(P2), Some(P2)]]

let board2Column2 = [Some(P1), Some(P1), Some(P1), Some(P1), Some(P2)]

let state2: state = State(Win(P1), board2)

//P2 win in row
let board3: list(list(option(whichPlayer))) = [
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P1), Some(P1), Some(P1), Some(P2)],
[None,     None,     None,     None,     Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P2)],
[None,     Some(P2), Some(P2), Some(P1), Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P2)],
[Some(P1), Some(P1), Some(P2), Some(P2), Some(P1)]]

//P1 win on diagonal
let board4: list(list(option(whichPlayer))) = [
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P1), Some(P1), Some(P1), Some(P2)],
[None,     None,     None,     None,     Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P1)],
[None,     Some(P2), Some(P2), Some(P1), Some(P2)],
[None,     None,     Some(P1), Some(P2), Some(P1)],
[Some(P1), Some(P1), Some(P2), Some(P2), Some(P2)]]

let board4Column6 = [None, None, Some(P1), Some(P2), Some(P1)]


checkExpect(otherPlayer(P1),P2,"otherPlayer")
checkExpect(stringOfPlayer(P1),"P1","stringOfPlayer")
checkExpect(currentPlayer(state1), P1, "currentPlayer ongoing")
checkExpect(currentPlayer(state2), P1, "currentPlayer win")
checkExpect(transpose(board1), transposedBoard1, "transpose")
checkExpect(stringOfBoard(board1), badStringBoard1, "stringOfBoard, board1")
checkExpect(stringOfState(state1), stringBoard1, "stringOfState, state1")
checkExpect(stringOfMove(Move(3)), "3", "stringOfMove")
checkExpect(initialState("5 7"), State(Ongoing(P1),emptyBoard), "initialState")
//cannot check boardBuildHeight or boardBuilder because they are inside a function
checkExpect(legal(board1Row1, 1), board1LegalMoves, "legal")
checkExpect(legalMoves(state1), board1LegalMoves, "legalMoves")
checkExpect(legalMoves(State(Ongoing(P1), fullBoard)), [], "legalMoves, fullBoard")
checkExpect(gameStatus(state1), Ongoing(P1), "gameStatus, state1")
checkExpect(gameStatus(state2), Win(P1), "gameStatus, state2")

let column1 = [None, Some(P1), Some(P1), Some(P1), Some(P1)]
let column2 = [Some(P1), Some(P2), Some(P1), Some(P1), Some(P1), Some(P1)]
let column3 = [None, Some(P1), Some(P2), Some(P1), Some(P1)]
let tinyBoard = [
  [Some(P1), Some(P2), Some(P2)],
  [None,     None,     Some(P1)],
  [None,     Some(P1), Some(P2)]]

checkExpect(columnCheck(column1, P1), true, "columnCheck, column1 | P1")
checkExpect(columnCheck(column1, P2), false, "columnCheck, column1 | P2")
checkExpect(columnCheck(column2, P1), true, "columnCheck, column2 | P1")
checkExpect(columnCheck(column3, P1), false, "columnCheck, column3 | P1")
checkExpect(columnCheck(
            [Some(P1), Some(P1), Some(P1)], P1), false, "columnCheck, 3 elements | P1")
checkExpect(columnsToRow(board1, 1), board1Row1, "columnsToRow, board1 | 1")
checkExpect(columnsToRow(board1, 2), board1Row2, "columnsToRow, board1 | 2")
checkExpect(columnsToRow(board1, 5), board1Row5, "columnsToRow, board1 | 5")
checkExpect(rowCheck(board3, 5, P2), true, "rowCheck, board3 | 5 | P2")
checkExpect(rowCheck(board3, 4, P2), false, "rowCheck, board3 | 4 | P2")
checkExpect(kElement(column3, 1), None, "kElement, column 3 | 1")
checkExpect(kElement(column3, 4), Some(P1), "kElement, column 3 | 4")
checkExpect(mainDiagonal(board1, 1), board1MainDiagonal, "mainDiagonal, board1 | 1")
checkExpect(mainDiagonal([], 1), [], "mainDiagonal, empty | 1")
checkExpect(mainDiagonal(tinyBoard, 1), [Some(P1), None, Some(P2)], "mainDiagonal, tinyBoard | 1")
checkExpect(diagonaldownhelp(board1), [[None, Some(P1), None, Some(P1)]], "diagonaldownhelp, board1")
checkExpect(diagonalrighthelp(board1),
           [[None, None, Some(P1), Some(P1), Some(P1)],
            [None, None, Some(P2), Some(P2), Some(P2)],
            [None, Some(P2), Some(P2), Some(P2)]],
            "diagonalrighthelp, board1")
checkExpect(diagonaldownhelp(tinyBoard), [], "diagonaldownhelp, tinyBoard")
checkExpect(diagonalrighthelp(tinyBoard), [], "diagonalrighthelp, tinyBoard")
checkExpect(dBoardResults(board1, P1, 1), false, "dBoardResults, board1 | P1 | 1")
checkExpect(dBoardResults(board4, P1, 1), true, "dBoardResults, board4 | P1 | 1")
checkExpect(dBoardResults(board3, P2, 1), false, "dBoardResults, board3 | P2 | 1")
checkExpect(addMoveToCol(column1, P1), 
           [Some(P1), Some(P1), Some(P1), Some(P1), Some(P1)],
           "addMoveToCol, column1 | P1")
checkError(() => addMoveToCol(column2, P2), "addMoveToCol -- column is full")
checkExpect(newBoard(board1, [], 2, P1), (board2, board2Column2), "newBoard, board1 | [] | 2 | P1")
checkExpect(findRowIndex(column1), 2, "findRowIndex, column1")
checkExpect(findRowIndex(column2), 1, "findRowIndex, column2")
checkExpect(isWin(board2, P1, board2Column2), Win(P1), "isWin, board2 | P1 | board2Column2")
checkExpect(isWin(board1, P2, board1Column5),
           Ongoing(P1),
           "isWin, board1 | P2 | board1Column5")
checkExpect(isWin(board4, P1, board4Column6),
           Win(P1), 
           "isWin, board4 | P1 | board4Column6")
checkExpect(nextState(state1, Move(2)), state2, "nextState, state1 | Move(2)")
checkExpect(nextState(State(Ongoing(P1), almostFullBoard), Move(1)),
           State(Draw, fullBoard),
           "nextState, State(Ongoing(P1), almostFullBoard) | Move(1)")
checkExpect(nextState(State(Ongoing(P1),emptyBoard), Move(1)),
           State(Ongoing(P2),
           [[None,     None,     None,     None, Some(P1)],
            [None,     None,     None,     None,     None],
            [None,     None,     None,     None,     None],
            [None,     None,     None,     None,     None],
            [None,     None,     None,     None,     None],
            [None,     None,     None,     None,     None],
            [None,     None,     None,     None,     None]]), 
            "nextState, State(Ongoing(P1), emptyBoard) | Move(1)")
checkExpect(moveOfString("5", state1), Move(5), "moveOfString, 5 | state1")
checkError(() => moveOfString("BAD", state1), "input must be an integer between 1 and 7")
checkError(() => moveOfString("10", state1), "input must be between 1 and 7 and in an open column")
checkError(() => moveOfString("7", state1), "input must be between 1 and 7 and in an open column")
//no test cases for estimate value needed
checkExpect(diagonalBoard(board1),
           [[None, Some(P1), None, Some(P2), Some(P2)],
            [None, None, Some(P1), Some(P1), Some(P1)],
            [None, None, Some(P2), Some(P2), Some(P2)],
            [None, Some(P2), Some(P2), Some(P2)], 
            [None, Some(P1), None, Some(P1)],
            [Some(P1), None, Some(P2), Some(P2), Some(P2)],
            [None, Some(P2), Some(P1), None, Some(P2)],
            [None, None, None, Some(P1), Some(P1)], 
            [None, None, Some(P1), Some(P2)],
            [Some(P1), Some(P2), Some(P1), Some(P1)]],
           "diagonalBoard, board1")
checkExpect(diagonalBoard(tinyBoard),
           [[Some(P1), None, Some(P2)], 
            [None, None, Some(P2)]],
           "diagonalBoard, tinyBoard")


// Need to check for horizontal, vertical and diagonal wins… how to orient matrix representation of board in each of these ways??
// What if we find ways to represent the vertical and diagonal lines as columns so we only need one function to check horizontal, vertical AND diagonal wins
// Transpose the vertical to horizontal
// Make pseudo-columns from the diagonals (will vary in size)
// Will run fxn on current board state to find diagonals, and then will reverse the list(columns) and run fxn again to find ways that n in a row could be made diagonally

// **if n is the number of consecutive spaces required to win → in our case it’s 4.
// **function will need an input that accounts for who is playing so that we are only checking squares occupied by that player
// when playing in a column, you check n-1 elements in prepending and n-1 elements following the first “None” element (which will then become Some(whichPlayer))

