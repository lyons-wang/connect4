open! CS17SetupGame;
open MyGameType; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
/* pickBest: (PlayerGame.state, int, PlayerGame.move, PlayerGame.move) => PlayerGame.move
       Input: a state of the game, s, an int representing the depth, d, the current
       move,curr, being checked against the best move, best (best is constantly changing if curr is better than best)
       Output: the next move in the game, calling upon nextMovehelp.
     */
  /* 
  evaluateMoves:
       (PlayerGame.state, int, list(PlayerGame.move), PlayerGame.move) =>
       PlayerGame.move
       Input: a state of the game, s, an int representing the depth, d, a list of
       player moves, moves, and the best move so far in the list, bestMove
       Output: calls upon pickBest to check whether the currMove or bestMove are better, based on the depth.
     Ex: Say Move(3) was the best move to make, and Move(2) is better than Move(1).
     OI: State(Ongoing(P1),originalBoard),4, [Move(2),Move(3)],Move(1))
      Idea: handed off to pickBest to go into depth 4 and evaluate which move
      is better.
      RI: State(Ongoing(P1),originalBoard), 4, [Move(3)], Move(2)
      RO: Move(3)
      Idea: Recursive Output is Original Output
    OO: Move(3)
     */
  /* nextMovehelp: (PlayerGame.state, int) => float
  Input: the state of the game, s, and the depth, d
  Output: a float, representing the lowest depth's estimateValue.
  OI: State(Ongoing(P1), originalBoard), 4
    RI: State(Ongoing(P1), originalBoard w/ Move(1)), 3
    RO: Calls evaluationMoveValues() to all legal moves of the current state.
  OO: Returns evaluationMoveValues() of the best state at the lowest depth (d == 0)
  */
  /* evaluateMoveValues: (PlayerGame.state, int, list(PlayerGame.move), float) => float
  Input: the state of the game, s, the depth, d, a list of possible legal moves, moves,
  and the estimateValue of the legal move before the tail was taken of that list of possible legal moves.
  Output: the best value of the deepest depth, comparing two legal moves together.

  If Move(1)'s bestValue is 5., Move(2)'s bestValue is 10., and Move(3)'s bestValue is 12.
  OI:
     s = State(Ongoing(P1), originalBoard), d = 3, moves = [Move(1), Move(2), Move(3)], bestValue = neg_infinity.
       Idea: We evaluate Move1 using nextMovehelp, compares its value with bestValue, and updates bestValue  if the new value is better.
       We then evaluate Move2 and Move3 similarly, ultimately returning the best value among all moves.

   Recursive Insight (RI):
     - Each move in `moves` is processed recursively:
       * Initial Input: s = State(Ongoing(P1), originalBoard), d = 3, moves = [Move(2), Move(3)], bestValue = 5.

   Recursive Output (RO): 12.

   Output Objective (OO): 12.   */
 /* program is efficient at depth 5, but becomes to exhaustive at any higher of 
    a number causing the program to take too long to run*/
 let depth = 5;
  let rec pickBest:
    (PlayerGame.state, int, PlayerGame.move, PlayerGame.move) =>
    PlayerGame.move =
    (s, d, curr, best) => {
      let bestVal = nextMovehelp(PlayerGame.nextState(s, best), d);
      let currVal = nextMovehelp(PlayerGame.nextState(s, curr), d);
      switch (PlayerGame.currentPlayer(s)) {
      | P1 =>
        if (bestVal > currVal) {
          best;
        } else {
          curr;
        }
      | P2 =>
        if (bestVal < currVal) {
          best;
        } else {
          curr;
        }
      };
    }
  and evaluateMoves:
    (PlayerGame.state, int, list(PlayerGame.move), PlayerGame.move) =>
    PlayerGame.move =
    (s, d, moves, bestMove) =>
      switch (moves) {
      | [] => bestMove
      | [currMove, ...restMoves] =>
        let newBest = pickBest(s, d, currMove, bestMove);
        evaluateMoves(s, d, restMoves, newBest);
      }
  and nextMovehelp: (PlayerGame.state, int) => float =
    (s, d) =>
      if (d == 0 || PlayerGame.legalMoves(s) == []) {
        PlayerGame.estimateValue(s);
      } else {
        let legalMoves = PlayerGame.legalMoves(s);
        let bestMove = List.hd(legalMoves); // Assume at least one legal move exists
        let bestEval =
          nextMovehelp(PlayerGame.nextState(s, bestMove), d - 1);
        let restMoves = List.tl(legalMoves);
        evaluateMoveValues(s, d - 1, restMoves, bestEval);
      }
  and evaluateMoveValues:
    (PlayerGame.state, int, list(PlayerGame.move), float) => float =
    (s, d, moves, bestValue) =>
      switch (moves) {
      | [] => bestValue
      | [currMove, ...restMoves] =>
        let currValue = nextMovehelp(PlayerGame.nextState(s, currMove), d); 
        let newBestValue =
          switch (PlayerGame.currentPlayer(s)) {
          | P1 =>
            if (bestValue > currValue) {
              bestValue;
            } else {
              currValue;
            }
          | P2 =>
            if (bestValue < currValue) {
              bestValue;
            } else {
              currValue;
            }
          };
        evaluateMoveValues(s, d, restMoves, newBestValue);
      };
      
/* nextMove: PlayerGame.state => PlayerGame.move
      Input: the current state of the game, s
      Output: the next move of the game, using evaluateMoves.
     */
  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let legalMoves = PlayerGame.legalMoves(s);
      if (legalMoves == []) {
        failwith("No available moves");
      } else {
        let bestMove = List.hd(legalMoves);
        evaluateMoves(s, depth, List.tl(legalMoves), bestMove);
      };
    };

  /* put your team name here! */
  let playerName = "∂";
  
};

module TestGame = MyConnect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 


