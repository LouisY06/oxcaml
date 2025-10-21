// Speed Card Game Logic - JavaScript implementation mirroring OCaml structure
// This mirrors the OCaml Hw2_speed_logic module for consistency

class Card {
  constructor(suit, rank) {
    this.suit = suit;
    this.rank = rank;
  }

  static rankValue(rank) {
    const values = {
      'Ace': 1, 'Two': 2, 'Three': 3, 'Four': 4, 'Five': 5, 'Six': 6,
      'Seven': 7, 'Eight': 8, 'Nine': 9, 'Ten': 10, 'Jack': 11,
      'Queen': 12, 'King': 13
    };
    return values[rank];
  }

  static canPlayOn(card, pileCard) {
    if (!pileCard) return false;
    
    const cardVal = Card.rankValue(card.rank);
    const pileVal = Card.rankValue(pileCard.rank);
    
    // Ace is wild - can play on King or 2
    if (card.rank === 'Ace') {
      return pileVal === 13 || pileVal === 2; // King or 2
    }
    if (pileCard.rank === 'Ace') {
      return cardVal === 13 || cardVal === 2; // King or 2
    }
    
    // Normal ±1 rule
    return cardVal === pileVal + 1 || cardVal === pileVal - 1;
  }

  toString() {
    const suitSymbols = { 'Hearts': '♥', 'Diamonds': '♦', 'Clubs': '♣', 'Spades': '♠' };
    const rankSymbols = { 
      'Ace': 'A', 'Two': '2', 'Three': '3', 'Four': '4', 'Five': '5',
      'Six': '6', 'Seven': '7', 'Eight': '8', 'Nine': '9', 'Ten': '10',
      'Jack': 'J', 'Queen': 'Q', 'King': 'K'
    };
    return rankSymbols[this.rank] + suitSymbols[this.suit];
  }

  equals(other) {
    return this.suit === other.suit && this.rank === other.rank;
  }
}

class Move {
  static PlayCard(card, pile) {
    return { type: 'PlayCard', card, pile };
  }
  
  static DrawCards() {
    return { type: 'DrawCards' };
  }
}

class GameState {
  constructor() {
    this.player1Hand = [];
    this.player2Hand = [];
    this.player1Stock = [];
    this.player2Stock = [];
    this.pile1 = null;
    this.pile2 = null;
    this.currentPlayer = 'Player1';
    this.gameOver = false;
    this.winner = null;
  }

  static create() {
    const gameState = new GameState();
    
    // Create deck
    const suits = ['Hearts', 'Diamonds', 'Clubs', 'Spades'];
    const ranks = ['Ace', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King'];
    
    const deck = [];
    for (const suit of suits) {
      for (const rank of ranks) {
        deck.push(new Card(suit, rank));
      }
    }
    
    // Shuffle deck
    for (let i = deck.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [deck[i], deck[j]] = [deck[j], deck[i]];
    }
    
    // Deal cards
    gameState.player1Hand = deck.slice(0, 5);
    gameState.player2Hand = deck.slice(5, 10);
    gameState.player1Stock = deck.slice(10, 25);
    gameState.player2Stock = deck.slice(25, 40);
    gameState.pile1 = deck[40];
    gameState.pile2 = deck[41];
    
    return gameState;
  }

  makeMove(move) {
    if (this.gameOver) {
      return { success: false, error: 'Game is over' };
    }

    switch (move.type) {
      case 'PlayCard':
        return this.playCard(move.card, move.pile);
      case 'DrawCards':
        return this.drawCards();
      default:
        return { success: false, error: 'Invalid move' };
    }
  }

  playCard(card, pileIndex) {
    const pile = pileIndex === 0 ? this.pile1 : this.pile2;
    
    if (!pile) {
      return { success: false, error: 'Empty pile' };
    }
    
    if (!Card.canPlayOn(card, pile)) {
      return { success: false, error: 'Invalid play' };
    }
    
    // Remove card from hand
    const hand = this.currentPlayer === 'Player1' ? this.player1Hand : this.player2Hand;
    const cardIndex = hand.findIndex(c => c.equals(card));
    if (cardIndex === -1) {
      return { success: false, error: 'Card not in hand' };
    }
    
    hand.splice(cardIndex, 1);
    
    // Place card on pile
    if (pileIndex === 0) {
      this.pile1 = card;
    } else {
      this.pile2 = card;
    }
    
    // Auto-draw to maintain 5 cards
    this.autoDraw();
    
    // Check for win
    if (hand.length === 0 && (this.currentPlayer === 'Player1' ? this.player1Stock : this.player2Stock).length === 0) {
      this.gameOver = true;
      this.winner = this.currentPlayer;
    }
    
    // Switch players
    this.currentPlayer = this.currentPlayer === 'Player1' ? 'Player2' : 'Player1';
    
    return { success: true };
  }

  drawCards() {
    const stock = this.currentPlayer === 'Player1' ? this.player1Stock : this.player2Stock;
    const hand = this.currentPlayer === 'Player1' ? this.player1Hand : this.player2Hand;
    
    if (stock.length === 0) {
      return { success: false, error: 'No cards to draw' };
    }
    
    if (hand.length >= 5) {
      return { success: false, error: 'Hand is full' };
    }
    
    const card = stock.shift();
    hand.push(card);
    
    return { success: true };
  }

  autoDraw() {
    const stock = this.currentPlayer === 'Player1' ? this.player1Stock : this.player2Stock;
    const hand = this.currentPlayer === 'Player1' ? this.player1Hand : this.player2Hand;
    
    while (hand.length < 5 && stock.length > 0) {
      const card = stock.shift();
      hand.push(card);
    }
  }

  getAllMoves() {
    const moves = [];
    const hand = this.currentPlayer === 'Player1' ? this.player1Hand : this.player2Hand;
    
    // Check play moves
    for (const card of hand) {
      if (this.pile1 && Card.canPlayOn(card, this.pile1)) {
        moves.push(Move.PlayCard(card, 0));
      }
      if (this.pile2 && Card.canPlayOn(card, this.pile2)) {
        moves.push(Move.PlayCard(card, 1));
      }
    }
    
    // Check draw moves
    const stock = this.currentPlayer === 'Player1' ? this.player1Stock : this.player2Stock;
    if (stock.length > 0 && hand.length < 5) {
      moves.push(Move.DrawCards());
    }
    
    return moves;
  }

  areBothPlayersStuck() {
    const player1Moves = this.player1Hand.some(card => 
      (this.pile1 && Card.canPlayOn(card, this.pile1)) || 
      (this.pile2 && Card.canPlayOn(card, this.pile2))
    );
    const player2Moves = this.player2Hand.some(card => 
      (this.pile1 && Card.canPlayOn(card, this.pile1)) || 
      (this.pile2 && Card.canPlayOn(card, this.pile2))
    );
    
    return !player1Moves && !player2Moves;
  }

  changeMiddleCards() {
    // Create new random cards
    const suits = ['Hearts', 'Diamonds', 'Clubs', 'Spades'];
    const ranks = ['Ace', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King'];
    
    const newCard1 = new Card(
      suits[Math.floor(Math.random() * suits.length)],
      ranks[Math.floor(Math.random() * ranks.length)]
    );
    const newCard2 = new Card(
      suits[Math.floor(Math.random() * suits.length)],
      ranks[Math.floor(Math.random() * ranks.length)]
    );
    
    this.pile1 = newCard1;
    this.pile2 = newCard2;
  }
}

// Export for use in HTML
window.SpeedGame = {
  Card,
  Move,
  GameState
};
