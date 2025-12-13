const WebSocket = require('ws');
const { v4: uuidv4 } = require('uuid');

const PORT = process.env.PORT || 8080;
const wss = new WebSocket.Server({ port: PORT });

// Store active lobbies and connections
const lobbies = new Map(); // lobbyCode -> { hostId, hostWs, joinerId, joinerWs, matchId, gameState }
const connections = new Map(); // ws -> { userId, lobbyCode }

console.log(`WebSocket server running on port ${PORT}`);

wss.on('connection', (ws) => {
  console.log('New client connected');

  ws.on('message', (message) => {
    try {
      const data = JSON.parse(message);
      console.log('Received:', data.type, data);

      handleMessage(ws, data);
    } catch (error) {
      console.error('❌ Error parsing message:', error);
      ws.send(JSON.stringify({ type: 'error', message: 'Invalid message format' }));
    }
  });

  ws.on('close', () => {
    console.log('Client disconnected');
    handleDisconnect(ws);
  });

  ws.on('error', (error) => {
    console.error('❌ WebSocket error:', error);
  });
});

function handleMessage(ws, data) {
  const { type } = data;

  switch (type) {
    case 'create_lobby':
      handleCreateLobby(ws, data);
      break;
    case 'join_lobby':
      handleJoinLobby(ws, data);
      break;
    case 'player_ready':
      handlePlayerReady(ws, data);
      break;
    case 'game_state_update':
      handleGameStateUpdate(ws, data);
      break;
    case 'new_game_ready':
      handleNewGameReady(ws, data);
      break;
    case 'ping':
      ws.send(JSON.stringify({ type: 'pong' }));
      break;
    default:
      ws.send(JSON.stringify({ type: 'error', message: `Unknown message type: ${type}` }));
  }
}

function handleCreateLobby(ws, data) {
  const { userId } = data;

  // Generate lobby code
  const lobbyCode = generateLobbyCode();

  // Create lobby
  const lobby = {
    hostId: userId,
    hostWs: ws,
    joinerId: null,
    joinerWs: null,
    matchId: null,
    gameState: null,
    hostReady: false,
    joinerReady: false,
    gameStarted: false,
    hostNewGameReady: false,
    joinerNewGameReady: false,
    createdAt: Date.now()
  };

  lobbies.set(lobbyCode, lobby);
  connections.set(ws, { userId, lobbyCode });

  console.log(`Lobby created: ${lobbyCode} by ${userId}`);

  // Send response to host
  ws.send(JSON.stringify({
    type: 'lobby_created',
    lobbyCode,
    userId
  }));
}

function handleJoinLobby(ws, data) {
  const { userId, lobbyCode } = data;

  const lobby = lobbies.get(lobbyCode);

  if (!lobby) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Lobby not found'
    }));
    return;
  }

  if (lobby.hostId === userId) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Cannot join your own lobby'
    }));
    return;
  }

  if (lobby.joinerId) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Lobby is full'
    }));
    return;
  }

  // Join the lobby
  lobby.joinerId = userId;
  lobby.joinerWs = ws;
  connections.set(ws, { userId, lobbyCode });

  // Create match ID
  const matchId = `match_${lobby.hostId}_${userId}`;
  lobby.matchId = matchId;

  console.log(`${userId} joined lobby ${lobbyCode}, match: ${matchId}`);

  // Notify joiner
  ws.send(JSON.stringify({
    type: 'match_found',
    matchId,
    lobbyCode,
    playerId: userId,
    opponentId: lobby.hostId,
    isHost: false
  }));

  // Notify host
  if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
    lobby.hostWs.send(JSON.stringify({
      type: 'match_found',
      matchId,
      lobbyCode,
      playerId: lobby.hostId,
      opponentId: userId,
      isHost: true
    }));
  }
}

function handlePlayerReady(ws, data) {
  const { lobbyCode, playerId } = data;

  const lobby = lobbies.get(lobbyCode);
  if (!lobby) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Lobby not found'
    }));
    return;
  }

  // Mark player as ready
  if (playerId === lobby.hostId) {
    lobby.hostReady = true;
    console.log(`Host ${playerId} is ready in lobby ${lobbyCode}`);
  } else if (playerId === lobby.joinerId) {
    lobby.joinerReady = true;
    console.log(`Joiner ${playerId} is ready in lobby ${lobbyCode}`);
  }

  // Notify both players about ready status
  const readyStatusMsg = JSON.stringify({
    type: 'ready_status',
    hostReady: lobby.hostReady,
    joinerReady: lobby.joinerReady
  });

  if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
    lobby.hostWs.send(readyStatusMsg);
  }
  if (lobby.joinerWs && lobby.joinerWs.readyState === WebSocket.OPEN) {
    lobby.joinerWs.send(readyStatusMsg);
  }

  // If both players are ready and game hasn't started, start the game
  if (lobby.hostReady && lobby.joinerReady && !lobby.gameStarted) {
    lobby.gameStarted = true;
    console.log(`Game starting in lobby ${lobbyCode} - both players ready!`);

    const gameStartMsg = JSON.stringify({
      type: 'game_started'
    });

    if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
      lobby.hostWs.send(gameStartMsg);
    }
    if (lobby.joinerWs && lobby.joinerWs.readyState === WebSocket.OPEN) {
      lobby.joinerWs.send(gameStartMsg);
    }
  }
}

function handleGameStateUpdate(ws, data) {
  const conn = connections.get(ws);
  if (!conn) return;

  const { lobbyCode } = conn;
  const lobby = lobbies.get(lobbyCode);
  if (!lobby) return;

  const { gameState, playerId } = data;

  // Update stored game state
  lobby.gameState = gameState;

  console.log(`Game state update from ${playerId} in lobby ${lobbyCode}`);

  // Broadcast to opponent
  const message = JSON.stringify({
    type: 'game_state_update',
    gameState,
    fromPlayer: playerId
  });

  if (lobby.hostWs && lobby.hostWs !== ws && lobby.hostWs.readyState === WebSocket.OPEN) {
    lobby.hostWs.send(message);
  }
  if (lobby.joinerWs && lobby.joinerWs !== ws && lobby.joinerWs.readyState === WebSocket.OPEN) {
    lobby.joinerWs.send(message);
  }
}

function handleNewGameReady(ws, data) {
  const { lobbyCode, playerId } = data;

  const lobby = lobbies.get(lobbyCode);
  if (!lobby) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Lobby not found'
    }));
    return;
  }

  // Mark player as ready for new game
  if (playerId === lobby.hostId) {
    lobby.hostNewGameReady = true;
    console.log(`Host ${playerId} is ready for new game in lobby ${lobbyCode}`);
  } else if (playerId === lobby.joinerId) {
    lobby.joinerNewGameReady = true;
    console.log(`Joiner ${playerId} is ready for new game in lobby ${lobbyCode}`);
  }

  // Notify both players about new game ready status
  const newGameReadyStatusMsg = JSON.stringify({
    type: 'new_game_ready_status',
    hostReady: lobby.hostNewGameReady,
    joinerReady: lobby.joinerNewGameReady
  });

  if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
    lobby.hostWs.send(newGameReadyStatusMsg);
  }
  if (lobby.joinerWs && lobby.joinerWs.readyState === WebSocket.OPEN) {
    lobby.joinerWs.send(newGameReadyStatusMsg);
  }

  // If both players are ready for new game, start new game
  if (lobby.hostNewGameReady && lobby.joinerNewGameReady) {
    console.log(`Starting new game in lobby ${lobbyCode} - both players ready!`);

    // Reset ready flags for next game
    lobby.hostNewGameReady = false;
    lobby.joinerNewGameReady = false;

    const newGameStartMsg = JSON.stringify({
      type: 'new_game_start'
    });

    if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
      lobby.hostWs.send(newGameStartMsg);
    }
    if (lobby.joinerWs && lobby.joinerWs.readyState === WebSocket.OPEN) {
      lobby.joinerWs.send(newGameStartMsg);
    }
  }
}

function handleDisconnect(ws) {
  const conn = connections.get(ws);
  if (!conn) return;

  const { userId, lobbyCode } = conn;
  const lobby = lobbies.get(lobbyCode);

  if (lobby) {
    console.log(`${userId} disconnected from lobby ${lobbyCode}`);

    // Notify opponent
    const opponentWs = lobby.hostWs === ws ? lobby.joinerWs : lobby.hostWs;
    if (opponentWs && opponentWs.readyState === WebSocket.OPEN) {
      opponentWs.send(JSON.stringify({
        type: 'opponent_disconnected'
      }));
    }

    // Clean up lobby
    lobbies.delete(lobbyCode);
  }

  connections.delete(ws);
}

function generateLobbyCode() {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code;

  do {
    code = '';
    for (let i = 0; i < 6; i++) {
      code += chars[Math.floor(Math.random() * chars.length)];
    }
  } while (lobbies.has(code));

  return code;
}

// Clean up old lobbies every 5 minutes
setInterval(() => {
  const now = Date.now();
  const fiveMinutes = 5 * 60 * 1000;

  for (const [code, lobby] of lobbies.entries()) {
    if (now - lobby.createdAt > fiveMinutes && !lobby.joinerId) {
      console.log(`Cleaning up old lobby: ${code}`);
      lobbies.delete(code);
    }
  }
}, 5 * 60 * 1000);
