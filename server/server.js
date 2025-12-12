const WebSocket = require('ws');
const { v4: uuidv4 } = require('uuid');

const PORT = process.env.PORT || 8080;
const wss = new WebSocket.Server({ port: PORT });

// Store active lobbies and connections
const lobbies = new Map(); // lobbyCode -> { hostId, hostWs, joinerId, joinerWs, matchId, gameState }
const connections = new Map(); // ws -> { userId, lobbyCode }

console.log(`🚀 WebSocket server running on port ${PORT}`);

wss.on('connection', (ws) => {
  console.log('📱 New client connected');

  ws.on('message', (message) => {
    try {
      const data = JSON.parse(message);
      console.log('📨 Received:', data.type, data);

      handleMessage(ws, data);
    } catch (error) {
      console.error('❌ Error parsing message:', error);
      ws.send(JSON.stringify({ type: 'error', message: 'Invalid message format' }));
    }
  });

  ws.on('close', () => {
    console.log('👋 Client disconnected');
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
    case 'start_game':
      handleStartGame(ws, data);
      break;
    case 'game_state_update':
      handleGameStateUpdate(ws, data);
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
    createdAt: Date.now()
  };

  lobbies.set(lobbyCode, lobby);
  connections.set(ws, { userId, lobbyCode });

  console.log(`🏠 Lobby created: ${lobbyCode} by ${userId}`);

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

  console.log(`🤝 ${userId} joined lobby ${lobbyCode}, match: ${matchId}`);

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

function handleStartGame(ws, data) {
  const { lobbyCode, gameState } = data;

  const lobby = lobbies.get(lobbyCode);
  if (!lobby) {
    ws.send(JSON.stringify({
      type: 'error',
      message: 'Lobby not found'
    }));
    return;
  }

  // Store initial game state
  lobby.gameState = gameState;

  console.log(`🎮 Game started in lobby ${lobbyCode}`);

  // Broadcast to both players
  const message = JSON.stringify({
    type: 'game_started',
    gameState
  });

  if (lobby.hostWs && lobby.hostWs.readyState === WebSocket.OPEN) {
    lobby.hostWs.send(message);
  }
  if (lobby.joinerWs && lobby.joinerWs.readyState === WebSocket.OPEN) {
    lobby.joinerWs.send(message);
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

  console.log(`🎯 Game state update from ${playerId} in lobby ${lobbyCode}`);

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

function handleDisconnect(ws) {
  const conn = connections.get(ws);
  if (!conn) return;

  const { userId, lobbyCode } = conn;
  const lobby = lobbies.get(lobbyCode);

  if (lobby) {
    console.log(`👋 ${userId} disconnected from lobby ${lobbyCode}`);

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
      console.log(`🧹 Cleaning up old lobby: ${code}`);
      lobbies.delete(code);
    }
  }
}, 5 * 60 * 1000);
