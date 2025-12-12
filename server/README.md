# Speed Game WebSocket Server

WebSocket server for real-time multiplayer Speed card game.

## Setup

```bash
cd server
npm install
```

## Run Locally

```bash
npm start
```

Server will run on `ws://localhost:8080`

## Deploy to Railway (Free Option)

1. Sign up at https://railway.app
2. Create new project
3. Deploy from this directory
4. Railway will auto-detect Node.js and run `npm start`
5. Get your WebSocket URL: `wss://your-app.railway.app`

## Deploy to Render (Free Option)

1. Sign up at https://render.com
2. Create new Web Service
3. Connect GitHub repo
4. Set:
   - Root directory: `server`
   - Build command: `npm install`
   - Start command: `npm start`
5. Get your WebSocket URL: `wss://your-app.onrender.com`

## Environment Variables

- `PORT`: Server port (default: 8080)

## API

### Client → Server Messages

**Create Lobby:**
```json
{
  "type": "create_lobby",
  "userId": "user_123"
}
```

**Join Lobby:**
```json
{
  "type": "join_lobby",
  "userId": "user_456",
  "lobbyCode": "ABC123"
}
```

**Start Game:**
```json
{
  "type": "start_game",
  "lobbyCode": "ABC123",
  "gameState": { ... }
}
```

**Game State Update:**
```json
{
  "type": "game_state_update",
  "lobbyCode": "ABC123",
  "playerId": "user_123",
  "gameState": { ... }
}
```

### Server → Client Messages

**Lobby Created:**
```json
{
  "type": "lobby_created",
  "lobbyCode": "ABC123",
  "userId": "user_123"
}
```

**Match Found:**
```json
{
  "type": "match_found",
  "matchId": "match_host_joiner",
  "lobbyCode": "ABC123",
  "playerId": "user_123",
  "opponentId": "user_456",
  "isHost": true
}
```

**Game State Update:**
```json
{
  "type": "game_state_update",
  "gameState": { ... },
  "fromPlayer": "user_456"
}
```

**Error:**
```json
{
  "type": "error",
  "message": "Error description"
}
```
