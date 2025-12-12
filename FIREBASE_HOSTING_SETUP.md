# Firebase Hosting Setup Guide

This guide will help you deploy your Speed Card Game to Firebase Hosting and set up multiplayer functionality.

## Prerequisites

1. Firebase project created (see `FIREBASE_SETUP.md`)
2. Firebase CLI installed: `npm install -g firebase-tools`
3. Firebase Authentication enabled (Email/Password)
4. Firestore Database created

## Step 1: Install Firebase CLI

```bash
npm install -g firebase-tools
```

## Step 2: Login to Firebase

```bash
firebase login
```

## Step 3: Initialize Firebase Hosting

In your project directory:

```bash
firebase init hosting
```

When prompted:
- **Select your Firebase project** (or create a new one)
- **Public directory**: `.` (current directory)
- **Single-page app**: `Yes` (for routing support)
- **Overwrite index.html**: `No` (keep your existing file)

## Step 4: Configure Firebase Hosting

A `firebase.json` file will be created. Update it to include all necessary files:

```json
{
  "hosting": {
    "public": ".",
    "ignore": [
      "firebase.json",
      "**/.*",
      "**/node_modules/**",
      "**/_build/**",
      "**/*.ml",
      "**/*.mli",
      "**/dune",
      "**/dune-project",
      "**/opam",
      "**/.git/**"
    ],
    "rewrites": [
      {
        "source": "**",
        "destination": "/index.html"
      }
    ],
    "headers": [
      {
        "source": "**/*.@(js|css|html)",
        "headers": [
          {
            "key": "Cache-Control",
            "value": "no-cache, no-store, must-revalidate"
          }
        ]
      }
    ]
  }
}
```

## Step 5: Set Up Firestore Security Rules

In Firebase Console:
1. Go to **Firestore Database** > **Rules**
2. Update the rules to:

```javascript
rules_version = '2';
service cloud.firestore {
  match /databases/{database}/documents {
    // Players collection - user can read/write their own stats
    match /players/{userId} {
      allow read: if request.auth != null;
      allow write: if request.auth != null && request.auth.uid == userId;
    }
    
    // Matchmaking collection - authenticated users can create and read matchmaking requests
    match /matchmaking/{matchId} {
      allow read: if request.auth != null;
      allow create: if request.auth != null;
      allow update: if request.auth != null;
      allow delete: if request.auth != null;
    }
    
    // Matches collection - players in the match can read/write
    match /matches/{matchId} {
      allow read: if request.auth != null;
      allow write: if request.auth != null 
        && (resource == null || resource.data.lastUpdatedBy == request.auth.uid);
    }
  }
}
```

## Step 6: Build Your Application

```bash
# Activate OCaml environment
eval $(opam env --switch 5.2.0+ox)

# Build the project
dune build

# Copy the compiled JavaScript
cp _build/default/ui/hw6_speed_game_app.bc.js .
```

## Step 7: Deploy to Firebase Hosting

```bash
firebase deploy --only hosting
```

Your app will be available at: `https://YOUR_PROJECT_ID.web.app`

## Step 8: Test Multiplayer

1. Open your deployed app in two different browsers (or incognito windows)
2. Sign up with two different accounts
3. In both browsers, click "Play Online (Multiplayer)"
4. The matchmaking system should pair you together
5. Start the game and verify that moves sync in real-time

## Multiplayer Architecture

The multiplayer system uses Firebase Firestore with the following collections:

### `matchmaking` Collection
- **Purpose**: Queue for players looking for opponents
- **Document ID**: `mm_{uid}_{timestamp}`
- **Fields**:
  - `playerId`: User's Firebase UID
  - `status`: "waiting" or "matched"
  - `createdAt`: Timestamp
  - `matchId`: (when matched) The match document ID

### `matches` Collection
- **Purpose**: Active game matches
- **Document ID**: `match_{player1_uid}_{player2_uid}`
- **Fields**:
  - `player1`: First player's UID
  - `player2`: Second player's UID
  - `status`: "active"
  - `gameState`: Serialized game state (S-expression string)
  - `lastUpdatedBy`: UID of player who last updated the state
  - `timestamp`: Last update timestamp

### `players` Collection
- **Purpose**: Player statistics
- **Document ID**: User's Firebase UID
- **Fields**:
  - `wins`: Number of wins
  - `losses`: Number of losses
  - `games_played`: Total games played
  - `win_rate`: Win rate percentage

## How Matchmaking Works

1. **Player clicks "Play Online (Multiplayer)"**
   - Creates a document in `matchmaking` collection with status "waiting"
   - Queries for other players with status "waiting"

2. **If opponent found:**
   - Creates a match document in `matches` collection
   - Updates both matchmaking documents to "matched"
   - Both players receive `Match_found` action

3. **If no opponent:**
   - Sets up a Firestore listener on the matchmaking document
   - When another player matches, the listener fires
   - Player receives `Match_found` action

4. **During gameplay:**
   - Each move syncs game state to Firestore
   - Real-time listener on match document updates opponent's view
   - Only processes updates from opponent (checks `lastUpdatedBy`)

## Troubleshooting

### Matchmaking doesn't work
- Check Firestore security rules allow authenticated reads/writes
- Verify both players are signed in
- Check browser console for Firestore errors

### Game state not syncing
- Verify Firestore listener is set up (check console logs)
- Check that `lastUpdatedBy` field is being set correctly
- Ensure both players are in the same match document

### Deployment issues
- Make sure `hw6_speed_game_app.bc.js` is in the root directory
- Verify `index.html` references the correct JS file
- Check Firebase Hosting logs: `firebase hosting:channel:list`

## Continuous Deployment

To automatically deploy on every build:

1. Create `.github/workflows/deploy.yml` (if using GitHub):
```yaml
name: Deploy to Firebase Hosting
on:
  push:
    branches: [ main ]
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-node@v2
        with:
          node-version: '16'
      - run: npm install -g firebase-tools
      - run: firebase deploy --only hosting --token ${{ secrets.FIREBASE_TOKEN }}
```

2. Get Firebase token: `firebase login:ci`
3. Add token to GitHub Secrets as `FIREBASE_TOKEN`

