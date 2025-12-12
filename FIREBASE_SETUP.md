# Firebase Setup Instructions

This game now supports online multiplayer using Firebase Authentication and Firestore. Follow these steps to set it up:

## 1. Create a Firebase Project

1. Go to [Firebase Console](https://console.firebase.google.com/)
2. Click "Add project" or select an existing project
3. Follow the setup wizard

## 2. Enable Authentication

1. In Firebase Console, go to **Authentication** > **Get started**
2. Enable **Email/Password** sign-in method
3. Click on "Email/Password" and toggle "Enable"
4. Save

## 3. Create Firestore Database

1. In Firebase Console, go to **Firestore Database** > **Create database**
2. Start in **test mode** (for development) or set up security rules for production
3. Choose a location for your database

## 4. Get Firebase Configuration

1. In Firebase Console, go to **Project Settings** (gear icon)
2. Scroll down to "Your apps" section
3. Click the web icon (`</>`) to add a web app
4. Register your app (you can use any app nickname)
5. Copy the Firebase configuration object

## 5. Update index.html

Open `index.html` and replace the placeholder values in the Firebase config:

```javascript
const firebaseConfig = {
    apiKey: "YOUR_API_KEY",
    authDomain: "YOUR_PROJECT_ID.firebaseapp.com",
    projectId: "YOUR_PROJECT_ID",
    storageBucket: "YOUR_PROJECT_ID.appspot.com",
    messagingSenderId: "YOUR_MESSAGING_SENDER_ID",
    appId: "YOUR_APP_ID"
};
```

Replace:
- `YOUR_API_KEY` with your API key
- `YOUR_PROJECT_ID` with your project ID (appears in multiple places)
- `YOUR_MESSAGING_SENDER_ID` with your messaging sender ID
- `YOUR_APP_ID` with your app ID

## 6. Set Up Firestore Security Rules (REQUIRED for Multiplayer)

**IMPORTANT**: You must set up these security rules for multiplayer to work!

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
    
    // Lobbies collection (for lobby code system)
    match /lobbies/{lobbyCode} {
      allow read: if request.auth != null;
      allow create: if request.auth != null;
      allow update: if request.auth != null;
      allow delete: if request.auth != null;
    }
  }
}
```

3. Click **Publish** to save the rules

## 7. Build and Test

1. Build the project: `dune build`
2. Copy the compiled JS: `cp _build/default/ui/hw6_speed_game_app.bc.js .`
3. Open `index.html` in a browser
4. Try signing up with a new account
5. Click "Find Online Opponent" to test matchmaking

## Features

- **Authentication**: Users can sign up and sign in with email/password
- **Matchmaking**: Authenticated users can find online opponents
- **Real-time Sync**: Game state syncs in real-time via Firestore
- **Single Player**: Still works offline against AI

## Troubleshooting

- **Firebase not initialized**: Make sure you've updated the config in `index.html`
- **Authentication fails**: Check that Email/Password is enabled in Firebase Console
- **Matchmaking doesn't work**: Verify Firestore is created and security rules allow reads/writes
- **Game state not syncing**: Check browser console for Firestore errors


