# Deployment Guide

This guide explains how to deploy your Speed Card Game with:
- **Frontend**: GitHub Pages (free)
- **Backend**: Railway.app (free tier with $5 credit/month)

## Prerequisites

1. GitHub account
2. Railway account (sign up at https://railway.app with GitHub)
3. Game code pushed to GitHub repository

## Step 1: Deploy WebSocket Server to Railway

### Option A: Using Railway CLI (Recommended)

```bash
# Install Railway CLI
npm install -g @railway/cli

# Login to Railway
railway login

# Navigate to your project
cd /Users/louisyu/oxcaml

# Initialize Railway project
railway init

# Deploy the server
railway up

# Get your deployment URL
railway domain
# You'll get a URL like: https://your-app-name.up.railway.app
```

### Option B: Using Railway Dashboard

1. Go to https://railway.app
2. Click "New Project"
3. Select "Deploy from GitHub repo"
4. Choose your `oxcaml` repository
5. Railway will auto-detect the Node.js server
6. Click "Deploy"
7. Once deployed, go to "Settings" → "Generate Domain"
8. Copy your Railway URL (e.g., `https://your-app-name.up.railway.app`)

## Step 2: Update Client with Railway URL

After deploying to Railway, update the WebSocket URL in your client code:

```bash
# Edit ui/hw6_speed_game_ui.ml
# Replace line 24:
# else "wss://YOUR-APP-NAME.up.railway.app"
# with your actual Railway URL:
# else "wss://speed-game-abc123.up.railway.app"
```

## Step 3: Deploy Frontend to GitHub Pages

```bash
# Build the OCaml project
dune build

# The built files are in _build/default/ui/
# Copy them to your gh-pages branch root

# If not on gh-pages branch, switch to it
git checkout gh-pages

# Copy built files (adjust paths as needed)
cp _build/default/ui/*.js .
cp _build/default/ui/*.html .
cp -r _build/default/ui/assets . # if you have assets

# Commit and push
git add .
git commit -m "Deploy updated game with Railway backend"
git push origin gh-pages

# Your game will be live at:
# https://YOUR-USERNAME.github.io/oxcaml/
```

## Step 4: Test the Deployment

1. Open your GitHub Pages URL in a browser
2. The game should automatically connect to your Railway WebSocket server
3. Try creating a lobby - you should get a 6-character code
4. Open another browser/incognito window and join the lobby

## Environment Variables (Optional)

To make the Railway URL even more configurable, you can:

1. In Railway dashboard, go to your project → Variables
2. Add `PORT` variable (Railway sets this automatically to `8080` or dynamic port)
3. Your server already uses `process.env.PORT || 8080`

## Cost Breakdown

- **GitHub Pages**: FREE (unlimited static hosting)
- **Railway**: FREE with $5/month credit
  - WebSocket server uses minimal resources
  - Should stay within free tier for hobby projects
  - Sleeps after 30 minutes of inactivity (wakes on first request)

## Troubleshooting

### "WebSocket connection failed"
- Check Railway logs: `railway logs`
- Verify your Railway URL is correct in `hw6_speed_game_ui.ml`
- Ensure you're using `wss://` (secure WebSocket) not `ws://`

### "Lobby creation not working"
- Open browser console (F12) and check for errors
- Verify Railway server is running: visit `https://your-app.up.railway.app` (should see connection reset, that's OK)
- Check Railway logs for server errors

### "Server keeps sleeping"
- Railway free tier sleeps after inactivity
- First connection will wake it up (may take 5-10 seconds)
- Consider Railway Hobby plan ($5/month) for always-on server

## Local Development

For local development, the server runs on `http://localhost:8080`:

```bash
# Terminal 1: Run WebSocket server
cd server
npm install
npm start

# Terminal 2: Build and serve frontend
dune build --watch
# Open index.html in browser or use local server
```

## Updating the Deployment

### Update Backend (Railway)
```bash
# Make changes to server/server.js
git add server/
git commit -m "Update server logic"
git push origin main

# Railway auto-deploys on push (if connected to GitHub)
# Or manually: railway up
```

### Update Frontend (GitHub Pages)
```bash
# Make changes to OCaml code
dune build

# Switch to gh-pages and copy new build
git checkout gh-pages
cp _build/default/ui/*.js .
git add .
git commit -m "Update frontend"
git push origin gh-pages
```

## Next Steps

- [ ] Deploy server to Railway
- [ ] Get Railway URL and update `hw6_speed_game_ui.ml`
- [ ] Rebuild OCaml code with new URL
- [ ] Deploy to GitHub Pages
- [ ] Test multiplayer between two devices/browsers
- [ ] Share your game URL!

## Support

- Railway docs: https://docs.railway.app
- GitHub Pages docs: https://docs.github.com/pages
- WebSocket issues: Check Railway logs and browser console
