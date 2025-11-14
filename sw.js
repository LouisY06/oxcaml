// ============================================================
// SERVICE WORKER - Offline Support for Speed Card Game
// ============================================================
// A service worker is a JavaScript file that runs in the background,
// separate from the main page. It enables:
// - Offline functionality by caching files
// - Background sync capabilities
// - Push notifications (not used here)
//
// The Cache API is used to store network responses (HTML, CSS, JS)
// so the app can work without an internet connection.

// Cache version - increment this to force cache updates
// When changed, old caches are deleted and new ones are created
const CACHE_NAME = 'speed-game-v2';

// List of files to cache for offline access
// These are the essential files needed to run the game offline
const urlsToCache = [
  './',                              // Root/index page
  './index.html',                    // Main HTML file
  './hw6_speed_game_app.bc.js',      // Compiled OCaml/Bonsai JavaScript
  './hw5_html_css/hw5_speed_game.css', // CSS styles
  './sw.js'                          // This service worker file
];

// ============================================================
// INSTALL EVENT - Cache API: Initial Caching
// ============================================================
// This event fires when the service worker is first installed
// or when a new version is detected. We use the Cache API to
// store all necessary files for offline access.
self.addEventListener('install', (event) => {
  // event.waitUntil() ensures the service worker doesn't install
  // until the caching is complete
  event.waitUntil(
    // caches.open() opens a cache with the given name, creating it if needed
    // Returns a Promise that resolves to the Cache object
    caches.open(CACHE_NAME)
      .then((cache) => {
        console.log('Service Worker: Caching files');
        // cache.addAll() fetches all URLs and adds them to the cache
        // This is an atomic operation - if any file fails, all fail
        return cache.addAll(urlsToCache);
      })
      .catch((err) => {
        console.error('Service Worker: Cache failed', err);
      })
  );
  // skipWaiting() immediately activates this service worker,
  // even if other tabs are still using the old one
  self.skipWaiting();
});

// ============================================================
// ACTIVATE EVENT - Cache API: Cleanup Old Caches
// ============================================================
// This event fires when the service worker becomes active.
// We use the Cache API to delete old cache versions to free up space.
self.addEventListener('activate', (event) => {
  event.waitUntil(
    // caches.keys() returns all cache names
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          // Delete any cache that doesn't match the current version
          // This prevents old cached files from taking up space
          if (cacheName !== CACHE_NAME) {
            console.log('Service Worker: Deleting old cache', cacheName);
            // caches.delete() removes the specified cache
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
  // clients.claim() makes this service worker control all pages
  // immediately, without requiring a page reload
  return self.clients.claim();
});

// ============================================================
// FETCH EVENT - Cache API: Serve from Cache or Network
// ============================================================
// This event intercepts all network requests from the page.
// We use the Cache API to serve cached responses when offline,
// and cache new responses when online.
self.addEventListener('fetch', (event) => {
  // event.respondWith() allows us to provide a custom response
  event.respondWith(
    // caches.match() checks if the request is in the cache
    // Returns the cached Response if found, undefined otherwise
    caches.match(event.request)
      .then((response) => {
        // Strategy: Cache First, then Network (Cache-Then-Network)
        // If cached version exists, return it immediately (fast!)
        // Otherwise, fetch from network
        return response || fetch(event.request).then((response) => {
          // Only cache successful GET requests
          // Don't cache POST/PUT/DELETE or error responses
          if (event.request.method !== 'GET' || !response || response.status !== 200) {
            return response;
          }
          
          // Clone the response because:
          // 1. Responses can only be read once
          // 2. We need to return the original AND cache a copy
          const responseToCache = response.clone();
          
          // Open cache and store the response for future offline use
          // This happens asynchronously, so we return the original response immediately
          caches.open(CACHE_NAME).then((cache) => {
            // cache.put() stores the request/response pair in the cache
            cache.put(event.request, responseToCache);
          });
          
          return response;
        });
      })
      .catch(() => {
        // If both cache lookup AND network fetch fail:
        // - For page requests, try to serve the cached index.html
        // - For other requests, return an offline error message
        if (event.request.destination === 'document') {
          // Try to serve the cached index.html as a fallback
          return caches.match('./index.html') || caches.match('./');
        }
        // Return a basic offline error response
        return new Response('Offline - content not available', {
          status: 503,
          statusText: 'Service Unavailable',
          headers: new Headers({
            'Content-Type': 'text/plain'
          })
        });
      })
  );
});

