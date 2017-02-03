# Quarto

The game of Quarto built with [Purescript](http://www.purescript.org/) and [Halogen](https://github.com/slamdata/purescript-halogen).
A work in progress...

# Installation

```
bower install
pulp build --to js/app.js
```

### Start Server

Create server.pem in the project root, or start any other local server. (https is)
for future webRTC support)

```
openssl req -new -x509 -keyout server.pem -out server.pem -days 365 -nodes
```

And then run `python server.py`

Navigate to [https://localhost:4443/](https://localhost:4443/)
