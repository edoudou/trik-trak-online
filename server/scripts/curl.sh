#!/bin/bash

# join a game
curl -X POST http://localhost:8092/join

# Exchange a card
curl --header "Content-Type: application/json" \
     -X POST \
     --data '{"type": "Exchange", "card": 2}' \
     http://localhost:8092/play/0796006a-330e-40ba-b247-c91e68176ab0
# Quit the Game
curl --header "Content-Type: application/json" \
     -X POST \
     --data '{"type": "QuitGame"}' \
     http://localhost:8092/play/83ac2284-8256-45d2-abd6-bd01d04a73fb
