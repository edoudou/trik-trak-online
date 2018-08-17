#!/bin/bash

# join a game
curl -X POST http://localhost:8092/join

# Exchange a card
curl --header "Content-Type: application/json" \
     -X POST \
     --data '{"type": "Exchange", "card": 12}' \
     http://localhost:8092/play/ee7a4600-1bca-4c66-bfe4-6862c958db36

# Quit the Game
curl --header "Content-Type: application/json" \
     -X POST \
     --data '{"type": "QuitGame"}' \
     http://localhost:8092/play/83ac2284-8256-45d2-abd6-bd01d04a73fb
