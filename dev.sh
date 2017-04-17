#!/bin/bash

trap 'kill %1; kill %2; kill %3' SIGINT
python server.py & npm run compile & npm run webpack & npm run scss
