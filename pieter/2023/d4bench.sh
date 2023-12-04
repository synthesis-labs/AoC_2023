#!/usr/bin/env bash

echo "Python 3"
time python3 d4.py
time python3 d4.py
time python3 d4.py

echo "JS (Node 18.17.1)"
time node d4.js
time node d4.js
time node d4.js

echo "JS (Bun 1.0.15)"
time bun d4.js
time bun d4.js
time bun d4.js