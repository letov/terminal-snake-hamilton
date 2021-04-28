# terminal-snake-hamilton
Terminal snake game with bot mode, based on Hamilton cycle, written in Haskell.
Not working in Windows console (hSetBuffering stdin NoBuffering).
https://habr.com/ru/post/551504/
## requirements
* base >=4.12 && <4.13,
* time >=1.8.0.2,
* ansi-terminal >=0.11,
* random >= 1.1
## installation
cabal install
## usage
./dist/build/terminal-snake-hamilton/terminal-snake-hamilton

* w - Up
* s - Down
* a - Left
* d - Right
* hold - speed boost
* h - Hamilton mode
* q - Quit
