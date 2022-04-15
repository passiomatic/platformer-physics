all: run

levels:
	./tiled.py ./assets/level1.json ./src

clean: 
	rm -R dist 

build: levels
	elm make src/Main.elm --output dist/elm.js --optimize 

build-optimize-2:
	elm-optimize-level-2 src/Main.elm  --output dist/elm.js

dist: build-optimize-2
	cp index.html dist/

run:
	elm reactor
