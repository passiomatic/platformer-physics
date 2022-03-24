all: run

levels:
	python3 elmify.py ./assets/level1.json ./src

clean: 
	rm -R dist 

build: levels
	elm make src/Main.elm --output dist/elm.js --optimize 

run:
	elm reactor
