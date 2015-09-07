HFILE=./src/Main.hs
JSFILE=./build/Main.js

$(JSFILE): $(wildcard ./src/*.hs)
	hastec -isrc -O2 $(HFILE) -o $(JSFILE)

clean:
	rm ./src/*.hi
	rm ./src/*.o
