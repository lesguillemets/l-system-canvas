HFILE=./src/Main.hs
JSFILE=./build/Main.js

$(JSFILE): $(wildcard ./src/*.hs)
	hastec -isrc -O2 --debug $(HFILE) -o $(JSFILE)

clean:
	rm ./src/*.hi
	rm ./src/*.o
