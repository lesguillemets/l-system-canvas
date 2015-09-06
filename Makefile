HFILE=./src/LSystem.hs
JSFILE=./build/LSystem.js

$(JSFILE): $(HFILE)
	hastec -O2 $(HFILE) -o $(JSFILE)

clean:
	rm ./src/*.hi
	rm ./src/*.o
