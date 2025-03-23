TARGET = luavm
SRC = ./src/LuaVM.ml
OCAMLC = ocamlc

run:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./$(TARGET)

run-file:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./$(TARGET) $(FILE)

clean:
	rm -f ./src/*.cmi ./src/*.cmo ./src/*.cmx ./src/*.o ./src/$(TARGET) luavm
