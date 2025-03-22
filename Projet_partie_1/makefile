TARGET = luavm
SRC = ./src/LuaVM.ml
OCAMLC = ocamlc

all:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./$(TARGET)

clean:
	rm -f ./src/*.cmi ./src/*.cmo ./src/*.cmx ./src/*.o ./src/$(TARGET) luavm

