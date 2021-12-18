CPP := clang++
CPPFLAGS := -Wall -g -march=native -std=c++20
LINKERFLAGS := -lSDL2

OBJ_DIR := obj
_OBJ := GameBoy
OBJ := $(patsubst %, $(OBJ_DIR)/%, $(_OBJ).o)

SRC_DIR := src

MAIN := $(SRC_DIR)/main.cpp

BINARY_NAME := gbemu

.DEFAULT_GOAL := $(BINARY_NAME)

# Compile the target
$(BINARY_NAME): $(OBJ) $(MAIN)
#	clang-format -i -style=file $(MAIN)
	$(CPP) $(LINKERFLAGS) $(CPPFLAGS) $^ -o $@

# Compile all the object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.hpp
	mkdir -p $(OBJ_DIR)
# 	clang-format -i -style=file $<
	$(CPP) $(CPPFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -f $(OBJ_DIR)/*.o $(BINARY_NAME)
	rmdir --ignore-fail-on-non-empty $(OBJ_DIR)
