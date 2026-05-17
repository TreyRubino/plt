# Trey Rubino — CPSC 425

OCAMLC := ocamlc
BUILD  := build
EXECUTABLE := $(BUILD)/interp

ML  := ast.ml reader.ml runtime.ml eval.ml main.ml
CMO := $(addprefix $(BUILD)/,$(ML:.ml=.cmo))

SRC ?= test.cl-type

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	@mkdir -p $(BUILD)

$(BUILD)/%.cmo: %.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c $< -o $@
	@if [ -f $*.cmi ]; then mv -f $*.cmi $(BUILD)/; fi

$(EXECUTABLE): $(CMO)
	$(OCAMLC) -I $(BUILD) -o $@ $(CMO)

run: all
	./$(EXECUTABLE) $(SRC)

clean:
	rm -f $(EXECUTABLE) a.out
	rm -rf $(BUILD)

.PHONY: all run clean
