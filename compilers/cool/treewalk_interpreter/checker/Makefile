# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger

OCAMLC = ocamlc
BUILD  = build

EXECUTABLE = $(BUILD)/checker
SRC = test.cl-ast

ML  = ast.ml env.ml reader.ml typecheck.ml writer.ml validate.ml main.ml
CMO = $(addprefix $(BUILD)/,$(ML:.ml=.cmo))

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/%.cmo: %.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $*.cmi ] && mv -f $*.cmi $(BUILD)/) || true

$(EXECUTABLE): $(CMO) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -o $@ $(CMO)

run:
	make clean && make && ./$(EXECUTABLE) $(SRC)

clean:
	rm -f $(EXECUTABLE) *.cl-type a.out
	rm -rf $(BUILD)

.PHONY: all run clean
