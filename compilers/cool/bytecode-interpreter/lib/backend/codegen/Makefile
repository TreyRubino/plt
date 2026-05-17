# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# Codegen build

OCAMLC = ocamlc -g
BUILD  = ../../../build

ML = emit.ml gen.ml layout.ml lower.ml debug.ml codegen.ml bootstrap.ml
CMO = $(addprefix $(BUILD)/,$(ML:.ml=.cmo))

all: $(BUILD) $(CMO)

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/%.cmo: %.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $*.cmi ] && mv -f $*.cmi $(BUILD)/) || true

clean: 
	rm -rf $(BUIlD)

.PHONY: all clean

