# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# VM build

OCAMLC = ocamlc -g
BUILD  = ../../../build

ML = runtime.ml strings.ml heap.ml gc.ml stack.ml alloc.ml builtin.ml exec.ml vm.ml
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

