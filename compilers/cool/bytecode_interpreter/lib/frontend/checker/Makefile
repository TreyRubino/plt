# Trey Rubino
# CPSC 372 Independent study
# Dr. Schwesinger

OCAMLC = ocamlc -g
BUILD  = ../../../build

ML  = env.ml typecheck.ml validate.ml checker.ml
CMO = $(addprefix $(BUILD)/,$(ML:.ml=.cmo))

all: $(BUILD) $(CMO)

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/%.cmo: %.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $*.cmi ] && mv -f $*.cmi $(BUILD)/) || true

clean:
	rm -rf $(BUILD)

.PHONY: all clean
