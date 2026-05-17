# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# Lexer build

OCAMLLEX = ocamllex
OCAMLC   = ocamlc -g
BUILD    = ../../../build

MAIN_MLL = lexer.mll
GEN_ML   = $(BUILD)/lexer.ml

all: $(BUILD) $(BUILD)/lexer.cmo

$(BUILD):
	mkdir -p $(BUILD)

$(GEN_ML): $(MAIN_MLL) | $(BUILD)
	$(OCAMLLEX) -o $(GEN_ML) $(MAIN_MLL)

$(BUILD)/lexer.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/lexer.cmo $(GEN_ML)
	@([ -f lexer.cmi ] && mv -f lexer.cmi $(BUILD)/) || true

clean:
	rm -rf $(BUILD)

.PHONY: all clean
