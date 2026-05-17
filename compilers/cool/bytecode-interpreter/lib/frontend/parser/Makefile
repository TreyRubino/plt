# Trey Rubino
# CPSC 372 
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc -g
BUILD     = ../../../build

PARSER_MLY = parser.mly
YACC_BASE  = $(BUILD)/parser
GEN_ML     = $(YACC_BASE).ml
GEN_MLI    = $(YACC_BASE).mli

all: $(BUILD) $(BUILD)/parser.cmo

$(BUILD):
	mkdir -p $(BUILD)

$(GEN_ML): $(PARSER_MLY) | $(BUILD)
	$(OCAMLYACC) -b $(YACC_BASE) $(PARSER_MLY)
	@rm -f $(GEN_MLI) 

$(BUILD)/parser.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/parser.cmo $(GEN_ML)
	@([ -f parser.cmi ] && mv -f parser.cmi $(BUILD)/) || true

clean:
	rm -rf $(BUILD)

.PHONY: all clean
