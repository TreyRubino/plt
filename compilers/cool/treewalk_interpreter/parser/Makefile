# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc
BUILD     = build

PARSER_MLY = main.mly
YACC_BASE  = $(BUILD)/main
GEN_ML     = $(YACC_BASE).ml
GEN_MLI    = $(YACC_BASE).mli
GEN_OUT    = $(YACC_BASE).output

EXECUTABLE = $(BUILD)/parser
SRC        = two_class_test.cl-lex

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

$(GEN_ML): $(PARSER_MLY) | $(BUILD)
	$(OCAMLYACC) -v -b $(YACC_BASE) $(PARSER_MLY)
	@rm -f $(GEN_MLI) 

$(BUILD)/main.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/main.cmo $(GEN_ML)
	@([ -f main.cmi ] && mv -f main.cmi $(BUILD)/) || true

$(EXECUTABLE): $(BUILD)/main.cmo | $(BUILD)
	$(OCAMLC) -I $(BUILD) -o $(EXECUTABLE) $(BUILD)/main.cmo

run:
	$(MAKE) clean && $(MAKE) && ./$(EXECUTABLE) $(SRC)

clean:
	rm -rf $(BUILD)

.PHONY: all run clean
