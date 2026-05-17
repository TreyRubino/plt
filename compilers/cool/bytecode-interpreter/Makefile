# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# Build instructions with built in testing rules

OCAMLC = ocamlc -g
BUILD  = build

CORE    = lib/core

LEXER   = lib/frontend/lexer
PARSER  = lib/frontend/parser
CHECKER = lib/frontend/checker

CODEGEN = lib/backend/codegen
VM = lib/backend/vm

CORE_SRC = $(CORE)/error.ml $(CORE)/ast.ml $(CORE)/semantics.ml $(CORE)/bytecode.ml $(CORE)/ir.ml 
CORE_CMO = $(BUILD)/error.cmo $(BUILD)/ast.cmo $(BUILD)/semantics.cmo $(BUILD)/bytecode.cmo $(BUILD)/ir.cmo

MAIN = lib/core/main.ml
MAIN_CMO = $(BUILD)/main.cmo

EXEC = cooli

OBJS = \
	$(BUILD)/error.cmo \
  	$(BUILD)/ast.cmo \
	$(BUILD)/semantics.cmo \
	$(BUILD)/bytecode.cmo \
	$(BUILD)/ir.cmo \
  	$(BUILD)/lexer.cmo \
  	$(BUILD)/parser.cmo \
  	$(BUILD)/env.cmo \
  	$(BUILD)/validate.cmo \
  	$(BUILD)/typecheck.cmo \
  	$(BUILD)/checker.cmo \
	$(BUILD)/emit.cmo \
	$(BUILD)/gen.cmo \
	$(BUILD)/layout.cmo \
	$(BUILD)/lower.cmo \
	$(BUILD)/debug.cmo \
	$(BUILD)/bootstrap.cmo \
	$(BUILD)/codegen.cmo \
	$(BUILD)/runtime.cmo \
	$(BUILD)/heap.cmo \
	$(BUILD)/strings.cmo \
	$(BUILD)/gc.cmo \
	$(BUILD)/stack.cmo \
	$(BUILD)/alloc.cmo \
	$(BUILD)/builtin.cmo \
	$(BUILD)/exec.cmo \
	$(BUILD)/vm.cmo \
  	$(BUILD)/main.cmo

all: $(BUILD) core modules $(EXEC)

$(BUILD):
	mkdir -p $(BUILD)

core: $(CORE_CMO)

$(BUILD)/%.cmo: $(CORE)/%.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $(basename $<).cmi ] && mv -f $(basename $<).cmi $(BUILD)/) || true

$(MAIN_CMO): $(MAIN) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(MAIN_CMO) $(MAIN)
	@([ -f main.cmi ] && mv -f main.cmi $(BUILD)/) || true

modules:
	make -C $(PARSER)
	make -C $(LEXER)
	make -C $(CHECKER)
	make -C $(CODEGEN)
	make -C $(VM)

$(EXEC): $(OBJS)
	$(OCAMLC) -I $(BUILD) -o $(EXEC) $(OBJS)

clean:
	rm -rf $(BUILD)
	rm -f $(EXEC)
	rm -f debug.txt
	rm -f delta_report.txt

run: 
	make clean && make 
	./cooli ./test/files/hs.cl

regression:
	make clean && make 
	./tool/regression.sh

delta:
	make clean && make 
	./tool/regression.sh
	./tool/delta.sh

.PHONY: all core modules clean run
