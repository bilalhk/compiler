top: init parser
	@ cd _build && \
	ocamlmktop -o top ast.cmo tiger_parser.cmo tiger_lexer.cmo && \
	mv top ../

parser: ast
	@ cd parser && \
	ocamllex tiger_lexer.mll && \
	ocamlyacc -v tiger_parser.mly && \
	mv tiger_lexer.ml tiger_parser.ml tiger_parser.mli tiger_parser.output ../_build && \
	cd ../_build && \
	ocamlc -c tiger_parser.mli tiger_parser.ml tiger_lexer.ml

ast: FORCE
	@ cd ast && \
	ocamlc -c ast.ml && \
	mv ast.cmo ast.cmi ../_build

init: FORCE
	@ if [ -d _build ]; then \
		:; \
	else \
		mkdir _build; \
	fi

clean: FORCE
	@ rm -rf _build && \
	rm top

FORCE: