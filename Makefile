.PHONY : clean
clean:
	dune clean

COVERAGE := _coverage

.PHONY: coverage
coverage : clean
	BISECT_ENABLE=yes dune build
	dune runtest --no-buffer
	bisect-ppx-report \
	  -I _build/default/ -html $(COVERAGE)/ -text - -summary-only \
	  _build/default/test/bisect*.out _build/default/test/*/bisect*.out
	@echo See $(COVERAGE)/index.html

# Directory to write generated code to (.js and .d.ts files)
TS_OUT_DIR=./src/ts/generated

PROTO_FILE_DEPS += bookmark
PROTO_FILE_DEPS += completion
PROTO_FILE_DEPS += configuration
PROTO_FILE_DEPS += filer
PROTO_FILE_DEPS += keymap
PROTO_FILE_DEPS += task
PROTO_FILE_DEPS += types

define generate_for_ocaml
	protoc -I src/protobuf --ocaml_out=src/ocaml/server/generated \
		--ocaml_opt='singleton_record=true;annot=[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]' \
		src/protobuf/$1.proto

endef

define generate_for_typescript
	npx pbjs \
		-p src/protobuf \
		-t static-module \
		--keep-case \
		-o ${TS_OUT_DIR}/$1_pb.js \
		src/protobuf/$1.proto

	npx pbts \
		-o ${TS_OUT_DIR}/$1_pb.d.ts \
		${TS_OUT_DIR}/$1_pb.js

endef

.PHONY: generate
generate:
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_ocaml,$f))
	mkdir -p $(TS_OUT_DIR)
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_typescript,$f))
