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

# Path to this plugin
PROTOC_GEN_TS_PATH = ./node_modules/.bin/protoc-gen-ts

# Directory to write generated code to (.js and .d.ts files)
TS_OUT_DIR=./src/ts/generated

PROTO_FILE_DEPS += bookmark.proto
PROTO_FILE_DEPS += completion.proto
PROTO_FILE_DEPS += configuration.proto
PROTO_FILE_DEPS += filer.proto
PROTO_FILE_DEPS += keymap.proto
PROTO_FILE_DEPS += task.proto
PROTO_FILE_DEPS += types.proto

define generate_for_ocaml
	protoc -I src/protobuf --ocaml_out=src/ocaml/server/generated \
		--ocaml_opt='annot=[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]' \
		src/protobuf/$1

endef

define generate_for_typescript
	protoc \
		-I src/protobuf \
		--plugin="protoc-gen-ts=${PROTOC_GEN_TS_PATH}" \
		--js_out="import_style=commonjs,binary:${TS_OUT_DIR}" \
		--ts_out="${TS_OUT_DIR}" \
		src/protobuf/$1

endef

.PHONY: generate
generate:
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_ocaml,$f))
	mkdir -p $(TS_OUT_DIR)
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_typescript,$f))
