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

OCAML_OUT_DIR=src/ocaml/generated
# Directory to write generated code to (.js and .d.ts files)
TS_OUT_DIR=./src/ts/generated
# Path to this plugin
PROTOC_GEN_TS_PATH="./node_modules/.bin/protoc-gen-ts"

PROTO_FILE_DEPS += bookmark
PROTO_FILE_DEPS += completer
PROTO_FILE_DEPS += configuration
PROTO_FILE_DEPS += filer
PROTO_FILE_DEPS += keymap
PROTO_FILE_DEPS += theme
PROTO_FILE_DEPS += types
PROTO_FILE_DEPS += service

define generate_for_ocaml
	protoc -I src/protobuf --ocaml_out=$(OCAML_OUT_DIR) \
		--ocaml_opt='singleton_record=true;annot=[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]' \
		src/protobuf/$1.proto

endef

define generate_for_typescript
	 protoc -I src/protobuf \
		--plugin="protoc-gen-ts=${PROTOC_GEN_TS_PATH}" \
		--js_out="import_style=commonjs,binary:${TS_OUT_DIR}" \
		--ts_out="${TS_OUT_DIR}" \
		src/protobuf/$1.proto

endef

.PHONY: generate
generate:
	$(foreach f,$(PROTO_FILE_DEPS), $(rm $(OCAML_OUT_DIR)/$f.ml))
	mkdir -p $(OCAML_OUT_DIR)
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_ocaml,$f))

	rm -rf $(TS_OUT_DIR)
	mkdir -p $(TS_OUT_DIR)
	$(foreach f,$(PROTO_FILE_DEPS),$(call generate_for_typescript,$f))

.PHONY: build-linux build-windows

build-linux:
	scripts/build.sh linux
	npm install -g yarn
	yarn
	yarn build
	yarn package linux

build-windows:
	scripts/build.sh windows
	npm install -g yarn
	yarn
	yarn build
	yarn package win32

.PHONY: build-builder build-builder-alpine build-builder-debian build-packager-debian

build-builder-alpine:
	docker build -t sxfiler-builder:alpine -f misc/Dockerfile.builder.alpine .

build-builder-debian:
	docker build -t sxfiler-builder:debian -f misc/Dockerfile.builder.debian .

build-packager-debian:
	docker build -t sxfiler-packager:debian -f misc/Dockerfile.packager .
