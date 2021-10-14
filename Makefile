UTILS_PATH := build_utils
TEMPLATES_PATH := .

SERVICE_NAME := service_erlang

#ToDo: Use the latest build image tag
BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 117a2e28e18d41d4c3eb76f5d00af117872af5ac

CALL_W_CONTAINER := all gen_service gen_library submodules clean

all: gen

-include $(UTILS_PATH)/make_lib/utils_container.mk

.PHONY: $(CALL_W_CONTAINER) add_template

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

~/.config/rebar3/templates:
	mkdir -p ~/.config/rebar3/templates
	cp -rv ./* ~/.config/rebar3/templates

add_template: ~/.config/rebar3/templates

gen_service: add_template
	rebar3 new erlang-service name=snakeoil

gen_library: add_template
	rebar3 new erlang-service name=trickster

clean:
	rm Dockerfile
