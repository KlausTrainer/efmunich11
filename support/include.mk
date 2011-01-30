## -*- makefile -*-

APP_NAME := term_cache
DOC_FILES := *.html edoc-info erlang.png stylesheet.css

######################################################################
## Erlang

EMULATOR := beam
ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifdef DEBUG
  ERLC_FLAGS += $(DEBUG)
endif

EBIN_DIR := ../ebin
DOC_DIR  := ../doc

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_DOCUMENTS := $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html)
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
APPUP_FILES := $(wildcard *.appup)
EBIN_FILES = $(ERL_OBJECTS) $(ERL_DOCUMENTS) $(APP_FILES:%.app=../ebin/%.app) $(APPUP_FILES:%.appup=../ebin/%.appup)
EBIN_FILES_NO_DOCS = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app) $(APP_FILES:%.app=../ebin/%.app) $(APPUP_FILES:%.appup=../ebin/%.appup)
MODULES = $(ERL_SOURCES:%.erl=%)

../ebin/%.app: %.app
	cp $< $@

../ebin/%.appup: %.appup
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<

$(DOC_DIR)/%.html: %.erl
	$(ERL) -noshell -run edoc_run application "'$(APP_NAME)'" '".."' '"[]"'
