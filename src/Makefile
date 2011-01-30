include ../support/include.mk


all: $(EBIN_DIR) $(DOC_DIR) $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=+debug_info

clean:
	rm -rf $(EBIN_DIR) $(DOC_DIR)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(DOC_DIR):
	mkdir -p $(DOC_DIR)
