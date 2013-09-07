all:
	$(MAKE) -C mini-os links
	$(MAKE) -C libjhcrts
	$(MAKE) -C mini-os
