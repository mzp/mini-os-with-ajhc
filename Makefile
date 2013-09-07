all:
	$(MAKE) -C mini-os links
	$(MAKE) -C libjhcrts
	$(MAKE) -C mini-os

clean:
	$(MAKE) -C libjhcrts clean
	$(MAKE) -C mini-os clean
