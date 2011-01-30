all:
	(cd src;$(MAKE))

debug:
	(cd src;$(MAKE) debug)

clean:
	rm -f erl_crash.dump
	(cd src;$(MAKE) clean)
