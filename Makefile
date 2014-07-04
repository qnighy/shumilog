#!/usr/bin/make -f


.PHONY: all clean

all: shumilog

shumilog: shumilog.hs
	ghc --make -Wall $<

clean:
	$(RM) shumilog *.hi *.o
