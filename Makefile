#!/usr/bin/make -f


.PHONY: all clean

all: shumilog

shumilog: *.hs
	ghc --make -Wall shumilog.hs

clean:
	$(RM) shumilog *.hi *.o
