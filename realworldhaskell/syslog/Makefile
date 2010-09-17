
% : %.hs
	ghc --make $@

PROGS := \
  syslogtcpd \
  tcpEg1 \

all: ${PROGS}

.PHONY: clean
clean:
	rm -f *.hi *.o *.out ${PROGS}
