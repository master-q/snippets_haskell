PROG := RpnCalcState

all: ${PROG}
${PROG}: ${PROG}.hs
	ghc -Wall -O2 ${PROG}.hs

lint:
	hlint -c *.hs

clean:
	rm -rf BatchedQueueTest
	rm -rf *.hi *.o *~

.PHONY: lint clean
