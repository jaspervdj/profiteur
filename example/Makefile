binary-trees.prof.html: binary-trees.prof
	profiteur binary-trees.prof

binary-trees.prof: binary-trees
	./binary-trees 10 +RTS -p -RTS

binary-trees:
	ghc --make -auto-all -prof -rtsopts binary-trees.hs

clean:
	rm -f binary-trees
	rm -f binary-trees.prof
	rm -f binary-trees.prof.html

.PHONY: clean
