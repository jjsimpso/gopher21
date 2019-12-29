SRC_FILES = trie.rkt file-search.rkt corpus.rkt gopher.rkt main.rkt
#CORPUS = <corpus file>
#CORPUS_CACHE = <corpus cache file>
#CORPUS_DIR = <root directory of corpus>

EXE = main
DIST_DIR = gopher21

all: gopher21

main: $(SRC_FILES)
	raco make -v main.rkt
	raco exe -v main.rkt

gopher21: $(EXE)
	raco distribute $(DIST_DIR) $(EXE)

corpus: gopher21
	gopher21/bin/main -c $(CORPUS) -d $(CORPUS_CACHE) $(CORPUS_DIR)

clean:
	rm -rf compiled/
	rm -rf $(EXE)
	rm -rf $(DIST_DIR)
#	rm $(CORPUS) $(CORPUS_CACHE) 