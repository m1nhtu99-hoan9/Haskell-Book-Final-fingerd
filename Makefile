build:
	stack build

debug: build
	stack exec sudo debug

ghci-fingerd: 
	stack ghci --main-is fingerd:exe:fingerd

ghci-debug: 
	stack ghci --main-is fingerd:exe:debug

init: build 
	stack exec fingerd init

run: build
	stack exec sudo fingerd run