# `fingerd` Daemon

Final project of the Haskell Book

## Installation

- To build
  
```bash
stack build
```

- To intepret `Main` module for incremental development

```bash
$ stack ghci --main-is fingerd:exe:fingerd
```

## Sample Usage

After `stack build` successfully, to run `debug` server

```bash
$ # In a Terminal window
$ sudo `stack exec which debug`
$ # In other Terminal window
$ telnet localhost 79
$ # Type something here, it will be echoed in `debug` server stdout
```

```bash
$ # Ubuntu 20.04
$ finger mnhthng@localhost
Login: mnhthng
Name: mnhthng
Directory: /home/mnhthng
Shell: /bin/zsh 
```

## Side-notes 

- Source code formatted by [`brittany`](https://github.com/lspitzner/brittany). To specify `brittany` to be default code formatter with Haskell Language Server in VSCode, add this to `settings.json`:

```js
"haskell.formattingProvider": "brittany",
```