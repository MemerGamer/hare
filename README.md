<p align=center>
 <img src="./icons/Hare%20logo%20white.svg#gh-dark-mode-only" alt="Hare logo" style="width:300px;"/>
 <img src="icons/Hare%20logo%20black.svg#gh-light-mode-only" alt="Hare logo" style="width:300px;"/>
</p>

Hare is a HTTP server written in haskell, based on the [rabbit](https://github.com/MemerGamer/rabbit) HTTP server.

## Current state

- Currently it's only a static webserver
- After running the project (ex: `runhaskell Main.hs`) it displays the following message:
  - `Listening on: http://localhost:5050`
  - When clicked on the url it opens in the default browser
  - `Note`: The server constantly logs the requests to the terminal
- It can display static files located in the `sites` directory
- If there is no path specified after the port number it redirects to `site/index.html`
- If it cannot find a path or file it redirects to `sites/404.html`

## Future improvements

- [x] make it a dynamic webserver (it processes requests from the client)
- [ ] transpile haskell funtions to javascript

## Installation

### Clone the repository

```console
git clone https://github.com/MemerGamer/hare.git
cd hare/
```

### Install the dependencies

```console
cabal install --dependencies-only $(cat dependencies.txt)
```

### Run Hare

```console
runhaskell Main.hs
```
