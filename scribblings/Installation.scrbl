#lang scribble/manual

@(require scribble-tools)

@title{Installation}

Set up WebRacket for both terminal and browser use.

@section{Prerequisites}

You need:

@itemlist[
  @item{@tt{wasm-tools} (Bytecode Alliance) v1.243.0 or newer}
  @item{@tt{node} with support for @tt{--experimental-wasm-exnref}}
  @item{Racket 9.0 or newer}
  @item{@tt{raco-static-web}}
  @item{A local clone of the WebRacket repository}
]

@section{Installation Steps}

@bold{1. Install wasm-tools}

Download the latest release:
@url{https://github.com/bytecodealliance/wasm-tools/releases}

Extract and put it on your @tt{PATH}, then verify:

@shellblock{
tar -xvf wasm-tools-1.243.0-aarch64-macos.tar.gz
sudo mv wasm-tools /usr/local/bin/
wasm-tools
}

On macOS, if quarantine blocks execution:

@shellblock{
sudo xattr -d com.apple.quarantine /usr/local/bin/wasm-tools
}

@bold{2. Install Node.js}

Download:
@url{https://nodejs.org/en/download}

Verify Node and required flags:

@shellblock{
node
node --experimental-wasm-exnref --expose-gc
}

@bold{3. Install Racket}

Install Racket 9.0 or newer:
@url{https://download.racket-lang.org/}

@bold{4. Install raco-static-web}

@shellblock{
raco pkg install raco-static-web
raco static-web
}

@bold{5. Install nanopass}

@shellblock{
raco pkg install nanopass
}

@bold{6. Clone WebRacket}

@shellblock{
git clone https://github.com/soegaard/webracket.git
}

@bold{7. Install the local checkout as package @tt{webracket}}

Run in the root of the cloned @tt{webracket} repository:

@shellblock{
raco pkg install --auto --name webracket .
}

@bold{8. Quick test}

Serve examples and open one in your browser:

@shellblock{
cd examples
raco static-web
}

Then open:
@url{http://localhost:8000/}
