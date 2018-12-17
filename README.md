## Installation

    $ curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein
    $ sudo mkdir -p /usr/local/bin/
    $ sudo mv lein /usr/local/bin/lein
    $ sudo chmod a+x /usr/local/bin/lein
    $ export PATH=$PATH:/usr/local/bin

## Usage

À partir da pasta do projeto:

    $ lein deps
    $ lein repl
    comp_clojure_p3$ (teste x)

Este teste compila o programa: 
    let fn teste(x,y,z,v,w) = 
                            let var y = x 
                                in {
                                    y := true
                                } 
                            in {
                                teste(1,2,3*45,false,7)
                            }

Para testar outros códigos o comando é:

    $ comp_clojure_p3$ (teste (calc "código"))
    
Exemplos: 
    Fatorial
    
    (teste (calc "let var z = 1 in{ let var true = 10 in {while (not(true==0)) do {z:=z*true; true:= true-1}}}"))    
    (teste (calc "let var z = 0 in { let fn f(x, y) = z := x + y in { f(10, 20) }}"))

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
