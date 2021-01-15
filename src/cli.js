var Elm = require('../runtime/main').Elm;
var fs = require('fs/promises');
var os = require('os');


// Get data from the command line
var args = process.argv.slice(2);

var main = Elm.Main.init({ flags: args });

// Get output from the worker
main.ports.put.subscribe(function(output) {
  console.log(output + "\n");
});

process.chdir(os.homedir());

main.ports.fsRequest.subscribe(({method, args}) => {
    fs[method](...args)
        .then(main.ports.fsResponse.send)
        .catch(console.error);
});