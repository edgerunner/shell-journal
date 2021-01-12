var Elm = require('../runtime/main').Elm;


// Get data from the command line
var args = process.argv.slice(2);

var main = Elm.Main.init({ flags: args });

// Get output from the worker
main.ports.put.subscribe(function(output) {
  console.log(output + "\n");
});