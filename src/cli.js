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

main.ports.fs.subscribe(function({method, path, data}) {
    path = path.replace(/^~/, os.homedir())
    switch (method) {
        case "read":
        case "edit":
            fs.readFile(path, { encoding: "utf8" })
            .then(body => main.ports.read.send({body, data, method}))
            .catch(console.error);

            break;
        case "append":
            fs.appendFile(path, data, { encoding: "utf8" })
            .catch(console.error);
            break;
        default:
            break;
    }
});