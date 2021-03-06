// Your init script
//
// Atom will evaluate this file each time a new window is opened. It is run
// after packages are loaded/activated and after the previous editor state
// has been restored.
//

atom.commands.add('atom-workspace', 'proto-repl-custom:switch-to-shadow-cljs-app-repl', () =>
  protoRepl.eval('(shadow.cljs.devtools.api/nrepl-select :dev)'));

atom.commands.add('atom-workspace', 'proto-repl-custom:exit-cljs-repl', () =>
  protoRepl.eval(':cljs/quit'));
