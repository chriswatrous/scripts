// Your init script
//
// Atom will evaluate this file each time a new window is opened. It is run
// after packages are loaded/activated and after the previous editor state
// has been restored.
//

atom.commands.add('atom-workspace', 'proto-repl-custom:switch-to-shadow-cljs-app-repl', () =>
  protoRepl.executeCodeInNs('(shadow.cljs.devtools.api/nrepl-select :dev)', {allSessions: true}));

atom.commands.add('atom-workspace', 'proto-repl-custom:exit-cljs-repl', () =>
  protoRepl.executeCodeInNs(':cljs/quit', {allSessions: true}));

// protoRepl.executeCodeInNs('(shadow.cljs.devtools.api/nrepl-select :dev)', {allSessions: true});
