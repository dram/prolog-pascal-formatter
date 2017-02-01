:- initialization(main).

:- use_module(formatter).
:- use_module(lexer).
:- use_module(parser).

read_source(File, List) :- sys:load_file(File, Atom), atom_chars(Atom, List).

write_source(File, List) :- atom_chars(Atom, List), sys:save_file(File, Atom).

main :-
  writeln('Run lexer.'),
  sys:getenv('IN', InputFile),
  read_source(InputFile, Input),
  lexer:analyze(Input, TokenList),

  writeln('Run parser.'),
  parser:parse(TokenList, SyntaxTree),

  writeln('Run formatter.'),
  formatter:format(SyntaxTree, Output),
  sys:getenv('OUT', OutputFile),
  write_source(OutputFile, Output),

  halt.