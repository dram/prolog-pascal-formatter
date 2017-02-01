:- module(lexer).
:- export([analyze/2]).

%%% ISO 7185  6.1 Lexical tokens
get_token([Char|Chars], RestChars, Token) :-

  %% ISO 7185  6.1.5 Numbers
  Char = '+', [Char1|Chars1] = Chars, is_digit(Char1) ->
    get_number(Chars1, [Char1, '+'], RestChars, Number),
    Token = number(Number) ;

  Char = '+' ->
    RestChars = Chars,
    Token = operator('+') ;

  %% ISO 7185  6.1.5 Numbers
  Char = '-', [Char1|Chars1] = Chars, is_digit(Char1) ->
    get_number(Chars1, [Char1, '-'], RestChars, Number),
    Token = number(Number) ;

  Char = '-' ->
    RestChars = Chars,
    Token = operator('-') ;

  Char = '*' ->
    RestChars = Chars,
    Token = operator('*') ;

  Char = '/' ->
    RestChars = Chars,
    Token = operator('/') ;

  Char = '=' ->
    RestChars = Chars,
    Token = operator('=') ;

  Char = '<', [Char1|Chars1] = Chars, Char1 = '=' ->
    RestChars = Chars1,
    Token = operator('<=') ;

  Char = '<', [Char1|Chars1] = Chars, Char1 = '>' ->
    RestChars = Chars1,
    Token = operator('<>') ;

  Char = '<' ->
    RestChars = Chars,
    Token = operator('<') ;

  Char = '>', [Char1|Chars1] = Chars, Char1 = '=' ->
    RestChars = Chars1,
    Token = operator('>=') ;

  Char = '>' ->
    RestChars = Chars,
    Token = operator('>') ;

  Char = '[' ->
    RestChars = Chars,
    Token = delimiter('[') ;

  Char = ']' ->
    RestChars = Chars,
    Token = delimiter(']') ;

  Char = '.', [Char1|Chars1] = Chars, Char1 = '.' ->
    RestChars = Chars1,
    Token = operator('..') ;

  Char = '.' ->
    RestChars = Chars,
    Token = delimiter('.') ;

  Char = ',' ->
    RestChars = Chars,
    Token = delimiter(',') ;

  Char = ':', [Char1|Chars1] = Chars, Char1 = '=' ->
    RestChars = Chars1,
    Token = operator(':=') ;

  Char = ':' ->
    RestChars = Chars,
    Token = delimiter(':') ;

  Char = ';' ->
    RestChars = Chars,
    Token = delimiter(';') ;

  Char = '^' ->
    RestChars = Chars,
    Token = operator('^') ;

  Char = '(', [Char1|Chars1] = Chars, Char1 = '*' ->
    get_comment(Chars1, RestChars, Token) ;

  Char = '(' ->
    RestChars = Chars,
    Token = delimiter('(') ;

  Char = ')' ->
    RestChars = Chars,
    Token = delimiter(')') ;

  Char = '\'' ->
    get_string(Chars, RestChars, Token) ;

  Char = '{' ->
    get_comment(Chars, RestChars, Token) ;

  Char = ' ' ->
    get_token(Chars, RestChars, Token) ;

  Char = '\t' ->
    get_token(Chars, RestChars, Token) ;

  Char = '\r' ->
    get_token(Chars, RestChars, Token) ;

  %% Newline is parsed as a seperate token, used to determine position
  %% of comments.
  Char = '\n' ->
    RestChars = Chars,
    Token = delimiter('\n') ;

  is_digit(Char) ->
    get_number(Chars, [Char], RestChars, Number),
    Token = number(Number) ;

  is_letter(Char) ->
    get_identifier(Chars, [Char], RestChars, Identifier),
    (is_keyword(Identifier) ->
       Token = keyword(Identifier) ;
       Token = identifier(Identifier)).

%%% ISO 7185  6.1.1 General
is_digit(Char) :-
  char_code(Char, Code), Code >= 0x30, Code =< 0x39.

%%% UTF-8 characters is also included.
is_letter(Char) :-
  char_code(Char, Code),
  (Code >= 0x41, Code =< 0x5A ; Code >= 0x61, Code =< 0x7A ; Code >= 0x80).

%%% ISO 7185  6.1.2 Special-symbols
is_keyword(and). is_keyword(array).
is_keyword(begin).
is_keyword(case). is_keyword(const).
is_keyword('div'). is_keyword(do). is_keyword(downto).
is_keyword(else). is_keyword(end).
is_keyword(file). is_keyword(for). is_keyword(function).
is_keyword(goto).
is_keyword(if). is_keyword(in).
is_keyword(label).
is_keyword('mod').
is_keyword(nil). is_keyword(not).
is_keyword(of). is_keyword(or).
is_keyword(packed). is_keyword(procedure). is_keyword(program).
is_keyword(record). is_keyword(repeat).
is_keyword(set).
is_keyword(then). is_keyword(to). is_keyword(type).
is_keyword(until).
is_keyword(var).
is_keyword(while). is_keyword(with).


%%% ISO 7185  6.1.3 Identifiers
get_identifier([Char|Chars], Buffer, RestChars, Identifier) :-
  (is_letter(Char) ; is_digit(Char)) ->
    get_identifier(Chars, [Char|Buffer], RestChars, Identifier) ;
  RestChars = [Char|Chars],
  reverse(Buffer, Reversed),
  atom_chars(Atom, Reversed),
  sys:lower(Atom, Identifier).


%%% ISO 7185  6.1.5 Numbers
get_number([Char|Chars], Buffer, RestChars, Number) :-
  (Char = '.' ; Char = 'e' ; Char = 'E' ; is_digit(Char)) ->
    get_number(Chars, [Char|Buffer], RestChars, Number) ;
  RestChars = [Char|Chars],
  reverse(Buffer, Reversed),
  atom_chars(Atom, Reversed),
  sys:upper(Atom, Number).


%%% ISO 7185  6.1.7 Character-strings
get_string(Chars, RestChars, String) :-
  get_string_helper(Chars, [], RestChars, String).

get_string_helper([Char|Chars], Buffer, RestChars, String) :-
  Char = '\'', [Char1|Chars1] = Chars, Char1 = '\'' ->
    get_string_helper(Chars1, [Char1|Buffer], RestChars, String) ;
  Char = '\'' ->
    RestChars = Chars,
    reverse(Buffer, Reversed),
    atom_chars(Atom, Reversed),
    String = string(Atom) ;
  get_string_helper(Chars, [Char|Buffer], RestChars, String).


%%% ISO 7185  6.1.8 Token separators
get_comment(Chars, RestChars, Comment) :-
  get_comment_helper(Chars, [], RestChars, Comment).

get_comment_helper([Char|Chars], Buffer, RestChars, Comment) :-
  %% Skip '\r' characters.
  Char = '\r' ->
    get_comment_helper(Chars, Buffer, RestChars, Comment) ;
  Char = '}' ->
    RestChars = Chars,
    reverse(Buffer, Reversed),
    atom_chars(Atom, Reversed),
    Comment = comment(Atom) ;
  Char = '*', [Char1|Chars1] = Chars, Char1 = ')' ->
    RestChars = Chars1,
    reverse(Buffer, Reversed),
    atom_chars(Atom, Reversed),
    Comment = comment(Atom) ;
  get_comment_helper(Chars, [Char|Buffer], RestChars, Comment).


%%% Analyze all tokens.
get_tokens(Chars, Buffer, Tokens) :-
  Chars = [] ->
    reverse(Buffer, Tokens) ;
  get_token(Chars, RestChars, Token),
  (Token \= comment(_), [delimiter('\n')|Buffer1] = Buffer ->
     %% Remove newlines before non-comment tokens.
     get_tokens(RestChars, [Token|Buffer1], Tokens) ;
   get_tokens(RestChars, [Token|Buffer], Tokens)).


analyze(Input, Tokens) :-
  get_tokens(Input, [], Tokens).