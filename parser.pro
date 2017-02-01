:- module(parser).
:- export([parse/2]).

parse_comment_list(Tokens, RestTokens, Comments) :-
  parse_comment_list_helper(Tokens, [], RestTokens, Comments).

parse_comment_list_helper(Tokens, Buffer, RestTokens, Comments) :-
  [comment(Comment)|Tokens1] = Tokens ->
    parse_comment_list_helper(Tokens1,
                              [Comment|Buffer], RestTokens, Comments) ;
  [delimiter('\n')|Tokens1] = Tokens ->
    parse_comment_list_helper(Tokens1, Buffer, RestTokens, Comments) ;
  RestTokens = Tokens,
  reverse(Buffer, Comments).

%%% ISO 7185  6.1.6 Labels

parse_label(Tokens, RestTokens, Label) :-
  [number(Number)|RestTokens] = Tokens,
  Label = label(Number).


%%% ISO 7185  6.2.1 Blocks

parse_block(Tokens, RestTokens, Block) :-
  parse_label_declaration_part(Tokens, Tokens1, Labels),
  parse_constant_definition_part(Tokens1, Tokens2, Constants),
  parse_type_definition_part(Tokens2, Tokens3, Types),
  parse_variable_declaration_part(Tokens3, Tokens4, Variables),
  parse_procedure_and_function_declaration_part(Tokens4,
						Tokens5,
						ProcedureAndFunctions),
  parse_statement_part(Tokens5, RestTokens, Statements),
  Block = block(Labels,
		Constants,
		Types,
		Variables,
		ProcedureAndFunctions,
		Statements).

parse_label_declaration_part([Token|Tokens], RestTokens, Labels) :-
  Token = keyword(label) ->
    parse_label(Tokens, Tokens1, Label),
    parse_label_declaration_part_helper(Tokens1, [Label], RestTokens, Labels) ;
  RestTokens = [Token|Tokens],
  Labels = labels([]).

parse_label_declaration_part_helper([Token|Tokens],
				    Buffer, RestTokens, Labels) :-
  Token = delimiter(',') ->
    parse_label(Tokens, Tokens1, Label),
    parse_label_declaration_part_helper(Tokens1,
					[Label|Buffer], RestTokens, Labels) ;
  Token = delimiter(';') ->
    RestTokens = Tokens,
    reverse(Buffer, Reversed),
    Labels = labels(Reversed).

parse_constant_definition_part([Token|Tokens], RestTokens, Constants) :-
  Token = keyword(const) ->
    parse_constant_definition_part_helper(Tokens,
					  [], RestTokens, Constants) ;
  RestTokens = [Token|Tokens],
  Constants = constants([]).

parse_constant_definition_part_helper([Token|Tokens],
				      Buffer, RestTokens, Constants) :-
  Token = identifier(Identifier) ->
    parse_constant_definition([Token|Tokens], Tokens1, Constant),
    [delimiter(';')|Tokens2] = Tokens1,
    parse_constant_definition_part_helper(Tokens2,
					  [Constant|Buffer],
					  RestTokens,
					  Constants) ;
  RestTokens = Tokens,
  reverse(Buffer, Reversed),
  Constants = constants(Reversed).

parse_type_definition_part([Token|Tokens], RestTokens, Types) :-
  Token = keyword(type) ->
    parse_type_definition_part_helper(Tokens, [], RestTokens, Types) ;
  RestTokens = [Token|Tokens],
  Types = types([]).

parse_type_definition_part_helper([Token|Tokens], Buffer, RestTokens, Types) :-
  Token = identifier(Identifier) ->
    parse_type_definition([Token|Tokens], Tokens1, Type),
    [delimiter(';')|Tokens2] = Tokens1,
    parse_type_definition_part_helper(Tokens2,
				      [Type|Buffer], RestTokens, Types) ;
  RestTokens = Tokens,
  reverse(Buffer, Reversed),
  Types = types(Reversed).

parse_variable_declaration_part([Token|Tokens], RestTokens, Variables) :-
  Token = keyword(var) ->
    parse_variable_declaration_part_helper(Tokens, [], RestTokens, Variables) ;
  RestTokens = [Token|Tokens],
  Variables = variables([]).

parse_variable_declaration_part_helper([Token|Tokens],
				      Buffer, RestTokens, Variables) :-
  Token = identifier(Identifier) ->
    parse_variable_declaration([Token|Tokens], Tokens1, Variable),
    [delimiter(';')|Tokens2] = Tokens1,
    parse_variable_declaration_part_helper(Tokens2,
                                           [Variable|Buffer],
                                           RestTokens,
                                           Variables) ;
  RestTokens = Tokens,
  reverse(Buffer, Reversed),
  Variables = variables(Reversed).

parse_procedure_and_function_declaration_part(Tokens,
					      RestTokens,
					      ProcedureAndFunctions) :-
  parse_procedure_and_function_declaration_part_helper(Tokens,
						       [],
						       RestTokens,
						       ProcedureAndFunctions).

parse_procedure_and_function_declaration_part_helper([Token|Tokens],
						     Buffer,
						     RestTokens,
						     ProcedureAndFunctions) :-
  Token = keyword(procedure) ->
    parse_procedure_declaration([Token|Tokens], Tokens1, Procedure),
    parse_procedure_and_function_declaration_part_helper(
	Tokens1, [Procedure|Buffer], Tokens2, ProcedureAndFunctions),
    [delimiter(';')|RestTokens] = Tokens2 ;
  Token = keyword(function) ->
    parse_function_declaration([Token|Tokens], Tokens1, Function),
    parse_procedure_and_function_declaration_part_helper(
	Tokens1, [Function|Buffer], Tokens2, ProcedureAndFunctions),
    [delimiter(';')|RestTokens] = Tokens2 ;
  reverse(Buffer, ProcedureAndFunctions),
  RestTokens = [Token|Tokens].

parse_statement_part(Tokens, RestTokens, Statements) :-
  parse_compound_statement(Tokens, RestTokens, Statements).


%%% ISO 7185  6.8.3 Structured-statements

parse_statement_sequence(Tokens, RestTokens, Statements) :-
  parse_statement_sequence_helper(Tokens, [], RestTokens, Statements).

parse_statement_sequence_helper(Tokens, Buffer, RestTokens, Statements) :-
  parse_statement(Tokens, Tokens1, Statement) ->
    ([delimiter(';')|Tokens2] = Tokens1 ->
       parse_statement_sequence_helper(Tokens2,
				       [Statement|Buffer],
				       RestTokens,
				       Statements) ;
     RestTokens = Tokens1,
     reverse(Buffer, Reversed),
     Statements = statement_sequence(Reversed)) ;
  RestTokens = Tokens,
  reverse(Buffer, Reversed),
  Statements = statement_sequence(Reversed).

parse_compound_statement(Tokens, RestTokens, Statements) :-
  [keyword('begin')|Tokens1] = Tokens,
  parse_statement_sequence(Tokens1, Tokens2, Statements),
  [keyword('end')|RestTokens] = Tokens2.

%%% ISO 7185  6.10

parse_program(Tokens, RestTokens, Program) :-
  parse_comment_list(Tokens, Tokens1, Prologues),
  parse_program_heading(Tokens1, Tokens2, Heading),
  [delimiter(';')|Tokens3] = Tokens2,
  parse_program_block(Tokens2, Tokens3, Block),
  [delimiter('.')|Tokens4] = Tokens3,
  parse_comment_list(Tokens4, RestTokens, Postludes),
  Program = program(Heading,
                    Block,
                    comments(prologues(Prologues), postludes(Postludes))).

parse_program_heading(Tokens, RestTokens, Heading) :-
  [keyword(program)|Tokens1] = Tokens,
  [Name|Tokens2] = Tokens1,
  ([delimiter('(')|Tokens3] = Tokens2 ->
     parse_program_parameter_list(Tokens3, Tokens4, ParameterList),
     [delimiter(')')|RestTokens] = Tokens4 ;
   ParameterList = parameter_list([]),
   RestTokens = Tokens2),
  Heading = program_heading(program_name(Name), ParameterList).

parse_program_parameter_list(Tokens, RestTokens, ParameterList) :-
  parse_program_parameter_list_helper(Tokens, [], RestTokens, ParameterList).

parse_program_parameter_list_helper([Token|Tokens],
				    Buffer, RestTokens, ParameterList) :-
  Token = delimiter(')') ->
    RestTokens = [Token|Tokens],
    reverse(Buffer, Reversed),
    ParameterList = program_parameter_list(Reversed) ;
  parse_program_parameter_list_helper(Tokens,
                                      [Token|Buffer],
                                      RestTokens, ParameterList).

parse_program_block(Tokens, RestTokens, Block) :-
  parse_block(Tokens, RestTokens, Block).


parse(Tokens, Tree) :-
  parse_program(Tokens, RestTokens, Tree).