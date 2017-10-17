%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(disable_tco).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Ast, _Opt) ->
    ast_traverse:map(fun walk/2, Ast).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(post, {function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, walk_clauses(Clauses, {atom, Name})};
walk(post, {'fun', Line, {clauses, Clauses}}) ->    
    {'fun', Line, {clauses, walk_clauses(Clauses, undefined)}};
walk(post, {named_fun, Line, Name, Clauses}) ->
    {named_fun, Line, Name,  walk_clauses(Clauses, {var, Name})};
walk(_Type, Node) ->
    Node.

walk_clauses(Clauses, Name) ->
    lists:map(
      fun(Clause) ->
              walk_clause(Clause, Name)
          end, Clauses).

walk_clause({clause, Line, Patterns, Guards, Body}, Name) ->
    {clause, Line, Patterns, Guards, walk_body(Body, Name)}.

walk_body([{call, _Line, {Type, _Line1, FName}, _Args} = Rep], Name) ->
    if
        {Type, FName} == Name ->
            [Rep];
        true ->
            [add_try_catch(Rep)]
    end;
walk_body([{call, _Line, {remote, _Line1, _Module, _Function}, _Args} = Rep], _Name) ->
    [add_try_catch(Rep)];
walk_body([H|T], Name) ->
    [H|walk_body(T, Name)];
walk_body([], _Name) ->
    [].

%% TODO: variable name of 'Class' and 'Exceptions' should be generate by erl_syntax_lib:new_variable_name(Used).
add_try_catch({call, Line, _Fun, _Args} = Expr) ->
    {'try', Line, [Expr], [], 
     [{clause,Line, 
       [{tuple, Line, 
         [{var, Line, 'Class'}, {var, Line, 'Exception'}, {atom, Line, '_'}]}],
       [],
       [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, raise}}, 
         [{var, Line, 'Class'}, {var, Line, 'Exception'}, 
          {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, get_stacktrace}}, []}]}]}],
     []}.
