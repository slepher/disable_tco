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
    ast_traverse:map_with_state(fun walk/3, sets:new(), Ast).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre,  {function, _Line, _Name, _Arity, _Clauses} = Function, _Variables) ->
    Variables = ast_traverse:reduce(fun walk_variables/3, sets:new(), Function),
    {Function, Variables};
walk(post, {function, Line, Name, Arity, Clauses}, Variables) ->
    {NClauses, NVariables} = walk_clauses(Clauses, {atom, Name}, Variables),
    {{function, Line, Name, Arity, NClauses}, NVariables};
walk(post, {'fun', Line, {clauses, Clauses}}, Variables) ->  
    {NClauses, NVariables} = walk_clauses(Clauses, undefined, Variables),
    {{'fun', Line, {clauses, NClauses}}, NVariables};
walk(post, {named_fun, Line, Name, Clauses}, Variables) ->
    {NClauses, NVariables} = walk_clauses(Clauses, {atom, Name}, Variables),
    {{named_fun, Line, Name, NClauses}, NVariables};
walk(_Type, Node, Variables) ->
    {Node, Variables}.

walk_variables(leaf, {var, _Line, Name}, Variables) ->
    sets:add_element(Name, Variables);
walk_variables(_Type, _Node, Variables) ->
    Variables.

walk_clauses(Clauses, Name, Variables) ->
    {NClauses, NVaraibles} = 
        lists:foldl(
          fun(Clause, {CAcc, VAcc}) ->
                  {NClause, NVAcc} = walk_clause(Clause, Name, VAcc),
                  {[NClause|CAcc], NVAcc}
          end, {[], Variables}, Clauses),
    {lists:reverse(NClauses), NVaraibles}.

walk_clause({clause, Line, Patterns, Guards, Body}, Name, Variables) ->
    {NBody, NVariables} = walk_body(Body, Name, Variables),
    {{clause, Line, Patterns, Guards, NBody}, NVariables}.

walk_body([{call, _Line, {Type, _Line1, FName}, _Args} = Rep], Name, Variables) ->
    if
        {Type, FName} == Name ->
            {[Rep], Variables};
        true ->
            {NRep, NVariables} = add_try_catch(Rep, Variables), 
            {[NRep], NVariables}
    end;
walk_body([{call, _Line, {remote, _Line1, _Module, _Function}, _Args} = Rep], _Name, Variables) ->
    {NRep, NVariables} = add_try_catch(Rep, Variables),
    {[NRep], NVariables};
walk_body([H|T], Name, Variables) ->
    {NT, NVariables} = walk_body(T, Name, Variables),
    {[H|NT], NVariables};
walk_body([], _Name, Variables) ->
    {[], Variables}.

-ifdef('OTP_RELEASE').
add_try_catch({call, Line, _Fun, _Args} = Expr, Variables) ->
    Class = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Class" ++ integer_to_list(N)) end, Variables),
    Exception = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Exception" ++ integer_to_list(N)) end, Variables),
    StackTrace = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("StackTrace" ++ integer_to_list(N)) end, Variables),

    NVariables = sets:union(sets:from_list([Class, Exception, StackTrace]), Variables),

    {{'try', Line, [Expr], [], 
      [{clause,Line, 
        [{tuple, Line, 
          [{var, Line, Class}, {var, Line, Exception}, {var, Line, StackTrace}]}],
        [],
        [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, raise}}, 
          [{var, Line, Class}, {var, Line, Exception}, {var, Line, StackTrace}]
         }]}],
      []}, NVariables}.
-else.
add_try_catch({call, Line, _Fun, _Args} = Expr, Variables) ->
    Class = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Class" ++ integer_to_list(N)) end, Variables),
    Exception = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Exception" ++ integer_to_list(N)) end, Variables),

    NVariables = sets:union(sets:from_list([Class, Exception]), Variables),

    {{'try', Line, [Expr], [], 
      [{clause,Line, 
        [{tuple, Line, 
          [{var, Line, Class}, {var, Line, Exception}, {atom, Line, '_'}]}],
        [],
        [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, raise}}, 
          [{var, Line, Class}, {var, Line, Exception}, 
           {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, get_stacktrace}}, []}]}]}],
      []}, NVariables}.
-endif.
