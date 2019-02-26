# Tail Call Optimization 

erlang optimize tail call and treat it as inline function
but it will lost stack trace info in exception raises 

add this parse transform when you debug

    -compile({parse_transform, disable_tco}).

this parse transformer will trans

    b(Expression3, Expression4)
    
to

        try 
            b(Expression3, Expression4)
        catch
            Class:Exception ->
                erlang:raise(Class, Exception, erlang:get_stacktrace())
        end
    
in

    a(Patterns) -> 
        Expression1,
        Expression2,
        b(Expressions3, Expression4).
        
and

    fun(Patterns) ->
        Expression1,
        Expression2,
        b(Expression3, Expression4)
    end.
    
so you can get full stack trace in debug.

this parse_transform should only used in debug, it will cause performance issue.

# TODO

it will not transform 

    a() ->
        a().
        
but will transform

    a(T) ->
        b(T).
        
    b(T) ->
        a(T).
        
the cycle call should be detected and not transformed.
