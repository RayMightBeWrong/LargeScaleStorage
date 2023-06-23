%%%-------------------------------------------------------------------
%% @doc data_interface public API
%% @end
%%%-------------------------------------------------------------------

-module(data_interface).
-export([create/0, read/2, put/3]).

%%%-------------------------------------------------------------------
%% para cada cliente será criado um contexto, que neste caso será um map.
%% o contexto terá de ser utilizado nas funções de get e put para verificar dependencias e versoes
%% mapa de chaves para versão -> temos de guardar as versões que já lemos para se saber quais são as dependencias de escrita
create() ->
	#{}.

%% @param KL -> List of keys
%% @param Cxt -> session's context
%% @returns {New context, list of key-value pairs}
read(KL, Cxt) ->
	ListValues = data_gateway:read_wait(KL),
	MapDependencies = dependencies_to_map(ListValues,#{}),
	ListDependencies = maps:to_list(MapDependencies),
	MapValuesGetted = values_to_map(ListValues, #{}),
	FinalValuesMap = check_deps(ListDependencies, MapValuesGetted),
	MapIterator = maps:iterator(FinalValuesMap),
	NewCxt = cxt_after_get(Cxt,maps:next(MapIterator)),
	ResultValues = format_values(maps:next(MapIterator)),
	{NewCxt, ResultValues}.

%% @param Key
%% @param Value
%% @param Cxt -> session's context
put(K, V, Cxt) ->
	ListDependencies = get_dependencies(Cxt),
	Ver = data_gateway:put_wait(K, V, ListDependencies),
	_NewCxt = maps:put(K,Ver,#{}).

%% -----------------------------------------------------------------
%% ------------------------- Aux Functions --------------------------

get_dependencies(Cxt) ->
	maps:to_list(Cxt).

%% function to convert all the dependencies obtained in the get operation to a map: Key -> Version 
dependencies_to_map([],DepMap) -> DepMap;
dependencies_to_map([ {_Key,{_Val,_Ver,Deps}} | T ], DepMap) ->
	NewDepMap = add_to_dep_map(DepMap,Deps),
	dependencies_to_map(T,NewDepMap).


add_to_dep_map(DepMap,[]) -> DepMap;
add_to_dep_map(DepMap, [{Key, Ver} | T]) ->
	case  maps:is_key(Key, DepMap) of
		true -> 
			OldVer = maps:get(Key,DepMap),
			if 
				Ver > OldVer ->
					NewDepMap = maps:put(Key,Ver,DepMap),
					add_to_dep_map(NewDepMap,T);
				true -> 
					add_to_dep_map(DepMap,T)

			end;
		false ->
			NewDepMap = maps:put(Key, Ver, DepMap),
			add_to_dep_map(NewDepMap, T)
	end.
			

%% function used to convert the values obtained in the get operation to a map: Key -> (value, version)
values_to_map([], ValuesMap) -> ValuesMap;
values_to_map([{Key, {Val, Ver, _}} | T ], ValuesMap) -> 
	NewValuesMap = maps:put(Key, {Val,Ver}, ValuesMap),
	values_to_map(T,NewValuesMap).
	


%% function used to check if there are any missing dependencies, and if there are, get them from de data server	
check_deps([],MapValues) -> MapValues;
check_deps([{Key,Ver}|T], MapValues) -> 
	case maps:is_key(Key,MapValues) of
		true -> 
			{_, VerGetted} = maps:get(Key,MapValues),
			if 
				VerGetted < Ver ->
					{NewValue, Version} = data_gateway:read_version_wait(Key,Ver),
					NewMapValues = maps:put(Key,{NewValue,Version},MapValues),
					check_deps(T,NewMapValues);
				true -> 
					check_deps(T,MapValues)
			end;
		false -> 
			check_deps(T,MapValues)
	end
	.

%% Create final context after get all the values and necessary dependencies 
cxt_after_get(Cxt, none) -> Cxt;
cxt_after_get(Cxt, {K,{_,V},I}) ->
	case maps:is_key(K,Cxt) of
		true -> 
			Value = maps:get(K,Cxt),
			if 
				V > Value -> 
					NewCxt = maps:put(K,V,Cxt),
					cxt_after_get(NewCxt, maps:next(I));
				true -> cxt_after_get(Cxt,maps:next(I))

			end;
		false ->
			NewCxt = maps:put(K,V,Cxt),
			cxt_after_get(NewCxt, maps:next(I))
	end.
		
format_values(none) -> [];
format_values({K,{Val,_},I}) -> [{K,Val} | format_values( maps:next(I) )].