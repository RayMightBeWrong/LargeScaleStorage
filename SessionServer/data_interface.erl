%%%-------------------------------------------------------------------
%% @doc data_interface public API
%% @end
%%%-------------------------------------------------------------------

-module(data_interface).
-export([createDB/0,create/0, get/2, put/3, ,get_next_version/2]).

%%%-------------------------------------------------------------------
%% coisas do rebar3 

start(_StartType, _StartArgs) ->
	data_interface_sup:start_link().

stop(_State) -> 
	ok.



%%%-------------------------------------------------------------------
%% para cada cliente será criado um contexto, que neste caso será um map.
%% o contexto terá de ser utilizado nas funções de get e put para verificar dependencias e versoes
%% mapa de chaves para versão -> temos de guardar as versões que já lemos para se saber quais são as dependencias de escrita
create() ->
	#{}.

get(KL, Cxt) -> 
	ListValues = get_most_recent_values(KL),
	MapDependencies = dependencies_to_map(ListValues,#{}),
	ListDependencies = maps:to_list(MapDependencies),
	MapValuesGetted = values_to_map(ListValues, #{}),
	FinalValuesMap = check_deps(ListDependencies, MapValuesGetted),
	MapIterator = maps:iterator(FinalValuesMap),
	NewCxt = cxt_after_get(Cxt,maps:next(MapIterator)),
	ResultValues = format_values(maps:next(MapIterator)),
	{NewCxt, ResultValues}
	.

put(K, V, Cxt) ->
	ListDependencies = get_dependences(Cxt),
	demoDB ! {put, K, V, ListDependencies, self()}, %% this will be replaced with proper comunication with data servers
	Ver = get_version(),
	NewCxt = maps:put(K,Ver,#{})
	.

%% -----------------------------------------------------------------
%% ------------------------- Aux Function --------------------------



get_version() ->
	receive
		{ver,Ver} -> Ver %% TODO -> needs to be replaced with communication with data servers
	end.

get_dependences(Cxt) ->
	maps:to_list(Cxt).


get_most_recent_values([]) -> [];
get_most_recent_values([K | T]) ->
	demoDB ! {get, K, self()}, %% TODO -> needs to be replaced with communication with data servers
	receive
		{get_res, K, MR} -> [{K,MR} | get_most_recent_values(T)]	
	end.


%% function to convert all the dependencies obtained in the get operation to a map: Key -> Version 
dependencies_to_map([],DepMap) -> DepMap;
dependencies_to_map([ {Key,{Val,Ver,Deps}} | T ], DepMap) -> 
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
					{NewValue, Version} = get_version(Key,Ver),
					NewMapValues = maps:put(Key,{NewValue,Version},MapValues),
					check_deps(T,NewMapValues);
				true -> 
					check_deps(T,MapValues)
			end;
		false -> 
			check_deps(T,MapValues)
	end
	.



get_verion(Key,Version) -> 
	demoDB ! {get_ver, Key, Version, self()}, %% TODO -> needs to be replaced with communication with data servers
	receive
		{get_res, K, Value, Ver} -> {Value,Ver}
	end.

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

%% -----------------------------------------------------------------
%% ------------------------------ DEMO DB --------------------------
%% -----------------------------------------------------------------

%% dummy db for testing purposes

createDB() ->
	register(demoDB, spawn(fun() -> dummyDB(#{}) end ) ).



dummyDB(DB) ->
	receive
		{put, K, V, Deps, PID} ->  
			case maps:is_key(K, DB) of
				true ->
					LV = maps:get(K,DB),
					Ver = get_next_version(0,LV),
					NewDB = maps:put(K,[{V,Ver+1,Deps}|LV],DB),
					PID ! {ver, Ver+1},
					dummyDB(NewDB);
				false -> 
					NewDB = maps:put(K,[{V,0,Deps}],DB),
					PID ! {ver,0},
					dummyDB(NewDB)
			end;
		{get, K, PID} ->
			[MR | R] = maps:get(K,DB),
			PID ! {get_res,K ,MR},
			dummyDB(DB);

		{get_ver, K, Ver, PID} -> 
			ListVersions = maps:get(K,DB),
			Value = get_version(Ver, ListVersions),
			PID ! {get_res, K, Value, Ver},
			dummyDB(DB)

	end.

get_next_version(CM, []) -> CM;
get_next_version(CM,[{_,Ver,Deps}|T]) -> 
	if 
		Ver > CM -> 
			get_next_version(Ver,T);
		true -> 
			get_next_version(CM,T)
	end.
	
get_version(Ver, [ {Value, Ver, _} | T ]) -> Value;
get_version(Ver, [_ | T]) -> get_version(Ver,T).

%% -----------------------------------------------------------------
