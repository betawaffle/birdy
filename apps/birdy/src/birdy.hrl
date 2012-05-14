-define(PREFLIST(Name), ?PREFLIST(Index, 1)).
-define(PREFLIST(Name, N),
        riak_core_apl:get_apl(chash:key_of(Name), N, birdy)).
