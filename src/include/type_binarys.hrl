%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2016 21:57
%%%-------------------------------------------------------------------
-author("simonxu").

-type byte1() :: <<_:8>>.
-type byte2() :: <<_:16>>.
-type byte3() :: <<_:24>>.
-type byte4() :: <<_:32>>.
-type byte5() :: <<_:40>>.
-type byte5_up() :: <<_:40,_:_ * 8>>.
-type byte6() :: <<_:48>>.
-type byte8() :: <<_:64>>.
-type byte8_up() :: <<_:64,_:_ * 8>>.
-type byte14() :: <<_:112>>.
-type byte15() :: <<_:120>>.
-type byte23() :: <<_:184>>.
-type byte23_up() :: <<_:184,_:_ * 8>>.
-type bytes() :: <<_:8,_:_ * 8>>.