-define(LOWER(C), ishtar_util:chat_to_lower(C)).
-define(IS_ALPHA(C), ((?LOWER(C) >= $a) and (?LOWER(C) =< $z))).
-define(IS_NUM(C), ((C >= $0) and (C =< $9))).
-define(IS_ALPHANUM(C), (?IS_ALPHA(C) orelse ?IS_NUM(C))).