-module(eqsms_example).
-compile(export_all).
-include_lib("eutil/include/eutil.hrl").

-define(TEXT, unicode:characters_to_binary("【XXX】验证码1234，请您尽快验证，完成XXX注册。如非本人操作请忽略。")).
-define(MOBILE, "+8615102025006").

send_isms() ->
    eqsms:send_isms(?MOBILE, ?TEXT).

send_sms() ->
    eqsms:send_sms("86", "15102025006", ?TEXT).

send_multi_sms() ->
    Tels = [#{nationcode => <<"86">>, mobile => <<"15102025006">>}
            %, #{nationcode => "86", mobile => "15102025008"}
           ],
    eqsms:send_multi_sms(Tels, ?TEXT).
