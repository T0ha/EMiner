-module(em_utils).
-compile([export_all]).

reverse(Data) ->
    << <<D:32/big>> || <<D:32/little>> <= Data>>.

hex_to_bin(Data) ->
    << <<(list_to_integer(binary_to_list(D), 16))/integer>> || <<D:2/bytes>> <= Data>>.

bin_to_hex(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Data])).

dualsha256(Data) ->
    crypto:hash(sha256, crypto:hash(sha256,Data)).


build_mercle([], CoinbHashBin) ->
    CoinbHashBin;
build_mercle([MercleBranch | O], CoinbHashBin) ->
    build_mercle(O, dualsha256(<<CoinbHashBin/bytes, (hex_to_bin(MercleBranch))/bytes>>)).
    
build_block(Version, PrevHash, Coinb1, Coinb2, Extranonce1, Extra2len, MercleBranch, NBits, NTime) ->
    Extranonce2 = bin_to_hex(<<0:(Extra2len - 1)/integer-unit:8, 1>>),
    CoinbHash = dualsha256(hex_to_bin(<<Coinb1/bytes, Extranonce1/bytes, Extranonce2/bytes, Coinb2/bytes>>)),
    MercleRoot = build_mercle(MercleBranch, CoinbHash),
    <<Version/bytes, PrevHash/bytes, (bin_to_hex(reverse(MercleRoot)))/bytes, NTime/bytes, NBits/bytes>>.
