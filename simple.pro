% $ cargo install --git https://github.com/mthom/scryer-prolog.git
% $ echo -n thecateatsabat >sentence
% $ scryer-prolog simple.pl    
% ?- parse("sentence", "sentence.json").
% ^D
% $ python3 -m json.tool sentence.json

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(serialization/json)).

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(D,N)) --> det(D), noun(N).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
det(d("the")) --> "the".
det(d("a")) --> "a".
noun(n("bat")) --> "bat".
noun(n("cat")) --> "cat".
verb(v("eats")) --> "eats".

to_json(s(NP,VP), pairs([string("S")-list([JNP,JVP])])):-to_json(NP, JNP),to_json(VP, JVP).
to_json(np(D,N), pairs([string("NP")-list([JD,JN])])):-to_json(D, JD),to_json(N,JN).
to_json(vp(V,NP), pairs([string("VP")-list([JV,JNP])])):-to_json(V, JV), to_json(NP, JNP).
to_json(d(T), pairs([string("D")-string(T)])).
to_json(n(T), pairs([string("N")-string(T)])).
to_json(v(T), pairs([string("V")-string(T)])).

parse(F, J):-phrase_from_file(sentence(S), F), to_json(S, JS), phrase_to_file(json_chars(JS), J).
