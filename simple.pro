% This proof of concept proves that Prolog can parse text files using grammars
% and generate ASTs.

% USAGE
%
% $ echo -n thecateatsabat >sentence
% $ scryer-prolog simple.pro
% ?- parse("sentence", "sentence.json").
% <press ".">
% ^D
% $ python3 -m json.tool sentence.json

% DESCRIPTION

% Those are the dependencies of this program.
%
% pio is the Scryer Prolog library that supports executing DCG grammars on files
% efficiently. It provides the phrase_from_file predicate, which applies a DCG
% to a file; and the phrase_to_file predicate that can write parsed ASTs to a
% file. phrase_to_file is used for generating JSON.
%
% serialization/json is the library that contains the pairs, list, and string
% predicates that can be used to build JSON structures to serialize.

:- use_module(library(pio)).
:- use_module(library(serialization/json)).

% This is the grammar of the language.
%
% This grammar is all pure DCGs.

% The following DCG:

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

% should be read as follows:
%
% A "sentence" with structure s(NP, VP) is composed of:
%
% * a noun_phrase NP followed by
% * a verb_phrase VP
%
% When asked to parse a sentence, Prolog tries to parse a noun phrase, then
% Prolog tries to parse a verb phrase.

noun_phrase(np(D,N)) --> det(D), noun(N).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).

% The following DCG:

det(d("the")) --> "the".

% should be read as follows:
%
% A det(erminant) d("the") is composed of the string "the".

det(d("a")) --> "a".
noun(n("bat")) --> "bat".
noun(n("cat")) --> "cat".
verb(v("eats")) --> "eats".

% The following is crude code that implements the predicate to_json(AST, JSON).
% Prolog programs are usually comprised of predicates. A predicate describes a
% relationship between its arguments.
%
% to_json(A, J) should be read as "J is the A AST converted to JSON". This
% predicate is used by the parse predicate below to convert the parsed AST into
% JSON.
%
% This code is not a good example of how to do this, so we will not describe it
% in detail.

to_json(s(NP,VP), pairs([string("S")-list([JNP,JVP])])):-to_json(NP, JNP),to_json(VP, JVP).
to_json(np(D,N), pairs([string("NP")-list([JD,JN])])):-to_json(D, JD),to_json(N,JN).
to_json(vp(V,NP), pairs([string("VP")-list([JV,JNP])])):-to_json(V, JV), to_json(NP, JNP).
to_json(d(T), pairs([string("D")-string(T)])).
to_json(n(T), pairs([string("N")-string(T)])).
to_json(v(T), pairs([string("V")-string(T)])).

% The parse predicate parse(F,J) should be read as "J is a file that contains
% the parsed JSON AST of the sentence contained in the file F".

parse(F, J):-phrase_from_file(sentence(S), F), to_json(S, JS), phrase_to_file(json_chars(JS), J).

% EXPERIMENTING WITH THIS GRAMMAR
%
% We can use the Prolog interactive console to experiment with the grammar. We
% can do this to test our code and understand how Prolog works.
%
% The phrase(G, P) predicate allows us to execute DCG grammars. It should be
% read as G is the result of parsing P. A simple command is:
%
% ?- phrase(sentence(X), "acateatsabat").
%
% , which is read as "sentence X is the result of parsing "acateatsthebat". If
% you execute this statement, then Prolog prints:
%
% X = s(np(d("a"),n("cat")),vp(v("eats"),np(d("a"),n("bat"))))
%
% Which means "sentence s(np(d("a"),n("cat")),vp(v("eats"),np(d("a"),n("bat"))))"
% is the result of parsing "acateatsabat".
%
% We can also do the same with "sub parts" of the grammar:
%
% ?- phrase(verb_phrase(X), "eatsthebat").
%  X = vp(v("eats"),np(d("the"),n("bat")))
%
% But the most surprising characteristic of Prolog is that statements can be
% bidirectional. If instead of executing:
%
% ?- phrase(sentence(X), "acateatsabat").
%
% we execute:
%
% ?- phrase(sentence(X), Y).
%
% , setting Y as the sentence, Prolog will try to find all combinations of X and
% Y that satisfy the predicate, giving us all the sentences that the grammar
% knows about:
%
%    X = s(np(d("the"),n("bat")),vp(v("eats"),np(d("the"),n("bat")))), Y = "thebateatsthebat"
% ;  X = s(np(d("the"),n("bat")),vp(v("eats"),np(d("the"),n("cat")))), Y = "thebateatsthecat"
% ;  X = s(np(d("the"),n("bat")),vp(v("eats"),np(d("a"),n("bat")))), Y = "thebateatsabat"
% ;  X = s(np(d("the"),n("bat")),vp(v("eats"),np(d("a"),n("cat")))), Y = "thebateatsacat"
% ;  X = s(np(d("the"),n("cat")),vp(v("eats"),np(d("the"),n("bat")))), Y = "thecateatsthebat"
% ;  X = s(np(d("the"),n("cat")),vp(v("eats"),np(d("the"),n("cat")))), Y = "thecateatsthecat"
% ;  X = s(np(d("the"),n("cat")),vp(v("eats"),np(d("a"),n("bat")))), Y = "thecateatsabat"
% ;  X = s(np(d("the"),n("cat")),vp(v("eats"),np(d("a"),n("cat")))), Y = "thecateatsacat"
% ;  X = s(np(d("a"),n("bat")),vp(v("eats"),np(d("the"),n("bat")))), Y = "abateatsthebat"
% ;  X = s(np(d("a"),n("bat")),vp(v("eats"),np(d("the"),n("cat")))), Y = "abateatsthecat"
% ;  X = s(np(d("a"),n("bat")),vp(v("eats"),np(d("a"),n("bat")))), Y = "abateatsabat"
% ;  X = s(np(d("a"),n("bat")),vp(v("eats"),np(d("a"),n("cat")))), Y = "abateatsacat"
% ;  X = s(np(d("a"),n("cat")),vp(v("eats"),np(d("the"),n("bat")))), Y = "acateatsthebat"
% ;  X = s(np(d("a"),n("cat")),vp(v("eats"),np(d("the"),n("cat")))), Y = "acateatsthecat"
% ;  X = s(np(d("a"),n("cat")),vp(v("eats"),np(d("a"),n("bat")))), Y = "acateatsabat"
% ;  X = s(np(d("a"),n("cat")),vp(v("eats"),np(d("a"),n("cat")))), Y = "acateatsacat".
%
% After Prolog shows the first solution, it prompts to if it should "continue".
% The possible things you can do are:
%
% SPACE: show further answers
% RETURN or .: stop
% f: shows 5 answers
% a: shows all answers
% h: help
