% Sample parsing of some AsciiDoc features
% This one currently only works on SWI Prolog
%
% Differences from AsciiDoctor:
%
% * _*a*_ is not correctly handled by AsciiDoctor https://asciidoc.zulipchat.com/#narrow/stream/335219-asciidoc-lang/topic/Clarification.20on.20constrained.20formatting.20marks/near/299416275

:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

punct(pu(",")) --> ",".
punct(pu(";")) --> ";".
punct(pu("\"")) --> "\"".
punct(pu(".")) --> ".".
punct(pu("?")) --> "?".
punct(pu("!")) --> "!".

space(sp(" ")) --> " ".

formatting_mark(fm("*")) --> "*".
formatting_mark(fm("_")) --> "_".
formatting_mark(fm("`")) --> "`".
formatting_mark(fm("#")) --> "#".
formatting_mark(fm("~")) --> "~".

pre_constrained_formatting_mark(pre_cfm(X)) --> space(X).
pre_constrained_formatting_mark(pre_cfm(bl)) --> [bl].

post_constrained_formatting_mark(post_cfm(X)) --> space(sp([X])).
post_constrained_formatting_mark(post_cfm(X)) --> punct(pu([X])).
post_constrained_formatting_mark(post_cfm(el)) --> [el].

% TODO: the content of a constrained formatting mark cannot begin/end with spaces
constrained_formatting_mark([Pre, cfm(F, T, F)]), [Post] -->
    pre_constrained_formatting_mark(Pre),
    formatting_mark(F),
    {F=fm(FM)},
    nested_line_parts(T, FM),
    formatting_mark(F),
    post_constrained_formatting_mark(post_cfm(Post)).

% this is a hack to surround the content of a constrained formatting mark in be-el
nested_line_parts(X, F, B, A) :- append([B1, F, B2], B), append([[bl], B1, [el], F, B2], NB), line_parts(X, NB, A).

text(txt(T)) --> string(T), {length(T,X), X =\= 0}.

line_part([Pre, cfm(F, T, F)]) --> constrained_formatting_mark([Pre, cfm(F, T, F)]), !.
line_part([X]) --> text(X), !.

line_parts([X|XS]) --> line_part(X), line_parts(XS).
line_parts([]) --> [].

parse_line(X, Y) :- append([[bl], X, [el]], XW), phrase(line_parts(Y), XW).
