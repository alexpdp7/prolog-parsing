% Sample parsing of some AsciiDoc features
% Works on SWI Prolog, might work with Scryer Prolog
%
% Differences from AsciiDoctor:
%
% * _*a*_ is not correctly handled by AsciiDoctor https://asciidoc.zulipchat.com/#narrow/stream/335219-asciidoc-lang/topic/Clarification.20on.20constrained.20formatting.20marks/near/299416275

:- set_prolog_flag(double_quotes, chars).

% this is unnecessary with SWI, but needed with Scryer
:- use_module(library(lists)).

punct(",") --> ",".
punct(";") --> ";".
punct("\"") --> "\"".
punct(".") --> ".".
punct("?") --> "?".
punct("!") --> "!".

space(" ") --> " ".

formatting_mark("*") --> "*".
formatting_mark("_") --> "_".
formatting_mark("`") --> "`".
formatting_mark("#") --> "#".
formatting_mark("~") --> "~".

pre_constrained_formatting_mark(pre_cfm(X)) --> space(X).
pre_constrained_formatting_mark(pre_cfm(bl)) --> [bl].

post_constrained_formatting_mark(post_cfm(X)) --> space(X).
post_constrained_formatting_mark(post_cfm(X)) --> punct(X).
post_constrained_formatting_mark(post_cfm(el)) --> [el].

constrained_formatting_mark([Pre, cfm(F, T, F)]), [Post] -->
    pre_constrained_formatting_mark(pre_cfm(Pre)),
    formatting_mark(F),
    nested_line_parts(T, F),
    {not_wrapped_in_spaces(T), T \= [bl|[el|_]]}, % TODO: bogus, we should get rid of those bl,el bits before!
    formatting_mark(F),
    post_constrained_formatting_mark(post_cfm(Post)).

not_wrapped_in_spaces(X) :- not_prefixed_by_spaces(X), reverse(X, RX), not_prefixed_by_spaces(RX).
not_prefixed_by_spaces([bl|X]) :- !, not_prefixed_by_spaces(X).
not_prefixed_by_spaces([' '|_]) :- !, fail.
not_prefixed_by_spaces(_).

% this is a hack to surround the content of a constrained formatting mark in be-el
nested_line_parts(X, F, B, A) :- append([B1, F, B2], B), append([[bl], B1, [el], F, B2], NB), line_parts(X, NB, A).

unconstrained_formatting_mark(ucfm([F, F, T, F, F])) -->
    formatting_mark(F),
    formatting_mark(F),
    nested_line_parts(T,F),
    formatting_mark(F),
    formatting_mark(F).

char(T) --> [T].

line_part(X) --> unconstrained_formatting_mark(X), !.
line_part(X) --> constrained_formatting_mark(X), !.
line_part(X) --> char(X), !.

line_parts([X|XS]) --> line_part(X), line_parts(XS).
line_parts([]) --> [].

parse_line(X, Y) :- append([[bl|X], [el]], XW), phrase(line_parts(Y), XW).

% run tests with:
%
% $ swipl -g run_tests -t halt asciidoc_poc.pro

:- begin_tests(basic).
:- set_prolog_flag(double_quotes, chars).

test(plain) :- parse_line("abc", [bl, a, b, c, el]).
test(single_cfm) :- parse_line("*abc*", [[bl, cfm([*], [bl, a, b, c, el], [*])], el]).
test(nested_cfm) :- parse_line("*_a_*", [[bl, cfm([*], [[bl, cfm(['_'], [bl, a, el], ['_'])], el], [*])], el]).

% but see below cfms_separted_by_single_space; this one has two spaces
test(nested_cfm_double_two_spaces) :- parse_line("*_a_  _b_*", [[bl, cfm([*], [[bl, cfm(['_'], [bl, a, el], ['_'])], [' '], [[' '], cfm(['_'], [bl, b, el], ['_'])], el], [*])], el]).

test(cfm_no_constraint_outside) :- parse_line("a*b*", [bl, a, *, b, *, el]).
test(cfm_no_constraint_inside) :- parse_line("* a *", [bl, *, ' ', a, ' ', *, el]).

test(ucfm) :- parse_line("aaaaa**b**", [bl, a, a, a, a, a, ucfm([[*], [*], [bl, b, el], [*], [*]]), el]).
test(ucfm_nested_in_cfm) :- parse_line("_aa**b**_", [[bl, cfm(['_'], [bl, a, a, ucfm([[*], [*], [bl, b, el], [*], [*]]), el], ['_'])], el]).
test(lone_ucfm) :- parse_line("**b**", [bl, ucfm([[*], [*], [bl, b, el], [*], [*]]), el]).
test(lone_ucfm_with_nested_cfm) :- parse_line("**_b_**", [bl, ucfm([[*], [*], [[bl, cfm(['_'], [bl, b, el], ['_'])], el], [*], [*]]), el]).

% KNOWN ISSUES

test(cfms_separated_by_single_space_not_parsed) :- parse_line("*abc* *def*", [[bl, cfm([*], [bl, a, b, c, el], [*])], [' '], *, d, e, f, *, el]).
% ... although...
test(cfms_separated_by_two_spaces) :- parse_line("*abc*  *def*", [[bl, cfm([*], [bl, a, b, c, el], [*])], [' '], [[' '], cfm([*], [bl, d, e, f, el], [*])], el]).

test(consecutive_ucfms_wrongly_nested) :- parse_line("**a** **b**", [bl, ucfm([[*], [*], [bl, a, el, ucfm([[*], [*], [bl, ' ', el], [*], [*]]), b], [*], [*]]), el]).

:- end_tests(basic).
