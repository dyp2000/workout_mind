%% -*- coding: utf-8 -*-
{application, test06, [
	{description, "test #06"},
	{vsn, "1.0"},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		compiler,
		syntax_tools,
		goldrush,
		lager,
		crypto,
		cowboy
	]},
	{mod, {test06, []}},
	{env, []}
]}.