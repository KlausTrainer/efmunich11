{application, term_cache,
 [{description, "A simple cache server that stores Erlang terms."},
  {vsn, "0.1.0"},
  {modules, [
    term_cache,
    term_cache_app,
    term_cache_sup
  ]},
  {registered, [term_cache_sup]},
  {mod, {term_cache_app, []}},
  {applications, [kernel, stdlib]}
 ]}.
