tomle
=====

[TOML](https://github.com/mojombo/toml) in Erlang, using the parsetools [leex](http://erlang.org/doc/man/leex.html) and
[yecc](http://erlang.org/doc/man/yecc.html).

Using TOML version b098bd2b06920b69102bd4929cc5d7784893a123.

It parses the `example.toml` file into this erlang term::

```erlang
       [{"title", "TOML Example"},
        {"owner", 
         [{"name", "Tom Preston-Werner"},
          {"organization", "GitHub"},
          {"bio", "GitHub Cofounder & CEO\nLikes tater tots and beer."},
          {"dob", {{1979,05,27},{07,32,00}}}]},
        {"database",
         [{"server", "192.168.1.1"},
          {"ports", [8001, 8001, 8002]},
          {"connection_max", 5000},
          {"enabled", true}
         ]},
        {"servers",
         [{"alpha", 
           [{"ip", "10.0.0.1"}, {"dc", "eqdc10"}]},
          {"beta", 
           [{"ip", "10.0.0.2"}, {"dc", "eqdc10"}]}]},
        {"clients",
          [{"data", [["gamma", "delta"], [1, 2]]},
           {"hosts", ["alpha", "omega"]}]}
       ]
```

It also parses the `hard_example.toml` file without errors.

