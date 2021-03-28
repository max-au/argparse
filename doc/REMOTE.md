# Remote CLI
It is often convenient to access a remote node using CLI. OTP includes `erl_call`
utility enabling basic remote access, e.g. `./erl_call -a 'rand uniform' -n my_node`.

Remote CLI provides convenience script to enable this kind of interaction:
```
    cli -n NODENAME math cos 3.14
```


## Default node name
When node name is not given, `cli` attempts to find a node using following heuristics:


## Environment variables
CLI is an escript, so it accepts  Erlang environment variables
```
    ERL_AFLAGS='-proto_dist inet6_tls' cli math sin 3.14
```

## Implementation details