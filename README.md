# servant-oauth2-combinator


#### Hacking

Simplest example:

```
./hack example-basic
```

Cookie example:
```
./hack example-cookies
```

#### Before running

You'll need to make a GitHub OAuth application.

The details of which you'll need to place in `./example-basic/config.toml`.

The most important detail is that the callback URL on github, and in the
config, is set to the same thing:

```
http://localhost:8080/auth/github/complete
```

#### Todo

- [ ] Show an example of multiple auths (distinguish by type)
- [x] See if we can get away with only doing it for `complete`
- [x] Pass settings through somehow
- [x] Try and hide away as much details as possible
- [x] Define a basic structure following `AuthProtect`
