# servant-oauth2

This is a modern servant wrapper around the
[wai-middleware-auth](https://github.com/fpco/wai-middleware-auth) OAuth2
provider implementations.

Documentation:

- <https://tweag.github.io/servant-oauth2/servant-oauth2/>
- <https://tweag.github.io/servant-oauth2/servant-oauth2-examples/>


#### Hacking

[Simplest example](./servant-oauth2-examples/src/Servant/OAuth2/Examples/Simple.hs):

```
./hack example-basic
```

[Cookie example](./servant-oauth2-examples/src/Servant/OAuth2/Examples/Cookies.hs):
```
./hack example-cookies
```

[Example that performs "authorisation"](./servant-oauth2-examples/src/Servant/OAuth2/Examples/Authorisation.hs) (i.e. there is an 'admin' section):
```
./hack example-auth
```

#### Before running

You'll need to make a GitHub OAuth application, and a Google one, if you want
to test that as well.

The details of which you'll need to place in `./config.toml`. See
`./config.example.toml` for an example.

The most important detail is that the callback URL on github, and in the
config, is set to the same thing:

```
http://localhost:8080/auth/github/complete
```

#### Todo

- [ ] Contributing guide
- [x] Document exported functions
- [x] More detailed readme documentation
- [x] Build in an example of 'Authorisation'; i.e. lifting the logins to the type-level
- [x] Show an example of multiple auths (distinguish by type)
- [x] See if we can get away with only doing it for `complete`
- [x] Pass settings through somehow
- [x] Try and hide away as much details as possible
- [x] Define a basic structure following `AuthProtect`
