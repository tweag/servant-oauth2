# Example-Authorised

Building on the _cookies_ example, we will build in type-level security into
the servant API, so that we can have an 'admin' area which only certain users
can access; i.e. we'll perform "authorisation" of users as well.

### Setup

Create a file `example-basic` called `config.toml`:

```
[oauth-github]
  name         = "servant-oauth2-example-basic"
  callback_url = "http://localhost:8080/auth/github/complete"
  id           = "..."
  secret       = "..."
```

### Running

From the home directory:

```
stack run -- example-auth
```
