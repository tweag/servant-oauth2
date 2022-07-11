# Example-Basic

### Setup

Create a file in this folder called `config.toml`:

```
[oauth]
  name         = "servant-oauth2-example-basic"
  callback_url = "http://localhost:8080/auth/github/complete"
  id           = "..."
  secret       = "..."
```

### Running

From the home directory:

```
stack run -- example-basic
```
