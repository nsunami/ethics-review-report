# Report generator for Ethics Monitor
Generating a report for reviewer activities from Ethics Monitor

The app downloads all applications after the login. The downloading process may take ~5 minutes.

## Environment variables

You will need to create `.Renviron` file in the app root directory with the 
following environment variables.

- `HAPLO_API_KEY` - API Key from Haplo 
- `HAPLO_BASE_URL`  - the base URL of Ethics Monitor. For EUR it is https://ethicsmonitor.eur.nl/
- `ADMIN_USERNAME` - Username for login
- `ADMIN_PASSWORD` - Password for login

