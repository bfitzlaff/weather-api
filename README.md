# weather-api

This is a simple haskell web server that provides an endpoint to query for weather info by latitude and longitude. It uses https://openweathermap.org/ as the backend data provider. To run the server:

1. If you haven't already, [install Stack](https://www.fpcomplete.com/haskell/get-started/)
2. Build the project with `stack build`
3. Modify `/config/settings.dhall` with your desired port, default is 1234
4. Run the server with `stack exec weather-api-exe`

Once the server is running you can access it on the localhost on your configured port. The endpoints exposed are:

- `GET` `/ping` - pongs
- `GET` `/weather?lat={latitude}&lon={longitude}` - returns weather info as json
- `Get` `/openapi.json` - returns full [OpenApi](https://swagger.io/specification/) spec for the server as json
