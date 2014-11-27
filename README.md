campus
======

Simple web app to subscribe to the calendar offered by the Campus Office (https://www.campus.rwth-aachen.de/office/)

### Deploy to Heroku
Run the following commands:
```
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master # This may take a while
heroku config:set CAMPUS_USER=123456 # Your campus office user id (matriculation number)
heroku config:set CAMPUS_PASS=password # Your password
heroku open # open the app
```
