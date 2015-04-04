campus
======

![build status](https://circleci.com/gh/jhedev/campus.png?style=shield)

Simple web app to subscribe to the calendar offered by the [Campus Office](https://www.campus.rwth-aachen.de/office/) of [RWTH Aachen](http://www.rwth-aachen.de/). (See @Faerbit's [fork](https://github.com/Faerbit/campus) to get this running for the Campus Office of [FH Aachen](https://www.fh-aachen.de/))

### Build and run locally
```
cabal sandbox init # Create a sandbox
cabal install # Installs all dependencies and builds campus, may take a while
PORT=3000 CAMPUS_USER=ab123456 CAMPUS_PASS=password cabal run # to run campus
```

### Run the Docker container
```
docker run -e CAMPUS_USER=ab123456 -e CAMPUS_PASS=password jhedev/campus
```

### Deploy to Heroku
Run the following commands:
```
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master # This may take a while
heroku config:set CAMPUS_USER=ab123456 # Your campus office user id (matriculation number)
heroku config:set CAMPUS_PASS=password # Your password
heroku open # open the app
```
