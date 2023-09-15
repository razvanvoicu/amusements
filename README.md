There are 2 sbt projects here:
* www - minimal HTTP end-point for interactions with storage
* scalajs - client-based JS code

The end-point is supposed to run behind a proxy that would also serve additional
HTML or JS files that need to be loaded into the client. As this project is expected
to be deployed into the Google App Engine, an nginx instance would
already be provided by the engine. The app will simply work when deployed.
For testing, an instance of nginx needs to run locally. The current sbt
tasks are not configured to start nginx; this has to be done independently.

## Testing

### Start nginx

Make sure `nginx` is installed, say in `C:\Program Files\nginx-1.23.2`.
The file `C:\Program Files\nginx-1.23.2\conf\nginx.conf` must be configured
to serve files from `www/deploy` and to proxy on port 8080. The config
file on this machine can be used as a sample. Then nginx can be started with
the commands:

> C:\Users\Raz>cd "\Program Files\nginx-1.23.2"
> 
> C:\Program Files\nginx-1.23.2>nginx.exe

### Build the project

The sbt task `prepareDeploy` in the www project builds
the endpoint code into a jar, and then copies all the
relevant files into `www/deploy`. Once that's done, 
the project should be running locally on `http://localhost:80`,
and should be accessible from any browser.

## Deploying

> C:\code\www\deploy>gcloud app deploy
> 

The app should now be available at `https://raz.sg`
