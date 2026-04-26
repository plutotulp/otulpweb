{
  runCommand,
  gitignoreSource,
  otulpweb-webclient-closurecompiled,
  otulpweb-server,
  bootstrap,
}:
runCommand "otulpweb"
  {
    src = gitignoreSource ./.;
    server = otulpweb-server;
    client = otulpweb-webclient-closurecompiled;
    inherit bootstrap;
  }
  ''
    mkdir -p $out/static
    cp -v $src/index.html $out/static/
    cp -v $src/favicon.ico $out/static/
    cp -v $bootstrap/bootstrap.bundle.min.js     $out/static/bootstrap.bundle.min.js
    cp -v $bootstrap/bootstrap.bundle.min.js.map $out/static/bootstrap.bundle.min.js.map
    cp -v $bootstrap/bootstrap.min.css           $out/static/bootstrap.min.css
    cp -v $bootstrap/bootstrap.min.css.map       $out/static/bootstrap.min.css.map
    cp -v $client/all.js $out/static/

    cp -v $server/bin/server $out/otulpweb-server
  ''
