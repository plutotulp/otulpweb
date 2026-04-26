{
  runCommand,
  closurecompiler,
  otulpweb-webclient-ghcjs
}:
runCommand "otulpweb-webclient-closurecompiled"
  {
    client = otulpweb-webclient-ghcjs;
    inherit closurecompiler;
  }
  ''
    mkdir -p $out
    $closurecompiler/bin/closure-compiler \
      --compilation_level ADVANCED \
      --jscomp_off checkVars \
      --externs $client/bin/webclient.jsexe/all.js.externs \
      --js $client/bin/webclient.jsexe/all.js \
      --js_output_file all.js
    cp -v all.js $out/
  ''
