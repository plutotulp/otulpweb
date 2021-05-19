-- This is the dev server version of the configuration, which gets
-- used when running
--
--     make server-dev-ghcid-server
--
-- in the top-level directory. Make sure to run a deployment build
-- (client + bootstrap css/js + server) first with
--
--    make build/otulpweb-deployment.nix-result
--
-- otherwise the dev server won't have any files to serve.
{ listenPort = 8080
, clientFilePath = "../build/otulpweb-deployment.nix-result/static"
}
