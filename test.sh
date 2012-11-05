set -ex
(cd src/duck/ ; make) && GOPATH=`pwd` go test duck 
