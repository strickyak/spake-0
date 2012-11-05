

all: object.go table.go vector.go duck.go duck_test.go
	# go build -x *.go

duck.go : duck.gop
	cpp < duck.gop | grep -v '^#' > duck.go

duck_test.go : duck_test.gop
	cpp < duck_test.gop | grep -v '^#' > duck_test.go

object.go : object.gop
	cpp < object.gop | grep -v '^#' > object.go

table.go : table.gop
	cpp < table.gop | grep -v '^#' > table.go

vector.go : vector.gop
	cpp < vector.gop | grep -v '^#' > vector.go
