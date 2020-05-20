config ?= Debug

build:
	dotnet build -c $(config)
	dotnet publish -o out/ Decoder/Decoder.fsproj

test:
	dotnet test -c $(config)
