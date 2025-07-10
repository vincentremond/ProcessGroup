$ErrorActionPreference = "Stop"

dotnet tool restore
dotnet build

AddToPath .\ProcessGroup\bin\Debug\
