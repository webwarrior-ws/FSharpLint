#load "common.fsx"

open Common

#load "buildAndRelease.fsx"

exec "dotnet"  @"fornax build" "docs"
