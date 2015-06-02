# elm-init [![Build Status](https://travis-ci.org/JustusAdam/elm-init.svg?branch=master)](https://travis-ci.org/JustusAdam/elm-init) [![Hackage version](https://img.shields.io/hackage/v/elm-init.svg)](https://hackage.haskell.org/package/elm-init)

Initialize a new empty [Elm](//elm-lang.org) project.

## Features

Interactive setup tool for a new Elm project.

This tool basically helps you populate your `elm-package.json` file.

Including:

- License chooser (automatically adds license file)
- Version validator

## Install

Install the package from the [Hackage](//hackage.haskell.org/packages/elm-init) with `cabal install elm-init`.

## Use

Simply run

- `elm-init [DIRECTORY]`
- or `elm init [DIRECTORY]` if you have the elm platform installed

`DIRECTORY = '.'` if omitted. `DIRECTORY` need not exist, though needs to be a valid directory name.
