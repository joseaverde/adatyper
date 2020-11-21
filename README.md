# Adatyper
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Description
Adatyper is an open source typing game written in the Ada programming language.
This programme will contain a set of games and execices to learn typing in a
simple and fast way. It's aimed to be runned in a Terminal or in a Console
(Windows) that can recognise ANSI escape sequences, so it has no dependences at
all.


## Versions
The game isn't in a stable release yet, in other words there is no game yet.
Some pre releases executables are bundled in different versions of github, but
they are **not playable yet**.


## How to build the game?
You will need the _GNAT_ or any other Ada compiler and the _GPR_ build tools.
You can also use **alire** to compile it, but it hasn't been released in Alire
_yet_.

To compile it just run
> gprbuild -Padatyper

If you are running Windows run it with the following flags:
> gprbuild -Padatyper -XADATYPER_OPERATING_SYTEM=windows

These are the external variables you can use (they are introduced in gprbuild
with -X<VARIABLE>=<Value>

 * **ADATYPER_COMPILE_CHECKS**   (enabled/disabled [default=disabled]): Disable it for now, because there are some warnings that are harmless but I need to shut up.
 * **ADATYPER_RUNTIME_CHECKS**   (enabled/disabled [default=enabled]): It's better not to disable this, it's safer to have it enabled.
 * **ADATYPER_STYLE_CHECKS**     (enabled/disabled [default=disabled]): Please, don't enable this, I don't care about style checks maybe in the future I will change it. But if you try to compile it with this enabled keep in mind it's not going to compile.
 * **ADATYPER_CONTRACTS**        (enabled/disabled [default=enabled]): It's up to you to enable or disable this, but it's better to have it enabled.
 * **ADATYPER_BUILD_MODE**       (debug/optimize [default=debug]): Change this if you want to optimize it.
 * **ADATYPER_OPERATING_SYSTEM** (linux/windows [default=linux]): It's not going to compile in Windows if you don't change this.

To compile it with alire just run:
> alr build <Options>


## Licenses
This game is made available under the [GPLv3](LICENSE) licence.

## Contributing to the project
For detailed information check out [CONTRIBUTING.md](CONTRIBUTING.md)
