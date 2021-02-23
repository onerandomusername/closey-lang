# Curly
Curly is a functional programming language that focuses on iterators. Some of its main implementation features include sum types, iterators, list comprehensions, and quantifiers.

## Example
<pre>
primes = n <span class="hljs-keyword">in</span> (from <span class="hljs-number">2</span>) <span class="hljs-keyword">where</span>
    <span class="hljs-keyword">for</span> <span class="hljs-keyword">all</span> p <span class="hljs-keyword">in</span> (range <span class="hljs-number">2</span> n)
        n % p != <span class="hljs-number">0</span>
</pre>

## Build
Just type in the following:
```bash
git clone https://github.com/curly-lang/curly-lang && cd curly-lang && cargo build
```
This project depends on either `clang` or `gcc` for compiling, which can each be installed using your favourite package manager (`apt`/`pacman`/`dnf` for Linux and Homebrew/MacPorts for macOS).

Note: This repo has only been tested on Arch Linux, but should work on all other platforms rust supports without much issue.

## Progress
See TODO.md. Everything is highly experimental. Be cautious: code may be explosive.

## Support
Come to the [official discord server!](https://discord.gg/Gxfr6JDecv)
