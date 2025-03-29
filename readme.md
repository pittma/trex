Trex
====

A teensy-tiny slide generator based on Hakyll.

```
Usage: trex COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  new                      generate a new Trex Deck at the given path
  serve                    run the server and watch for changes
```

## Getting Started

### Install

Clone this repository and then install with `stack install`.

### Making a Deck

```
$ trex new vectorization
$ cd vectorization
$ vi slides/01.md
```

```markdown
<center>
# Cryptography 🤝 Vectorization
<p></p>
Using AES-XTS as a lens into vectorization techniques.
</center>
```

```
$ trex serve
```

Navigate to `http://localhost:8000/deck.html`:

![screenshot of the generate slide](/slide.png)

## Example

You can see a complete example
[here](https://github.com/pittma/vectorization).
