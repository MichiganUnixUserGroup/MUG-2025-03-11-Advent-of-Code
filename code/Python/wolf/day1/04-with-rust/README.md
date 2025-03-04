## First steps
This is set up to be run with `uv`.  So you do have to have `uv` installed.  There are lots of ways to do this.  I used [Homebrew](https://brew.sh), but there are specific [instructions](https://github.com/astral-sh/uv?tab=readme-ov-file#installation) in the `uv` repository.  `uv` will use a virtual environment, `.venv`, that it generates automatically and uses exactly when needed.  It doesn't leave the environment activated.  The environment is only activated during the execution of anything that needs it.  You can immediately run `day1.py` with real data or in tests and `uv` will generate `.venv` from scratch.

I'm showing the simplest way to run the tool and tests here; but I **did** add command line parsing.  There are options and arguments.  See the code.

## There's Rust here: you have to build it
This implementation of day1 is exactly like the 03-packages version, except for two things.  First, the `list_tools` package function `read_the_two_lists` takes an optional `Path` instead of an optional file handle.  If a `Path` is supplied, it should lead to the input.  If `None`, then stdin will be used.  Second, `list_tools` is written in Rust instead of Python.  So just like the Rustimplementations you'll find elsewhere in this repository, you must have the Rust toolchain installed.  I had to do three things to make sure that `list_tools` could be imported by `day1.py`.

  * I had to `cd` into the `list_tools` directory and `cargo build`
  * I had to copy the dynamic library it built and change the name.  This is different per platform.  See [here](https://pyo3.rs/v0.17.2/building_and_distribution#manual-builds).
  * I had to run `maturin develop` to get `list_tools` into Python's virtual environment.  That meant the virtual environment had to be active.  The virtual environment belongs to the Python part of the project, one level up the directory hierarchy; and you activate it with `uv`.  `maturin` is a tool installed **in** that virtual environment.  Once the environment is active, all will be well.

## Where to be (now that you're on the Python part)
You should `cd` into this directory.  That is: the directory that contains this `README.md` file and the `day1.py` file.  If you want to run this script on actual Advent of Code data; then you should have that data somewhere around, because you supply it's path or else pipe in its data to `day1.py`.

## How to run the app in "production" mode, on real data
`uv run day1.py <your-data-file>`

## Here's how I run it
`uv run day1.py < day1.input`

## Note: I don't have tests implemented in this version, but I plan to implement them.
## How to run the tests.  The test data from the problem description is supplied.
`PYTHONPATH=. uv run pytest`

## Something extra to note about editing

If your editor knows Python (probably because you have plugins or whatever), then it typically won't "see" the things you are trying to import.  It will underline them, or whatever.  If your environment actually contains those things, then your editor **does** know about them.  Since `uv run ...` activates your virtual environment for the execution of whatever thing you name next, you can make your editor see all your imports and packages by saying

```
PYTHONPATH=. uv run nvim -p day1.py list_tools/src/lib.rs
```

At least, that's what **I** say.
