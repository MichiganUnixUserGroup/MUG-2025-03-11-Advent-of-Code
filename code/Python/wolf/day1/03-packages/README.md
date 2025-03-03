## First steps
This is set up to be run with `uv`.  So you do have to have `uv` installed.  There are lots of ways to do this.  I used [Homebrew](https://brew.sh), but there are specific [instructions](https://github.com/astral-sh/uv?tab=readme-ov-file#installation) in the `uv` repository.  `uv` will use a virtual environment, `.venv`, that it generates automatically and uses exactly when needed.  It doesn't leave the environment activated.  The environment is only activated during the execution of anything that needs it.  You can immediately run `day1.py` with real data or in tests and `uv` will generate `.venv` from scratch.

I'm showing the simplest way to run the tool and tests here; but I **did** add command line parsing.  There are options and arguments.  See the code.

## Where to be
You should `cd` into this directory.  That is: the directory that contains this `README.md` file and the `day1.py` file.  If you want to run this script on actual Advent of Code data; then you should have that data somewhere around, because you supply it's path or else pipe in its data to `day1.py`.

## How to run the app in "production" mode, on real data
`uv run day1.py <your-data-file>`

## Here's how I run it
`uv run day1.py < day1.input`

## How to run the tests.  The test data from the problem description is supplied.
`PYTHONPATH=. uv run pytest`

## How is this different from 02-best-practices?

In this implementation, `day1.py` only contains `main` and appropriate imports.  Everything that knows about lists: reading them in, manipulating them, etc, is in the "package" `list_tools`.  You know it's a package because it has an `__init__.py`.  Since the tests are all about testing those functions, the `tests/` directory has moved down, to be a child of `list_tools/`.

## Something extra to note about editing

If your editor knows Python (probably because you have plugins or whatever), then it typically won't "see" the things you are trying to import.  It will underline them, or whatever.  If your environment actually contains those things, then your editor **does** know about them.  Since `uv run ...` activates your virtual environment for the execution of whatever thing you name next, you can make your editor see all your imports and packages by saying

```
PYTHONPATH=. uv run nvim -p day1.py list_tools/*.py
```

At least, that's what **I** say.  Although, if you have `fd` or don't mind rewriting this as a `find` command, the following might be better:

```
PYTHONPATH=. uv run nvim -p $(fd --extension py)
```
