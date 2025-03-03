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
