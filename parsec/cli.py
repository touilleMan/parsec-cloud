import click

from parsec.ui import shell

try:
    from parsec.core.cli import core_cmd
except ImportError:

    @click.command()
    def core_cmd():
        raise SystemExit("No available.")


try:
    from parsec.backend.cli import backend_cmd, init_cmd
except ImportError:

    @click.command()
    def backend_cmd():
        raise SystemExit("No available.")

    @click.command()
    def init_cmd():
        raise SystemExit("No available.")


try:
    from parsec.ui.fuse import cli as fuse_cmd
except ImportError:

    @click.command()
    def fuse_cmd():
        raise RuntimeError("No available, is fusepy installed ?")


except NameError:
    pass


@click.group()
def cli():
    pass


cli.add_command(core_cmd, "core")
cli.add_command(backend_cmd, "backend")
cli.add_command(init_cmd, "init")
try:
    cli.add_command(fuse_cmd, "fuse")
except NameError:
    pass
cli.add_command(shell.cli, "shell")


if __name__ == "__main__":
    cli()
