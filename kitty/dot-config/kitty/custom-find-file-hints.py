"""A Kitty hint to open selected path in editor.

Calls `edit [+LINE[:COLUMN]] FILENAME` command (as for emacsclient).

See https://sw.kovidgoyal.net/kitty/kittens/hints/
"""

from pathlib import Path
import re
import shlex


def _make_paths_regexp(paths):
    query = "|".join(re.escape(path) for path in paths)
    return rf"(?:{query})[\S\n]*"


def mark(text, args, Mark, extra_cli_args, *a):
    pattern = _make_paths_regexp(
        path.name + ("/" if path.is_dir() else "")  # nofmt
        for path in Path.cwd().iterdir()
    )
    idx = 0
    for match in re.finditer(pattern, text):
        groupdict = {}
        start = match.start()
        end = match.end()
        path = match.group(0)

        if linecol_match := re.search(r"\:(\d+)(?:\:(\d+))?", path):
            groupdict["line"] = linecol_match.group(1)
            groupdict["col"] = linecol_match.group(2)
            path = path[: linecol_match.start()]
            end = start + linecol_match.end()
        elif func_matches := list(re.finditer(r"\:\:([\w\r\n]+)", path)):
            groupdict["func"] = ".".join(
                re.sub(r"[\n\0]", "", m.group(1))  # nofmt
                for m in func_matches
            )
            path = path[: func_matches[0].start()]
            end = start + func_matches[-1].end()

        # Some paths could take up several lines or end exactly at the end of a line,
        # so that characters on the next line got included in the extracted path.
        # In both cases we restore original path.
        wrapped_path = ""
        wrapped_end = start
        while match := re.search(r"(.+?)\0*\n", path):
            wrapped_path += match.group(1)
            wrapped_end += match.end()
            if Path(wrapped_path).exists():
                path = wrapped_path
                end = wrapped_end
                break
            path = wrapped_path + path[match.end() :]
        if wrapped_path:
            path = wrapped_path
            end = wrapped_end

        idx += 1
        yield Mark(idx, start, end, path, groupdict)


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    for path, context in zip(data["match"], data["groupdicts"]):
        cmd = ["edit"]
        if line := context.get("line"):
            linecol = f"+{line}"
            if col := context.get("col"):
                linecol += f":{col}"
            cmd.append(linecol)
        # TODO: cmd.append(f"-e (find-file-function {path} {func})")
        cmd.append(path)
        text = " ".join(shlex.quote(arg) for arg in cmd)
        window = boss.window_id_map.get(target_window_id)
        window.paste_bytes(f"{text}\r")
