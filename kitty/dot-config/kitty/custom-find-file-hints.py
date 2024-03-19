from pathlib import Path
import re
import shlex


def _make_paths_regexp(paths):
    query = "|".join(re.escape(path) for path in paths)
    return fr"(?:{query})[\S\r\n]*"


def mark(text, args, Mark, extra_cli_args, *a):
    pattern = _make_paths_regexp(path.name for path in Path.cwd().iterdir())
    idx = 0
    for match in re.finditer(pattern, text):
        groupdict = {}
        path = re.sub(r"[\r\n\0]", "", match.group(0))
        if linecol_match := re.search(r"\:(\d+)(?:\:(\d+))?$", path):
            groupdict["line"] = linecol_match.group(1)
            groupdict["col"] = linecol_match.group(2)
            path = path[:linecol_match.start()]
        if func_matches := list(re.finditer(r"\:\:(\w+)", path)):
            groupdict["func"] = ".".join(m.group(1) for m in func_matches)
            path = path[:func_matches[0].start()]
        if Path(path).is_file():
            idx += 1
            yield Mark(idx, match.start(), match.end(), path, groupdict)


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
