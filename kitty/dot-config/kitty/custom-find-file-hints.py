from pathlib import Path
import re
import shlex


LINE_REGEXP = r"\:(\d+)"
FUNC_REGEXP = r"\:\:([^\s\:]+)"


def make_paths_regexp(paths):
    query = "|".join(re.escape(path) for path in paths)
    return fr"(?:{query})[\S\r\n]*"


def re_extract(pattern, string):
    return re.sub(pattern, "", string), re.findall(pattern, string)


def clean_path(string):
    return string.strip(":")


def mark(text, args, Mark, extra_cli_args, *a):
    pattern = make_paths_regexp(path.name for path in Path.cwd().iterdir())
    for idx, match in enumerate(re.finditer(pattern, text)):
        start, end = match.span()
        mark_text = re.sub(r"[\r\n\0]", "", text[start:end])
        yield Mark(idx, start, end, mark_text, {})


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    for path in data["match"]:
        path = clean_path(path)
        path, line_col = re_extract(LINE_REGEXP, path)
        path, func = re_extract(FUNC_REGEXP, path)
        cmd = ["emacsclient", "-n"]
        if line_col:
            cmd.append("+" + ":".join(line_col[:2]))
        elif func:
            pass  # TODO: cmd.append(f"-e (find-file-function {path} {func})")
        cmd.append(path)
        text = " ".join(shlex.quote(arg) for arg in cmd)
        window = boss.window_id_map.get(target_window_id)
        window.paste_bytes(f"{text}\r")
