import os
import sublime
import sublime_plugin

from Default.exec import AsyncProcess, ProcessListener


class ShellCommandInputHandler(sublime_plugin.TextInputHandler):
    def placeholder(self):
        return "Shell Command"

    def validate(self, text: str):
        return len(text.strip()) > 0


class ShellExecCommand(sublime_plugin.WindowCommand, ProcessListener):
    OUTPUT_LIMIT = 2 ** 27
    OUTPUT_VIEW_NAME = "shell_exec"
    OUTPUT_VIEW_SYNTAX = "Packages/Text/Plain text.tmLanguage"

    def __init__(self, window):
        super().__init__(window)
        self.output_view = None
        self.proc = None
        self.encoding = "utf-8"

    def run(self, shell_command: str):
        self.output_view = self.window.create_output_panel(
            self.OUTPUT_VIEW_NAME, unlisted=True
        )
        self.output_view.settings().set("line_numbers", False)
        self.output_view.settings().set("gutter", False)
        self.output_view.settings().set("scroll_past_end", False)
        self.output_view.assign_syntax(self.OUTPUT_VIEW_SYNTAX)
        self.output_view.set_read_only(True)

        working_dir = ""
        if self.window.active_view() and self.window.active_view().file_name():
            working_dir = os.path.dirname(self.window.active_view().file_name())
        # Change to the working dir, rather than spawning the process with it,
        # so that emitted working dir relative path names make sense
        if working_dir != "":
            os.chdir(working_dir)

        self.proc = None
        self.output_size = 0

        self.window.run_command("show_panel", {"panel": f"output.{self.OUTPUT_VIEW_NAME}"})

        try:
            self.proc = AsyncProcess(
                cmd=None, shell_cmd=shell_command, env={}, listener=self
            )
            self.proc.start()
        except Exception as exc:
            self.write(str(exc) + "\n")
            self.write("[Finished]")

    def input(self, args: dict):
        return ShellCommandInputHandler()

    def write(self, characters):
        self.output_view.run_command(
            'append',
            {'characters': characters, 'force': True, 'scroll_to_end': True},
        )

    def on_data(self, proc, data):
        if proc != self.proc:
            return

        if self.output_size >= self.OUTPUT_LIMIT:
            return

        self.write(data)
        self.output_size += len(data)

        if self.output_size >= self.OUTPUT_LIMIT:
            self.write('\n[Output Truncated]\n')

    def on_finished(self, proc):
        if proc != self.proc:
            return

        if proc.killed:
            self.write("\n[Cancelled]")
        else:
            self.write("[Finished]")
