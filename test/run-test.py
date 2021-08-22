#!/usr/bin/python3

import logging
import os
import signal
import subprocess
import sys


logging.basicConfig(
    format='[%(asctime)s] %(levelname)-8s %(message)s',
    level=logging.INFO,
    datefmt='%Y-%m-%d %H:%M:%S')


g_exe = "_build/default/src/moonsmith.exe"
g_timeout_s = 1


def signal_handler(sig, frame):
    sys.exit(0)


def append_file(filename, line):
    with open(filename, 'a') as f:
        f.write(line)


def get_comment_header(out):
    comment_lines = ["\n"]
    for line in out.split("\n"):
        comment_lines.append("-- " + line)
    comment_lines = comment_lines[:len(comment_lines)-1]
    return "\n".join(comment_lines)


def run():
    i = 0
    try:
        subprocess.check_output(["mkdir", "-p", "test/out"])
    except subprocess.CalledProcessError as e:
        out = e.output.decode("utf-8")
        print(f"Can't create test/out: {out}")
        return -1

    logging.info(f"Starting test")

    while True:
        if i % 100 == 0 and i != 0:
            logging.info(f"Passed {i} tests")
        i = i + 1
        try:
            out = subprocess.check_output([g_exe, "-S", "1"],
                                          stderr=subprocess.STDOUT,
                                          timeout=g_timeout_s)
        except subprocess.CalledProcessError as e:
            print(f"{i}: moonsmith's exception: {e}")
            continue
        try:
            out = subprocess.check_output(["lua", "out.lua"],
                                          stderr=subprocess.STDOUT,
                                          timeout=g_timeout_s)
        except subprocess.CalledProcessError as e:
            out = e.output.decode("utf-8")
            print(f"{i}: Exception while executing Lua: {out}")
            # Write to the end to see correct number of line for an error.
            append_file("out.lua", get_comment_header(out))
            subprocess.run(["cp", "out.lua", f"test/out/{i}.lua"])
        except subprocess.TimeoutExpired:
            print(f"{i}: Timeout while executing Lua")
            subprocess.run(["cp", "out.lua", f"test/out/{i}.lua"])


if __name__ == '__main__':
    signal.signal(signal.SIGINT, signal_handler)
    run()
