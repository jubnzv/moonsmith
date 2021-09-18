#!/usr/bin/python3
#
# This script runs moonsmith to generate a random program, then runs a Lua
# interpreter to execute it. If moonsmith hangs, or a Lua interpreter throws an
# exception or can't evaluate result in 1 second, this test will be marked as
# failed.
#

import argparse
import logging
import os
import signal
import subprocess
import sys

from typing import Optional


logging.basicConfig(
    format='[%(asctime)s] %(levelname)-8s %(message)s',
    level=logging.INFO,
    datefmt='%Y-%m-%d %H:%M:%S')


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


def run(binary: str, tests_num: Optional[int], timeout: int):
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
        if i == tests_num:
            break
        i = i + 1
        try:
            out = subprocess.check_output([binary, "-S", "1"],
                                          stderr=subprocess.STDOUT,
                                          timeout=timeout)
        except subprocess.CalledProcessError as e:
            print(f"{i}: moonsmith's exception: {e}")
            continue
        try:
            out = subprocess.check_output(["lua", "out.lua"],
                                          stderr=subprocess.STDOUT,
                                          timeout=timeout)
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
    parser = argparse.ArgumentParser()
    parser.add_argument('-b', '--binary',
            default='_build/default/src/moonsmith.exe',
            help='Path to moonsmith binary')
    parser.add_argument('-t', '--timeout',
            default=1,
            help='Timeout to execute program (sec.)')
    parser.add_argument('-n', '--tests-num',
            default=None,
            help='Number of tests to run.')
    args = parser.parse_args()
    signal.signal(signal.SIGINT, signal_handler)
    run(args.binary, args.tests_num, args.timeout)
