#!/usr/bin/python

import socket
import readline
import sys
import thread
from select import select


init = [
#    "login bob test",
#    "buyShip"
]

PROMPT="seagame> "

def reader(socket):
    while True:
        data = socket.recv(4096)
        if data:
            sys.stdout.write('\r'+' '*(len(PROMPT)+len(readline.get_line_buffer()))+'\r')
            print data,
            sys.stdout.write(PROMPT + readline.get_line_buffer())
            sys.stdout.flush()

try:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(("127.0.0.1", 1234))
    thread.start_new_thread(reader, (s, ))
    while True:
        if init:
            parts = init.pop(0)
        else:
            parts = raw_input("seagame> ")
        if len(parts) > 0:
            s.send(parts+"\n")
except IOError:
    print "Disconnected"

