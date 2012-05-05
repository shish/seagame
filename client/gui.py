#!/usr/bin/python

from Tkinter import *
import sys
import socket
import select
import time
from py_interface import erl_term

VERSION="0.0.0"

def typify(data):
    try:
        return float(data)
    except:
        return data

class _App:
    def __menu(self, master):
        menubar = Menu(master)

        def file_menu():
            filemenu = Menu(menubar, tearoff=0)
            filemenu.add_command(label="Ping", command=self.send_ping)
            #filemenu.add_command(label="Append ctxt", command=self.append_file)
            filemenu.add_separator()
            #filemenu.add_command(label="Exit", command=self.save_settings_and_quit)
            return filemenu
        menubar.add_cascade(label="File", menu=file_menu())

        def help_menu():
            def show_about():
                t = Toplevel(master)
                t.title("About")
                t.transient(master)
                t.resizable(False, False)
                #Label(t, image=self.img_logo).grid(column=0, row=0, sticky=(E, W))
                Label(t, text="Seagame Client %s" % VERSION, anchor=CENTER).grid(column=0, row=1, sticky=(E, W))
                Label(t, text="(c) 2012 Shish", anchor=CENTER).grid(column=0, row=2, sticky=(E, W))
                Button(t, text="Close", command=t.destroy).grid(column=0, row=3, sticky=(E, ))
                #win_center(t)

            helpmenu = Menu(menubar, tearoff=0)
            helpmenu.add_command(label="About", command=show_about)
            return helpmenu
        menubar.add_cascade(label="Help", menu=help_menu())

        master.config(menu=menubar)

    def __init__(self, master):
        self.master = master
        self.menu = self.__menu(master)
        self.last_ping = time.time()

        self.h = Scrollbar(master, orient=HORIZONTAL)
        self.v = Scrollbar(master, orient=VERTICAL)
        self.output = Text(
            master,
            width=80, height=25,
            xscrollcommand=self.h.set,
            yscrollcommand=self.v.set
        )
        self.h['command'] = self.output.xview
        self.v['command'] = self.output.yview

        self.input = Entry(master)
        self.input.bind("<KeyRelease-Return>", self.handle_user_input)

        master.grid_columnconfigure(0, weight=1)
        master.grid_rowconfigure(1, weight=1)
        self.output.grid(  column=0, row=1, sticky=(N, W, E, S))
        self.v.grid(       column=1, row=1, sticky=(N, S))
        self.h.grid(       column=0, row=2, sticky=(W, E))
        self.input.grid(   column=0, row=3, sticky=(W, E))

        self.input.focus_set()

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect(("127.0.0.1", 1234))

        self.master.createfilehandler(self.socket, tkinter.READABLE, self.handle_network_input)

    def handle_user_input(self, e):
        d = self.input.get()
        self.show_text(">>> "+d)
        self.send(d)

    def handle_network_input(self, sd, mask):
        d = self.socket.recv(4096)
        term = erl_term.BinaryToTerm(d)

        if type(term[0]) == str:
            msg_type = term[0]
        else:
            msg_type = term[0].atomText

        if msg_type == "notification":
            self.show_text("NOTIFICATION: "+str(term[1]))
        elif msg_type == "pong":
            self.show_text("PING-PONG: "+str(time.time() - self.last_ping))
        else:
            self.show_text("unknown message: "+str(term))

    def send(self, d):
        parts = d.split()
        self.input.delete(0, END)
        if parts:
            parts = [typify(p) for p in parts]
            self.socket.send(erl_term.TermToBinary(tuple(parts)))

    def show_text(self, text):
        self.output.insert(END, str(text)+"\n")
        self.output.yview_moveto(1)

    def send_ping(self):
        self.last_ping = time.time()
        self.send("ping")


def main(argv):
    root = Tk()
    root.title("Seagame GUI")
    _App(root)
    root.mainloop()

if __name__ == "__main__":
    sys.exit(main(sys.argv))
