#!/usr/bin/python

from Tkinter import *
import sys
import socket
import select
import time
import math
from py_interface import erl_term

VERSION="0.0.0"

MODE_UNKNOWN=1
MODE_DOCK=2
MODE_SEA=3

AUTORUN=["login bob test"]


def typify(data):
    try:
        return float(data)
    except:
        return data


class TupleDict:
    def __init__(self, stats):
        for tup in stats:
            if len(tup) == 2:
                setattr(self, tup[0].atomText, tup[1])
            else:
                setattr(self, tup[0].atomText, tup[1:])

class ZoneStatus(TupleDict):
    pass

class ShipStatus(TupleDict):
    pass


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
        self.notifications = []
        self.mode = MODE_UNKNOWN
        self.server_time = (0, 0, 0)
        self.ship_statuses = {}
        self.zone_status = None

        self.canvas = Canvas(
            master,
            background="cyan"
        )

        self.h = Scrollbar(master, orient=HORIZONTAL)
        self.v = Scrollbar(master, orient=VERTICAL)
        self.output = Text(
            master,
            width=80, height=5,
            xscrollcommand=self.h.set,
            yscrollcommand=self.v.set
        )
        self.h['command'] = self.output.xview
        self.v['command'] = self.output.yview

        self.input = Entry(master)
        self.input.bind("<KeyRelease-Return>", self.handle_user_input)

        master.grid_columnconfigure(0, weight=1)
        master.grid_rowconfigure(1, weight=1)
        self.canvas.grid(  column=0, row=1, sticky=(N, W, E, S))
        self.output.grid(  column=0, row=2, sticky=(W, E))
        self.v.grid(       column=1, row=2, sticky=(N, S))
        self.h.grid(       column=0, row=3, sticky=(W, E))
        self.input.grid(   column=0, row=4, sticky=(W, E))

        self.input.focus_set()

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect(("127.0.0.1", 1234))

        self.master.createfilehandler(self.socket, tkinter.READABLE, self.handle_network_input)
        for cmd in AUTORUN:
            self.send(cmd)
        self.render_loop()

    def render_loop(self):
        view_w = self.canvas.winfo_width()
        view_h = self.canvas.winfo_height()

        self.canvas.delete(ALL)
        if self.mode == MODE_DOCK:
            self.canvas.create_text(
                3, 3,
                text="Dock Mode", tags="notification", anchor=NW, width=300,
                font="TkFixedFont",
                state="disabled",
            )
        elif self.mode == MODE_SEA:
            self.render_sea(view_w, view_h)
            self.render_sea_controls(view_w, view_h)
        else:
             self.canvas.create_text(
                3, 3,
                text="Unknown Mode", tags="notification", anchor=NW, width=300,
                font="TkFixedFont",
                state="disabled",
            )
        self.render_common(view_w, view_h)
        self.master.after(200, self.render_loop)

    def render_sea(self, view_w, view_h):
        x0 = view_w/2
        y0 = view_h/2
        text = "Sea Mode"
        if self.zone_status:
            text = text + ": " + str(self.zone_status.name)
        self.canvas.create_text(
            3, 3,
            text=text, tags="notification", anchor=NW, width=300,
            font="TkFixedFont",
            state="disabled",
        )
        for shipname in self.ship_statuses:
            ship = self.ship_statuses[shipname]
            self.canvas.create_rectangle(
                x0+ship.location[0],    y0+ship.location[1],
                x0+ship.location[0]+10, y0+ship.location[1]+10,
                fill="#afa", outline="#000", tags="ship",
                state="disabled",
            )
            ship.location = (
                ship.location[0] + math.cos(ship.direction) * ship.velocity * 0.2,
                ship.location[1] + math.sin(ship.direction) * ship.velocity * 0.2
            )
            ship.velocity = ship.velocity + ship.acceleration
            ship.direction = ship.direction + ship.turn

    def render_sea_controls(self, view_w, view_h):
        def button(x, y, cmd):
            btn = self.canvas.create_rectangle(
                x, y,
                x+10, y+10,
                fill="#afa", outline="#000", tags="controls",
            )
            def accel(e):
                self.send(cmd)
            self.canvas.tag_bind(btn, "<1>", accel)

        x0 = view_w - 60
        y0 = view_h - 80
        button(x0+20, y0,    "setAcceleration  1")
        button(x0+20, y0+20, "setAcceleration  0")
        button(x0+20, y0+60, "setAcceleration -1")
        button(x0,    y0+40, "setTurn -1")
        button(x0+20, y0+40, "setTurn  0")
        button(x0+40, y0+40, "setTurn  1")

    def render_common(self, view_w, view_h):
        w = 300
        h = 20
        x = (view_w-w)/2
        y = 50
        now = time.time()
        self.notifications = filter(lambda n: n[0]>now-5, self.notifications)
        for n, (ntime, text) in enumerate(self.notifications):
            self.canvas.create_rectangle(
                x, y-n*h*1.5,
                x+w, y+h-n*h*1.5,
                fill="#ddd", outline="#000", tags="notification",
                state="disabled",
            )
            self.canvas.create_text(
                x+w/2, y+h/2-n*h*1.5,
                text=text, tags="notification", width=300,
                font="TkFixedFont",
                state="disabled",
            )

        self.canvas.create_text(
            view_w-3, 3,
            text="%02d:%02d" % (self.server_time[0], self.server_time[1]),
            tags="notification", anchor=NE,
            font="TkFixedFont",
            state="disabled",
        )

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
            self.notifications.insert(0, (time.time(), str(term[1])))
        elif msg_type == "pong":
            self.show_text("PING-PONG: "+str(time.time() - self.last_ping))
        elif msg_type == "board_ship":
            self.mode = MODE_SEA
        elif msg_type == "leave_ship":
            self.mode = MODE_DOCK
        elif msg_type == "time":
            self.server_time = term[1]
        elif msg_type == "ship_status":
            ship_status = ShipStatus(term[1])
            self.ship_statuses[ship_status.name] = ship_status
        elif msg_type == "zone":
            self.zone_status = ZoneStatus(term[1])
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
