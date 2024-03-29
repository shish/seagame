#!/usr/bin/python

from Tkinter import *
import Image
import ImageTk
import sys
import socket
import select
import time
import math
from glob import glob

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


def read_term(data):
    if len(data) == 0:
        return None, data

    # tuple
    elif data[0] == "{":
        data = data[1:]
        tup = []
        while True:
            if data[0] == "}":
                data = data[1:]
                break
            elif data[0] in ", \n":
                data = data[1:]
            else:
                el, data = read_term(data)
                tup.append(el)
        return tuple(tup), data

    # list
    elif data[0] == "[":
        data = data[1:]
        tup = []
        while True:
            if data[0] == "]":
                data = data[1:]
                break
            elif data[0] in ", \n":
                data = data[1:]
            else:
                el, data = read_term(data)
                tup.append(el)
        return tup, data

    # string
    elif data[0] == '"':
        data = data[1:]
        string = ""
        while True:
            if data[0] == '"':
                data = data[1:]
                break
            else:
                string = string + data[0]
                data = data[1:]
        return string, data

    # atom
    elif data[0] in 'abcdefghijklmnopqrstuvwxyz':
        string = ""
        while True:
            if len(data) == 0 or data[0] not in 'abcdefghijklmnopqrstuvwxyz_0123456789':
                break
            else:
                string = string + data[0]
                data = data[1:]
        return string, data

    # number
    elif data[0] in '0123456789':
        string = ""
        num_type = int
        while True:
            if len(data) == 0 or data[0] not in '0123456789.':
                break
            else:
                if data[0] == ".":
                    num_type = float
                string = string + data[0]
                data = data[1:]
        return num_type(string), data

    else:
        data = data[1:]
        return read_term(data)


class TupleDict:
    def __init__(self, stats):
        for tup in stats:
            if len(tup) == 2:
                setattr(self, tup[0], tup[1])
            else:
                setattr(self, tup[0], tup[1:])

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

    def get_photo_image(self, base, rotation=0):
        round_rotation = int(rotation % 360) / 18 * 360
        key = base+"-"+str(rotation)
        if base not in self.images:
            self.images[base] = Image.open(glob("images/%s.*" % base)[0])
        if key not in self.photo_images:
            self.photo_images[key] = ImageTk.PhotoImage(self.images[base].rotate(rotation))
        return self.photo_images[key]

    def __init__(self, master):
        self.master = master
        self.menu = self.__menu(master)
        self.last_ping = time.time()
        self.notifications = []
        self.mode = MODE_UNKNOWN
        self.server_time = (0, 0, 0)
        self.ship_statuses = {}
        self.zone_status = None
        self.images = {}
        self.photo_images = {}
        self.network_input_buffer = ""

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
            self.render_sea_base(view_w, view_h)
            self.render_sea_ships(view_w, view_h)
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

    def render_sea_base(self, view_w, view_h):
        img = self.get_photo_image("sea")
        img_w = img.width()
        img_h = img.height()
        for x in range(0, view_w+img_w, img_w):
            for y in range(0, view_h+img_h, img_h):
                self.canvas.create_image(x, y, anchor=NW, image=img)

        text = "Sea Mode"
        if self.zone_status:
            text = text + ": " + str(self.zone_status.name)
        self.canvas.create_text(
            3, 3,
            text=text, tags="notification", anchor=NW, width=300,
            font="TkFixedFont",
            state="disabled",
        )

    def render_sea_ships(self, view_w, view_h):
        x0 = view_w/2
        y0 = view_h/2
        for shipname in self.ship_statuses:
            ship = self.ship_statuses[shipname]
            self.canvas.create_image(
                x0+ship.location[0], y0+ship.location[1],
                image=self.get_photo_image("cattrel", -ship.direction/3.1415*360)
            )
            # TODO: separate thread for client-side prediction?
            ship.location = (
                ship.location[0] + math.cos(ship.direction) * ship.velocity * 0.2,
                ship.location[1] + math.sin(ship.direction) * ship.velocity * 0.2
            )
            ship.velocity = ship.velocity + ship.acceleration * 0.2
            ship.direction = ship.direction + ship.turn * 0.2

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
        self.input.delete(0, END)

    def handle_network_input(self, sd, mask):
        self.network_input_buffer = self.network_input_buffer + self.socket.recv(4096)

        while True:
            term, self.network_input_buffer = read_term(self.network_input_buffer)

            if not term:
                break

            if term[0] == "notification":
                self.notifications.insert(0, (time.time(), str(term[1])))
            elif term[0] == "pong":
                self.show_text("PING-PONG: "+str(time.time() - self.last_ping))
            elif term[0] == "board_ship":
                self.mode = MODE_SEA
            elif term[0] == "leave_ship":
                self.mode = MODE_DOCK
            elif term[0] == "time":
                self.server_time = term[1]
            elif term[0] == "ship_status":
                ship_status = ShipStatus(term[1])
                self.ship_statuses[ship_status.name] = ship_status
            elif term[0] == "zone":
                self.zone_status = ZoneStatus(term[1])
            else:
                self.show_text("unknown message: "+str(term))

    def send(self, d):
        self.socket.send(d+"\n")

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


def test():
    print read_term("")
    print read_term("{}")
    print read_term("[]")
    print read_term("42")
    print read_term("42.5")
    print read_term('"hello"')
    print read_term('{"hello", "there", "bob"}')
    print read_term('[123, 45.6]')
    print read_term('{"zone_status", [{weather, "calm"}, {police, "active"}]}')
    print read_term('{"it", "is"} ["a", "nice"] "day"')


if __name__ == "__main__":
    sys.exit(main(sys.argv))
